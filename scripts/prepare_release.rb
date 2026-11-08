#!/usr/bin/env ruby

require 'open3'
require 'English'

mode = ARGV.fetch(0, 'patch')
unless %w[patch minor].include?(mode)
  warn "Usage: #{File.basename(__FILE__)} [patch|minor]"
  exit 1
end

def git(*args)
  stdout, stderr, status = Open3.capture3('git', *args)
  unless status.success?
    warn "git #{args.join(' ')} failed"
    warn stderr unless stderr.empty?
    exit status.exitstatus || 1
  end
  stdout
end

def run(*args)
  system(*args) || exit($CHILD_STATUS.exitstatus || 1)
end

status = git('status', '--porcelain')
unless status.empty?
  warn 'Release preparation requires a clean worktree.'
  warn status
  exit 1
end

upstream = git('rev-parse', '--abbrev-ref', '--symbolic-full-name', '@{u}').strip
unpushed_commits = git('rev-list', '--count', "#{upstream}..HEAD").to_i
latest_tag = git('tag', '-l', 'v*', '--sort=-v:refname').lines.first&.strip
unless latest_tag&.match?(/\Av\d+\.\d+(?:\.\d+)?\z/)
  warn "Could not find a version tag to bump. Latest tag: #{latest_tag || '(none)'}"
  exit 1
end

major, minor, patch = latest_tag.delete_prefix('v').split('.').map(&:to_i)
patch ||= 0

case mode
when 'minor'
  minor += 1
  patch = 0
when 'patch'
  patch += 1
end

next_tag = "v#{major}.#{minor}.#{patch}"
unless git('tag', '-l', next_tag).strip.empty?
  warn "Tag already exists: #{next_tag}"
  exit 1
end

run('ruby', 'bin/generate_changes.rb', '--next-tag', next_tag)
run('git', 'add', 'Applications/TextMate/about/Changes.html')

_stdout, _stderr, diff_status = Open3.capture3('git', 'diff', '--cached', '--quiet')
if diff_status.success?
  warn 'Generated changelog did not change.'
  exit 1
end

if unpushed_commits.zero?
  run('git', 'commit', '-m', "Add changelog for #{next_tag}")
else
  run('git', 'commit', '--amend', '--no-edit')
end

run('ruby', 'bin/generate_contributions.rb', '--revision', 'HEAD^')
run('git', 'add', 'Applications/TextMate/about/Contributions.html')

_stdout, _stderr, diff_status = Open3.capture3('git', 'diff', '--cached', '--quiet')
run('git', 'commit', '--amend', '--no-edit') unless diff_status.success?

run('git', 'tag', '-a', next_tag, '-m', "TextMate #{next_tag.delete_prefix('v')}")

puts "Prepared release #{next_tag}"
