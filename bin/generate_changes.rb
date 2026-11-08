#!/usr/bin/env ruby
# == Synopsis
#
# Generate Changes.html from git commit history.
# Iterates through all tags and lists commits between each tag pair.
#
# Usage: generate_changes.rb [options]
#
# Options:
#   -o, --output FILE      : Output file (default: Applications/TextMate/about/Changes.html)
#   -t, --tags LIST        : Comma-separated list of tags (default: all tags)
#   -n, --next-tag TAG     : Add TAG as the top release for changes from latest tag to HEAD
#   -u, --github URL       : GitHub repository URL for compare links
#   -h, --help             : Show this message.

require 'optparse'
require 'cgi'
require 'fileutils'
require 'open3'

output_file = 'Applications/TextMate/about/Changes.html'
github_url = 'https://github.com/tectiv3/textmate'
tags = []
next_tag = nil

OptionParser.new do |opts|
  opts.banner = "Usage: #{File.basename(__FILE__)} [options]"
  opts.on_tail('-h', '--help', 'Show this message.') do
    puts opts
    exit
  end

  opts.on("-o", "--output FILE", "Output file (default: Applications/TextMate/about/Changes.html)") do |file|
    output_file = file
  end

  opts.on("-t", "--tags LIST", "Comma-separated list of tags") do |list|
    tags = list.split(',').map { |t| t.strip }.reject { |t| t.empty? }
  end

  opts.on("-n", "--next-tag TAG", "Add TAG as the top release for changes from latest tag to HEAD") do |tag|
    next_tag = tag
  end

  opts.on("-u", "--github URL", "GitHub repository URL for compare links") do |url|
    github_url = url
  end
end.parse!

def git(*args)
  # All output parsed by this script flows through here, so pin the encoding in
  # one place: the pipe is tagged with Encoding.default_external (US-ASCII under
  # the C locale), and String#split raises on any non-ASCII byte in a commit
  # subject unless the string is UTF-8.
  stdout, stderr, status = Open3.capture3('git', '-c', 'i18n.logOutputEncoding=UTF-8', *args)
  unless status.success?
    warn "git #{args.join(' ')} failed"
    warn stderr unless stderr.empty?
    exit status.exitstatus || 1
  end
  stdout.force_encoding(Encoding::UTF_8).scrub
end

def tag_date(tag)
  git('for-each-ref', "--format=%(creatordate:short)", "refs/tags/#{tag}").strip
end

def commits_between(from_tag, to_tag)
  git('log', '--pretty=format:%h%x00%ad%x00%s', '--date=short', '--reverse', "#{from_tag}..#{to_tag}", '--')
    .lines
    .map do |line|
      hash, date, subject = line.chomp.split("\0", 3)
      { hash: hash, date: date, subject: subject }
    end
end

def compare_url(github_url, from_tag, to_tag)
  from = CGI.escape(from_tag)
  to = CGI.escape(to_tag)
  "#{github_url.chomp('/')}/compare/#{from}...#{to}"
end

def html_id(date, tag)
  CGI.escapeHTML("#{date}#{tag}")
end

def date_for_ref(ref)
  git('log', '-1', '--format=%cd', '--date=short', ref, '--').strip
end

if tags.empty?
  tags = git('tag', '-l', '--sort=-v:refname').lines.map { |line| line.strip }.reject { |line| line.empty? }
else
  known_tags = git('tag', '-l', '--sort=-v:refname').lines.map { |line| line.strip }
  missing_tags = tags - known_tags
  unless missing_tags.empty?
    warn "Unknown tag#{missing_tags.length == 1 ? '' : 's'}: #{missing_tags.join(', ')}"
    exit 1
  end
  tags = known_tags.select { |tag| tags.include?(tag) }
end

tags = tags.uniq

if next_tag
  if next_tag.empty?
    warn 'Next tag cannot be empty.'
    exit 1
  elsif tags.include?(next_tag)
    warn "Tag already exists: #{next_tag}"
    exit 1
  end

  tags.unshift(next_tag)
end

if tags.length < 2
  warn 'At least two tags are required to generate changes between releases.'
  exit 1
end

puts "Found #{tags.length} tags: #{tags.join(', ')}"
puts "Output: #{output_file}"

dir = File.dirname(output_file)
unless File.exist?(dir)
  FileUtils.mkdir_p(dir)
end

html = <<~HTML
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
    "http://www.w3.org/TR/html4/strict.dtd">

<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <link href="css/stylesheet.css" rel="stylesheet" type="text/css" />
    <title>Release Notes</title>
</head>

<body>
<h1 id="changes">Changes</h1>

HTML

tags.each_cons(2).with_index do |(tag, previous_tag), index|
  to_ref = tag == next_tag ? 'HEAD' : tag
  commits = commits_between(previous_tag, to_ref)

  if commits.empty?
    puts "No commits found between #{previous_tag} and #{tag}"
    next
  end

  date = tag == next_tag ? date_for_ref('HEAD') : tag_date(tag)
  article_class = index.zero? ? ' class="latest"' : ''

  html += "<article#{article_class}>\n"
  html += "<h2 id=\"#{html_id(date, tag)}\">#{CGI.escapeHTML(date)} (#{CGI.escapeHTML(tag)})</h2>\n\n"
  html += "<ul>\n"

  commits.each do |commit|
    subject = CGI.escapeHTML(commit[:subject])
    html += "<li>#{subject}</li>\n"
  end

  href = CGI.escapeHTML(compare_url(github_url, previous_tag, tag))
  html += "<li>See <a href=\"#{href}\">all changes since #{CGI.escapeHTML(previous_tag)}</a></li>\n"
  html += "</ul>\n"
  html += "</article>\n\n"
end

html += "</body>\n</html>\n"

File.write(output_file, html)
puts "Generated #{output_file} (#{File.size(output_file)} bytes)"
