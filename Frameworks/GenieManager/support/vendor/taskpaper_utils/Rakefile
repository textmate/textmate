require "bundler/gem_tasks"

# Added by devtools
require 'devtools'
Devtools.init_rake_tasks

desc 'Generate yard docs and verify coverage'
task 'metrics:documentation' do
  stats = YARD::CLI::Stats.new
  stats.run
  Devtools.notify 'Documentation incomplete!' if stats.instance_variable_get(:@undocumented) > 0
end

task 'ci:metrics' => 'metrics:documentation'
