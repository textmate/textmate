# -*- encoding: utf-8 -*-
version_file = File.expand_path('../lib/taskpaper_utils/version.rb', __FILE__)

Gem::Specification.new do |gem|
  gem.name          = "taskpaper_utils"
  gem.version       = File.read(version_file).match(/VERSION ?= ?['"](.+)['"]/)[1]
  gem.authors       = ["lasitha ranatunga"]
  gem.email         = ["exbinary@gmail.com"]
  gem.summary       = %q{Parse and work with TaskPaper formatted documents.}
  gem.license       = 'LGPL-3.0'
  gem.homepage      = "https://github.com/exbinary/taskpaper_utils"
  gem.description   = %{
    Simple library for working with TaskPaper formatted documents.
    Parse a TaskPaper document, work with the resulting set of
    model objects and reserialize.
  }

  # todo: depending on git here may not be portable
  gem.files         = `git ls-files`.split($/)
  gem.test_files    = gem.files.grep(%r{^spec/})
  gem.require_paths = ["lib"]
end
