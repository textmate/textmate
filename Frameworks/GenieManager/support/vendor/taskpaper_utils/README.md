TaskpaperUtils [![Build Status](https://travis-ci.org/exbinary/taskpaper_utils.png?branch=master)](https://travis-ci.org/exbinary/taskpaper_utils) [![Code Climate](https://codeclimate.com/github/exbinary/taskpaper_utils.png)](https://codeclimate.com/github/exbinary/taskpaper_utils) [![Coverage Status](https://coveralls.io/repos/exbinary/taskpaper_utils/badge.png)](https://coveralls.io/r/exbinary/taskpaper_utils) [![LGPLv3](http://upload.wikimedia.org/wikipedia/commons/a/a1/LGPL-V3.png)](http://www.gnu.org/licenses/lgpl.html)
==============

Simple ruby library for parsing and working with [TaskPaper] formatted documents.

The [TaskPaper] format is defined by [Hog Bay Software] for use in their excellent [TaskPaper] OS X app.  It is designed to be human readable and universally portable as plain text.

This library:

- parses a [TaskPaper] document,
- provides an object graph of projects, tasks and notes contained therein,
- allows changes to be saved back to a [TaskPaper] file,
- does _not_ interact with the [TaskPaper] app itself.

## Installation
I haven't published this as a gem yet, so...

Either use `bundler`:

    # add this to your Gemfile
    gem 'taskpaper_utils', git: 'git://github.com/exbinary/taskpaper_utils'
    # then:
    bundle install

Or build the gem yourself:

    git clone https://github.com/exbinary/taskpaper_utils
    gem build taskpaper_utils/taskpaper_utils.gemspec
    gem install taskpaper_utils-<version>.gem

## Usage

    require 'taskpaper_tools'

    # parse taskpaper document into object graph
    document = TaskpaperUtils.parse_file('path/to/file.taskpaper')

    # Document contains projects, tasks and notes 
    project = document.projects.first

    # find all entries in the project with an @done tag
    project.tagged(:done)

    # find all entries with a specific priority
    project.tagged(:priority, '1')

    # reference child entries using their text
    task = project['add search feature']
    puts task.text        # => "add search feature"
    puts task.raw_text    # => "\t- add search feature"

    # access nested child entries
    puts task.notes.map(&:text)
    puts task.subtasks.map(&:text)
    
    # reserialize the object graph back to file
    TaskpaperUtils.save(document, 'path/to/new/file.taskpaper') 

_More usage examples to come..._

## Status & Roadmap
Currently, this is mostly a spike (pre-alpha).
The public interface hasn't settled down yet.

A loose roadmap:

- Evolve API for adding, removing and modifying entries
- Expand search and filtering API
- ... ?

## Contributing
At this early stage i'm mostly in need of use-cases to guide API and design priorities.  I use this library in a couple of scripts but those workflows are pretty personalized so i'd appreciate input on what other uses you might envision for taskpaper_utils.

Of course i always value [questions, feedback, code review](https://github.com/exbinary), and [bug reports](https://github.com/exbinary/taskpaper_utils/issues).

## Resources

* A detailed description (and example) of the [TaskPaper] syntax can be found in [exemplar.taskpaper](spec/integration/exemplar.taskpaper)
* The TaskPaper [wiki](http://www.hogbaysoftware.com/wiki/TaskPaper), including a list of related tools.
* An alternative ruby library named [taskpaper-tools](https://github.com/thiagoa/taskpaper-tools), also in early development as of this writing.

## License and Copyright
Copyright 2013 lasitha ranatunga.

This program (the 'taskpaper_utils' library, including all source files therein) is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but _without any warranty_; without even the implied warranty of _merchantability_ or _fitness for a particular purpose_.  See the Lesser GNU General Public License for more details.

The Lesser GNU General Public License and the GNU General Public License can be found alongside this file as [LICENSE.lesser](LICENSE.lesser) and [LICENSE](LICENSE). They are also available [online](http://www.gnu.org/licenses/lgpl.html).

The [TaskPaper] trademark, OS X app and document format are the property of [Hog Bay Software], protected by copyright.  The author of `taskpaper_utils` is not affiliated with [Hog Bay Software].

---

[Hog Bay Software]: http://www.hogbaysoftware.com/
[TaskPaper]: http://www.hogbaysoftware.com/products/taskpaper

