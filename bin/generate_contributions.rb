#!/usr/bin/env ruby

require 'cgi'
require 'digest/md5'
require 'fileutils'
require 'open3'
require 'optparse'
require 'time'

output_file = 'Applications/TextMate/about/Contributions.html'
github_url = 'https://github.com/tectiv3/textmate'
revision = 'HEAD'

OptionParser.new do |opts|
  opts.banner = "Usage: #{File.basename(__FILE__)} [options]"
  opts.on_tail('-h', '--help', 'Show this message.') { puts opts; exit }

  opts.on('-o', '--output FILE', 'Output file (default: Applications/TextMate/about/Contributions.html)') do |file|
    output_file = file
  end

  opts.on('-u', '--github URL', 'GitHub repository URL for commit links') do |url|
    github_url = url
  end

  opts.on('-r', '--revision REV', 'Revision to generate from (default: HEAD)') do |rev|
    revision = rev
  end
end.parse!

KNOWN_GITHUB_USERS = {
  'e904dfc2f19fa297256c24c2a620c629' => 'tectiv3',
  '1178ce2f664a6cee9a05a3e11af5d8d2' => 'aaronbrethorst',
  '3b0ef5e2a5f1aa3ccf3f23a20adf8873' => 'Hoverbear',
  'ff3502050b3b1b00cb6c810d5c41ffc9' => 'bradchoate',
  'ee646002e51a3c83e01db85ae42187ff' => 'dmcdougall',
  '85af9ad71af2dc0166b7c0c5780fa086' => 'caldwell',
  'fa64968e4a3c8e20364bb92ba7511ff9' => 'dvennink',
  '0669ff1e3ada91e7f1e7714f6f9a67f6' => 'etienne',
  '49ed289f3de94dbcd7c10392bcc40b53' => 'fernando82',
  '7b3ae2214891a47b26b4db98949c1bb0' => 'gknops',
  '34820bca697fbf1598774b393c5ca4fe' => 'whitlockjc',
  'ec9254734cd341f1b104d558dc4fc36a' => 'joachimm',
  '09c16a631eeba332147a8d620e1369cc' => 'muellerj',
  '6890db3146e20bfb99be3bc7bc3bfeec' => 'lczekaj',
  'e34425c11547a48a4701c9d1720dadf8' => 'infininight',
  '65efe3355478c8db96bc82f22fd3aa20' => 'nathanieltagg',
  'ccc5b318408880a67eeebf0d18177fb5' => 'rhencke',
  '4cf620221f7e622260f8424b8142451f' => 'ryanmaxwell',
  '5780111eb4b5565816d9388b091e1057' => 'youngrok',
  '1bafa0ecf5643c71e6d5dea309889d21' => 'bobrocke',
  '16e62cebf0c65d7018b263d0f8be36c1' => 'sclukey',
  'bee584c4bc4deac1ee91006b97a8fc53' => 'mstarke',
  '578b7853042db14893ee5ec2ce043f98' => 'yyyc514',
  '8838005371ab9c0b1d40f0504bf8832a' => 'garysweaver',
  '1b97e22672bc2577ebbb63ef895debd4' => 'jtmkrueger',
  '3413d8cb793e54a6e062391875fd2636' => 'jacob-carlborg',
  'a8cb0cb6a2406ee9d85ea72f7c040697' => 'jsuder',
  'af76f04ca3004be2d6b0690bd0a6ff7c' => 'luikore',
  'bbe6320b030b1bb50349e4554d3169d6' => 'AJ-Acevedo',
  'a734c5fda1ef1237fa6a26a64940d0b1' => 'Dirklectisch',
  '7640cae93abde468b73f35d6620a9b04' => 'caleb',
  'f889181fc58ccb702822b54fe3702d24' => 'codykrieger',
  '571db4b87bd7d2fec3dcd5524cb7d9ae' => 'rdwampler',
  'a4c0d688809489ab98a162b10c57381c' => 'dusek',
  '7e9f543f0ffdb7c9a899e628fe76e7f3' => 'jtbandes',
  '04581c59babdab9788e932ecb79f9617' => 'zadr',
  '0ee1291a38e3c76fdfaadb2a0fa3428a' => 'duanemoody',
  '71c216d75354dda636b879dfc95654fb' => 'charliepark',
  'c8591aebaf7659f1ff429898345f446a' => 'olegam',
  'f275727e33d63e05cc0abab1bfc41da7' => 'sudara'
}.freeze

def git(*args)
  # All output parsed by this script flows through here, so pin the encoding in
  # one place: the pipe is tagged with Encoding.default_external (US-ASCII under
  # the C locale), and String#split raises on any non-ASCII byte in a commit
  # message unless the string is UTF-8.
  stdout, stderr, status = Open3.capture3('git', '-c', 'i18n.logOutputEncoding=UTF-8', *args)
  unless status.success?
    warn "git #{args.join(' ')} failed"
    warn stderr unless stderr.empty?
    exit status.exitstatus || 1
  end
  stdout.force_encoding(Encoding::UTF_8).scrub
end

def html(text)
  CGI.escapeHTML(text.to_s)
end

def commit_url(github_url, hash)
  "#{github_url.chomp('/')}/commit/#{hash}"
end

def tree_url(github_url, hash)
  "#{github_url.chomp('/')}/tree/#{hash}"
end

def gravatar_url(email_hash)
  fallback = 'https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png'
  "https://www.gravatar.com/avatar/#{email_hash}?s=48&amp;d=#{fallback}"
end

def author_name(name, email_hash)
  login = KNOWN_GITHUB_USERS[email_hash]
  return html(name) unless login

  %(<a href="https://github.com/#{html(login)}">#{html(name)}</a>)
end

def commit_body(body)
  stripped = body.to_s.strip
  return '' if stripped.empty?

  <<~HTML
    <span class="hidden-text-expander inline"><a href="javascript:;" class="js-details-target">…</a></span>
  </p>
  <div class="commit-desc"><pre>#{html(stripped)}
  </pre></div>
  HTML
end

format = '%H%x1f%an%x1f%ae%x1f%aI%x1f%s%x1f%b%x1e'
records = git('log', revision, "--pretty=format:#{format}")
commits = records.split("\x1e").map do |record|
  hash, name, email, date, subject, body = record.strip.split("\x1f", 6)
  next if name == 'Allan Odgaard'

  email_hash = Digest::MD5.hexdigest(email.to_s)
  {
    hash: hash,
    short_hash: hash[0, 10],
    name: name,
    email_hash: email_hash,
    date: Time.iso8601(date),
    subject: subject,
    body: body
  }
end.compact

html = <<~HTML
  <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
  	"http://www.w3.org/TR/html4/strict.dtd">

  <html>

  <head>
  	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  	<link href="css/stylesheet.css" rel="stylesheet" type="text/css" />
  	<link rel="stylesheet" type="text/css" href="css/contributions.css" charset="utf-8" />
  	<script type="text/javascript" src="js/contributions.js" charset="utf-8"></script>
  	<title>Contributions</title>
  </head>

  <body>
  <h1 id="contributions">Contributions</h1>

  <p>See <a href="#{html(github_url.chomp('/'))}/commits/master">commits at GitHub</a>.</p>

  <div>

HTML

current_day = nil
commits.each do |commit|
  day = commit[:date].strftime('%Y-%m-%d')
  if day != current_day
    html += "</ol>\n\n" if current_day
    html += "<h3 class=\"commit-group-heading\">#{commit[:date].strftime('%b %e, %Y')}</h3>\n\n"
    html += "<ol class=\"commit-group\">\n\n"
    current_day = day
  end

  url = commit_url(github_url, commit[:hash])
  browse_url = tree_url(github_url, commit[:hash])
  date_title = commit[:date].strftime('%Y-%m-%d %H:%M:%S')
  date_label = commit[:date].strftime('%B %e, %Y')
  body_html = commit_body(commit[:body])
  close_title = body_html.empty? ? "\n    </p>\n    " : body_html

  html += <<~HTML
    <li class="commit commit-group-item">
        <img class="gravatar" src="#{gravatar_url(commit[:email_hash])}" height="36" width="36">
        <p class="commit-title">
          <a href="#{html(url)}" class="message">#{html(commit[:subject])}</a>
          #{close_title}<div class="commit-meta">
          <div class="commit-links">
            <a href="#{html(url)}" class="gobutton">
              <span class="sha">#{html(commit[:short_hash])}<span class="mini-icon mini-icon-arr-right-mini"></span></span>
            </a>
            <a href="#{html(browse_url)}" class="browse-button" title="Browse the code at this point in the history" rel="nofollow">Browse code <span class="mini-icon mini-icon-arr-right"></span></a>
          </div>
          <div class="authorship">
            <span class="author-name">#{author_name(commit[:name], commit[:email_hash])}</span>
            authored <time class="js-relative-date" datetime="#{html(commit[:date].iso8601)}" title="#{html(date_title)}">#{html(date_label)}</time>
          </div>
        </div>
    </li>

  HTML
end

html += "</ol>\n\n" if current_day
html += <<~HTML
  </div>

  </body>
  </html>
HTML

FileUtils.mkdir_p(File.dirname(output_file))
File.write(output_file, html)
puts "Generated #{output_file} (#{File.size(output_file)} bytes, #{commits.length} commits)"
