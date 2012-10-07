#!/System/Library/Frameworks/Ruby.framework/Versions/1.8/usr/bin/ruby -wKU
# == Synopsis
#
# Module to assist in building the Contributors page using git commit history.
#

require 'getoptlong'
require 'rdoc/usage'
require 'digest/md5'
require 'net/https'
require 'uri'
require 'cgi'
require 'dbm'
require 'date'

# built using json 1.7.5 gem
require 'rubygems'
require 'json'


# Helper class to handle searching for GitHub users
# by their email address. Caches mappings to a file
# to avoid exhausting the GitHub API rate limits for
# anonymous requests.
class GitHubLookup

  def self.initialize(dbm_file)
    @db = DBM.new(dbm_file)
    ObjectSpace.define_finalizer(@db, proc {|id| db.close })
  end

  def self.user_by_email(email)
    emailhash = Digest::MD5.hexdigest(email)
    if @db.has_key?(emailhash)
      return @db[emailhash]
    end

    url = 'https://api.github.com/legacy/user/email/' + email
    uri = URI.parse(url)
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true
    http.verify_mode = OpenSSL::SSL::VERIFY_NONE

    # issue request
    request = Net::HTTP::Get.new(uri.request_uri)
    response = http.request(request)

    # could be a 404, return nil if so
    if response.code == '404'
      return @db[emailhash] = nil
    end

    user = JSON.parse(response.body)
    # save result to k/v store
    return @db[emailhash] = user['user']['login']
  end

end

def generate_credits(dbm_file)
  GitHubLookup.initialize(dbm_file)

  # use git's log command to pull out basic info:
  # git hash, author name, email address, author date, commit summary
  cmd = 'git log -z --date=iso --pretty=format:"%H%n%an%n%ae%n%ad%n%s%n%B"'

  `#{cmd}`.split(/\x00/s).each {|commit|
    fields = commit.split(/\n/, 6)

    # omit commits from Allan; he gets enough credit already ;)
    if fields[1] != 'Allan Odgaard' then
      # hash email address for referencing Gravatar userpics
      emailhash = Digest::MD5.hexdigest(fields[2])

      # escape user-supplied bits like name and subject
      hash = fields[0]
      name = CGI.escapeHTML(fields[1])
      # locate the GitHub login for the author's email address
      user = GitHubLookup.user_by_email(fields[2])
      date = DateTime.parse(fields[3])
      subject = CGI.escapeHTML(fields[4])
      body = CGI.escapeHTML(fields[5].sub(fields[4], '').sub(/[\s\x00]+$/s, '').sub(/^[\s\x00]+/s, ''))
      userpic = "http://www.gravatar.com/avatar/#{emailhash}?s=48&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png"

      # if we have a github username, populate a link to their
      # profile.
      if !user || user == ''
        user = nil
        STDERR << "WARNING: failed to find GitHub user for #{name} <#{fields[2]}>\n";
      end

      yield(hash, name, subject, body, userpic, date, user)
    end
  }
end

__END__
# Contributions

See [commits at GitHub][1].

<table width="100%">
    <tr>
        <th width="20%">Author</th>
        <th width="60%">Contribution</th>
        <th width="20%">Date</th>
    </tr>
<%
require 'bin/gen_credits'
credits = generate_credits(File.expand_path('~/Library/Caches/com.macromates.TextMate/githubcredits'))
%>
<%= credits %>
</table>

[1]: https://github.com/textmate/textmate/commits/master
