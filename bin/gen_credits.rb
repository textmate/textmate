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
require 'yaml'

# Helper class to handle searching for GitHub users
# by their email address. Caches mappings to a file
# to avoid exhausting the GitHub API rate limits for
# anonymous requests.
class GitHubLookup

  def self.initialize(dbm_file)
    @db = DBM.new(dbm_file)
    # seed with some contributors that don't have an email
    # address assigned publicly in their account
    @db['1178ce2f664a6cee9a05a3e11af5d8d2'] = 'aaronbrethorst'
    @db['3b0ef5e2a5f1aa3ccf3f23a20adf8873'] = 'Hoverbear'
    @db['ff3502050b3b1b00cb6c810d5c41ffc9'] = 'bradchoate'
    @db['ee646002e51a3c83e01db85ae42187ff'] = 'dmcdougall'
    @db['85af9ad71af2dc0166b7c0c5780fa086'] = 'caldwell'
    @db['fa64968e4a3c8e20364bb92ba7511ff9'] = 'dvennink'
    @db['0669ff1e3ada91e7f1e7714f6f9a67f6'] = 'etienne'
    @db['49ed289f3de94dbcd7c10392bcc40b53'] = 'fernando82'
    @db['7b3ae2214891a47b26b4db98949c1bb0'] = 'gknops'
    @db['34820bca697fbf1598774b393c5ca4fe'] = 'whitlockjc'
    @db['ec9254734cd341f1b104d558dc4fc36a'] = 'joachimm'
    @db['09c16a631eeba332147a8d620e1369cc'] = 'muellerj'
    @db['6890db3146e20bfb99be3bc7bc3bfeec'] = 'lczekaj'
    @db['e34425c11547a48a4701c9d1720dadf8'] = 'infininight'
    @db['65efe3355478c8db96bc82f22fd3aa20'] = 'nathanieltagg'
    @db['4e89e196a1f8fa34a6bdc6d165f75e5e'] = 'Ralle'
    @db['ccc5b318408880a67eeebf0d18177fb5'] = 'rhencke'
    @db['4cf620221f7e622260f8424b8142451f'] = 'ryanmaxwell'
    @db['5780111eb4b5565816d9388b091e1057'] = 'youngrok'
    @db['1bafa0ecf5643c71e6d5dea309889d21'] = 'bobrocke'
    @db['16e62cebf0c65d7018b263d0f8be36c1'] = 'sclukey'
    @db['bee584c4bc4deac1ee91006b97a8fc53'] = 'mstarke'
    @db['578b7853042db14893ee5ec2ce043f98'] = 'yyyc514'
    @db['8838005371ab9c0b1d40f0504bf8832a'] = 'garysweaver'
    @db['1b97e22672bc2577ebbb63ef895debd4'] = 'jtmkrueger'
    @db['3413d8cb793e54a6e062391875fd2636'] = 'jacob-carlborg'
    @db['a8cb0cb6a2406ee9d85ea72f7c040697'] = 'jsuder'
    @db['af76f04ca3004be2d6b0690bd0a6ff7c'] = 'luikore'
    @db['bbe6320b030b1bb50349e4554d3169d6'] = 'AJ-Acevedo'
    @db['a734c5fda1ef1237fa6a26a64940d0b1'] = 'Dirklectisch'
    @db['7640cae93abde468b73f35d6620a9b04'] = 'caleb'
    @db['f889181fc58ccb702822b54fe3702d24'] = 'codykrieger'
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
    request = Net::HTTP::Get.new(uri.request_uri, {'User-Agent' => 'curl'})
    response = http.request(request)

    # could be a 404, return nil if so
    if response.code == '404'
      return @db[emailhash] = nil
    end

    user = YAML.load(response.body)
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
