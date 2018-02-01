Title: Contributions
CSS: css/contributions.css
JS: js/contributions.js

# Contributions

See [commits at GitHub][1].

<div>
<%# this wrapping div prevents Markdown from trying to parse the ERB blocks %>
<%
last_group_heading = ''
require File.join(File.dirname(__FILE__), 'bin/gen_credits')
generate_credits(File.expand_path('~/Library/Caches/com.macromates.TextMate/githubcredits'), warn) do |hash, author, subject, body, userpic, date, github_user|
  group_heading = date.strftime('%b %e, %Y')
  if last_group_heading != group_heading
    if last_group_heading != ''
        _erbout << "</ol>\n"
    end
%>
<h3 class="commit-group-heading"><%= group_heading %></h3>

<ol class="commit-group">
<%
    last_group_heading = group_heading
  end
%>
  <li class="commit commit-group-item">
    <img class="gravatar" src="<%= userpic %>" height="36" width="36">
    <p class="commit-title">
      <a href="https://github.com/textmate/textmate/commit/<%= hash %>" class="message"><%= subject %></a>
      <% if body != '' %><span class="hidden-text-expander inline"><a href="javascript:;" class="js-details-target">…</a></span><% end %>
    </p>
    <% if body != '' %><div class="commit-desc"><pre><%= body %></pre></div><% end %>
    <div class="commit-meta">
      <div class="commit-links">
        <a href="https://github.com/textmate/textmate/commit/<%= hash %>" class="gobutton">
          <span class="sha"><%= hash[0,10] %><span class="mini-icon mini-icon-arr-right-mini"></span></span>
        </a>
        <a href="https://github.com/textmate/textmate/tree/<%= hash %>" class="browse-button" title="Browse the code at this point in the history" rel="nofollow">Browse code <span class="mini-icon mini-icon-arr-right"></span></a>
      </div>
      <div class="authorship">
        <span class="author-name"><% if github_user %><a href="http://github.com/<%= github_user %>"><% end %><%= author %><% if github_user %></a><% end %></span>
        authored <time class="js-relative-date" datetime="<%= date.strftime('%Y-%m-%dT%H:%M:%S%:z') %>" title="<%= date.strftime('%Y-%m-%d %H:%M:%S') %>"><%= date.strftime('%B %e, %Y') %></time>
      </div>
    </div>
  </li>
<%
end
if last_group_heading != ''
    _erbout << "</ol>\n"
end
%>
</div>

[1]: https://github.com/textmate/textmate/commits/master
