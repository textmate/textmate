#!/usr/bin/env ruby
require 'uri'
require 'nokogiri'
require 'shellwords'
require 'digest/sha1'
require 'cgi'

CACHE_DIR = '.' # We rely on the calling process to change to cache directory

MIME_TYPE_RANK = {
  'image/svg+xml' => 10,
  'image/png'     => 20,
  'image/gif'     => 30,
  'image/x-icon'  => 40,
}

def icons_using_nokigiri(html, page_url)
  page = Nokogiri::HTML(html)
  abort "error: parse error for #{page_url}" if page.nil?

  selectors = [
    'link[rel="apple-touch-icon"]',
    'link[rel="shortcut icon"]',
    'link[rel="icon"]',
  ]

  icons = []
  selectors.each do |selector|
    if node_set = page.css(selector)
      STDERR << "#{selector}: #{node_set.count} nodes matches\n"
      node_set.each do |node|
        url = URI.join(page_url, node['href']).to_s
        res = { :url => url }

        path = URI.parse(url).path
        case File.extname(path)
          when '.svg' then res[:type] = 'image/svg+xml'
          when '.png' then res[:type] = 'image/png'
          when '.gif' then res[:type] = 'image/gif'
          when '.ico' then res[:type] = 'image/x-icon'
          else res[:type] = 'unknown' # don’t trust node['type']
        end

        if node.key?('sizes')
          res[:sizes] = node['sizes']
        elsif path =~ /-(\d+x\d+)[.\-]/
          res[:sizes] = $1
        end

        icons << res
      end
    else
      STDERR << "#{selector}: No matching nodes\n"
    end
  end
  icons
end

# def parse_link_tags(html)
#   tags = html.scan(/<link\s*(.+?)\s*\/?>/m).flatten
#   tags = tags.map do |tag|
#     tag = tag.scan(/(\S+)\s*=\s*(?:'([^']*)'|"([^"]*)"|(\S+))/).map do |attr, single, double, bare|
#       [ attr, CGI.unescapeHTML(single || double || bare) ]
#     end
#     Hash[tag]
#   end
#   tags.select { |tag| tag['rel'] =~ /^(apple-touch-icon|shortcut icon|icon)$/ }
# end
#
# def icons_using_regex(html, page_url)
#   icons = []
#   parse_link_tags(html).each do |tag|
#     url = URI.join(page_url, tag['href']).to_s
#     res = { :url => url }
#
#     path = URI.parse(url).path
#     case File.extname(path)
#       when '.svg' then res[:type] = 'image/svg+xml'
#       when '.png' then res[:type] = 'image/png'
#       when '.gif' then res[:type] = 'image/gif'
#       when '.ico' then res[:type] = 'image/x-icon'
#       else res[:type] = 'unknown' # don’t trust tag['type']
#     end
#
#     if tag.key?('sizes')
#       res[:sizes] = tag['sizes']
#     elsif path =~ /-(\d+x\d+)[.\-]/
#       res[:sizes] = $1
#     end
#
#     icons << res
#   end
#   icons
# end

def rank(type)
  MIME_TYPE_RANK[type] || 1000
end

def area(sizes)
  if sizes && sizes =~ /^(\d+)x(\d+)$/
    $1.to_i * $2.to_i
  elsif sizes == 'any'
    512 * 512
  else
    16 * 16
  end
end

def get_best_icon(page_url)
  html = %x{ /usr/bin/curl -sL #{page_url.shellescape} }
  html.force_encoding('utf-8')
  abort "error: curl returned #{$?.exitstatus} for #{page_url}" unless $?.exitstatus == 0

  abort "error: empty page at #{page_url}" if html.empty?

  icons = icons_using_nokigiri(html, page_url)
  # icons = icons_using_regex(html, page_url)

  icons = icons.reject { |e| e[:type] == 'unknown' || e[:type] == 'image/svg+xml' }
  icons = icons.sort do |lhs, rhs|
    rank(lhs[:type]) == rank(rhs[:type]) ? area(rhs[:sizes]) <=> area(lhs[:sizes]) : rank(lhs[:type]) <=> rank(rhs[:type])
  end

  abort "No icons at #{page_url} (page is #{html.size} bytes)" if icons.empty?

  icons.each { |icon| STDERR << icon.inspect << "\n" }
  icons.each do |e|
    url  = e[:url]
    path = File.join(CACHE_DIR, Digest::SHA1.hexdigest(url) + File.extname(URI.parse(url).path))
    return path if File.exists?(path)

    STDERR << %x{ /usr/bin/curl -sLo #{path.shellescape} #{url.shellescape} }
    return path if $?.exitstatus == 0

    STDERR << "error: no icon at #{url}\n"
  end

  nil
end

if url = (ARGV.first || 'https://duckduckgo.com/?q=foo')
  url = $1 if url =~ /(.*?)\?.*/
  print get_best_icon(url)
end
