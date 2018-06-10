#!/usr/bin/env ruby
require 'uri'
require 'cgi'
require 'nokogiri'
require 'shellwords'
require 'json'
require 'pp'

MIME_TYPE_RANK = {
  'image/svg+xml' => 10,
  'image/png'     => 20,
  'image/gif'     => 30,
  'image/x-icon'  => 40,
}

def rank(type)
  MIME_TYPE_RANK[type] || 1000
end

def area(sizes)
  (sizes || []).map { |size| size[:any] ? 512*512 : size[:width] * size[:height] }.max || 16*16
end

def parse_sizes(str)
  str.scan(/(\d+)x(\d+)|(\bany\b)/i).map do |size|
    size.last == 'any' ? { :any => true } : { :width => size[0].to_i, :height => size[1].to_i }
  end
end

def icons_using_nokigiri(html, page_url)
  page = Nokogiri::HTML(html)
  abort "Parse error for #{page_url}" if page.nil?

  icons = []
  relations = [ 'shortcut icon', 'icon', 'apple-touch-icon' ]
  relations.each do |relation|
    if node_set = page.css("link[rel='#{relation}']")
      node_set.each do |node|
        url = URI.join(page_url, node['href']).to_s
        res = { :url => url, :relation => relation }

        path = URI.parse(url).path
        case File.extname(path)
          when '.svg' then res[:type] = 'image/svg+xml'
          when '.png' then res[:type] = 'image/png'
          when '.gif' then res[:type] = 'image/gif'
          when '.ico' then res[:type] = 'image/x-icon'
          else res[:type] = 'unknown' # don’t trust node['type']
        end

        if node.key?('sizes')
          res[:sizes] = parse_sizes(node['sizes'])
        elsif path =~ /[_-](\d+x\d+)[.\-]/
          res[:sizes] = parse_sizes($1)
        end

        icons << res
      end
    end
  end
  icons
end

def find_icons(page_url)
  html = %x{ /usr/bin/curl -sL #{page_url.shellescape} }
  html.force_encoding('utf-8')

  abort "curl returned #{$?.exitstatus} for #{page_url}" unless $?.exitstatus == 0
  abort "Empty page at #{page_url}" if html.empty?

  icons = icons_using_nokigiri(html, page_url)

  icons = icons.reject { |e| e[:type] == 'unknown' || e[:type] == 'image/svg+xml' }
  icons = icons.sort do |lhs, rhs|
    rank(lhs[:type]) == rank(rhs[:type]) ? area(rhs[:sizes]) <=> area(lhs[:sizes]) : rank(lhs[:type]) <=> rank(rhs[:type])
  end

  if icons.empty?
    favicon_url = page_url.sub(%r{^(https?://[^/]+).*$}, '\1/favicon.ico')
    status = %x{ /usr/bin/curl -s -o /dev/null -w "%{http_code}" #{favicon_url.shellescape} }
    if status == '200'
      icons << { :url => favicon_url, :relation => 'auto-discover' }
    end
  end

  abort "No icons at #{page_url} (page is #{html.size} bytes)" if icons.empty?

  return icons
end

if url = (ARGV.first || 'https://duckduckgo.com/?q=foo')
  url = $1 if url =~ /(.*?)\?.*/
  STDOUT << find_icons(url).to_json << "\n"
else
  abort "requires URL argument"
end
