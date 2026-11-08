#!/usr/bin/env ruby
# frozen_string_literal: true

require "json"
require "pathname"

# Theme preview: Ruby modules, symbols, interpolation, regex, and blocks.
module Preview
  VERSION = "2.4.0"
  DEFAULT_LIMIT = 1_024
  PATTERN = /(?<name>[a-z]+)\s+(\d+)/i

  class Theme
    attr_reader :name, :colors
    attr_accessor :dark

    def initialize(name, dark: false, **colors)
      @name = name
      @dark = dark
      @colors = colors.freeze
    end

    def description
      "#{name}: #{dark ? 'dark' : 'light'}"
    end

    def self.load(path)
      data = JSON.parse(Pathname(path).read, symbolize_names: true)
      new(data.fetch(:name), **data.fetch(:colors, {}))
    rescue JSON::ParserError => error
      warn "Invalid theme: #{error.message}"
      raise
    ensure
      puts "Finished reading"
    end
  end
end

items = [
  Preview::Theme.new("TextMate", accent: "#58a6ff"),
  Preview::Theme.new("Midnight", dark: true, accent: "#ffcc00")
]

numbers = [0xff, 0b1010, 3.14159, 6.02e23, -42]
escaped = "Tab:\t newline:\n star:\u2605"
words = %w[comments strings constants functions]
message = <<~REPORT
  Theme report
  Version: #{Preview::VERSION}
  Count: #{items.length}
  Words: #{words.join(', ')}
REPORT

items.each_with_index do |theme, index|
  next if theme.dark
  puts "#{index}: #{theme.description}"
end

result = numbers.select(&:positive?).map { |value| value * 2 }
puts message, escaped, result.inspect
puts Preview::PATTERN.match("theme 42")&.named_captures
