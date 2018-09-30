%w(version
   indent_aware
   entry_container
   document
   raw_entry
   entry
   parser
).each do |lib|
  require "taskpaper_utils/#{lib}"
end

# Provides an API for parsing and working with taskpaper formated documents.
# @see README
module TaskpaperUtils

  # Parse a taskpaper formated file
  #
  # @param path [String] of file to parse
  # @return [Document]
  def self.parse_file(path)
    File.open(path) { |file| parse(file) }
  end

  # Parse any enumerable object containing taskpaper formated entries.
  #
  # @param enum [#each<String>] Collection of strings to parse.
  #   Should expose an #each method that returns successive string in taskpaper format.
  # @return [Document]
  def self.parse(enum)
    Parser.new.parse(enum)
  end

  # Serialize a {Document} to a file in taskpaper format
  #
  # @param document [Document] to serialize
  # @param path [String] of file to serialize to (will be overwritten)
  def self.save(document, path)
    File.open(path, 'w') do |file|
      document.dump { |raw_text| file.puts(raw_text) }
    end
  end

end
