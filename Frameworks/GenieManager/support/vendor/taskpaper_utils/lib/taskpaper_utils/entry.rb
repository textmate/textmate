module TaskpaperUtils

  # Represents a single entry (project, task or note)
  class Entry
    extend Forwardable
    include IndentAware
    include EntryContainer

    # @macro contains_tasks
    # @macro contains_notes
    contains_entries_of_type :task, :note

    # (see #tasks)
    alias_method :subtasks, :tasks

    # @!attribute [r] raw_text
    #   @return [String] the single line of raw text for this entry in taskpaper format
    # @!attribute [r] type
    #   @return [Symbol] the type of this entry, :project, :task, or :note
    # @!attribute [r] text
    #   @return [String] the text of the entry without tabs,
    #     identifiers (such as '-' or ':') or trailing tags
    # @!attribute [r] trailing_tags
    #   @return [String] any tags that follow the text or an empty string ('') otherwise.
    #     Tags mixed in with the text are not considered trailing tags.
    #     Includes a preceding space if trailing tags exist.
    #   @example
    #     entry = Entry.parse("- task with a @tag mixed in @and @trailing @tags")
    #     entry.text  # => "task with a @tag mixed in"
    #     entry.trailing_tags   # => " @and @trailing @tags"
    def_delegators :@raw, :raw_text, :type, :text, :trailing_tags

    # @return [Entry] parent {Entry} to which this one belongs
    attr_reader :parent

    # @api private
    attr_writer :parent

    # @api private
    def self.parse(raw_text)
      new(RawEntry.new(raw_text))
    end

    # @api private
    def initialize(raw_entry)
      @raw = raw_entry
    end

    # @return [String] Text of the entry with trailing tags included.
    #   Excludes tabs and identifiers but keeps the colon(:) for projects
    #   @see {#trailing_tags}
    def text_with_trailing_tags
      # todo: type smell?
      "#{text}#{type?(:project) ? ':' : ''}#{trailing_tags}"
    end

    # @param string [String] to test against
    # @return [Boolean] whether the string provided matches the entry text.
    #   Tests against the text with and without trailing tags (see {#trailing_tags}).
    def matches?(string)
      text == string || text_with_trailing_tags == string
    end

    # Convenience accessor for the root document to which this {Entry} belongs
    # @return [Document]
    def document
      parent.document
    end

    # @param name [String] the name of the tag to lookup
    # @return [String] '' if this entry is tagged with no value (eg: @tag)
    # @return [String] the value of the tag if a value exists (eg: @tag(value))
    # @return [nil] if its not tagged with the given name
    def tag_value(name)
      tag, value = @raw.tags.detect { |tag, value| tag == name.to_s }
      tag && (value || '')
    end

    def tag?(tag, value = nil)
      if tag_value = tag_value(tag)   # rubocop:disable AssignmentInCondition
        value ? value.to_s == tag_value : true
      else
        false
      end
    end

    # only for troubleshooting
    # def inspect
    #   "#<#{self.class.name}:#{text}>"
    # end
  end
end
