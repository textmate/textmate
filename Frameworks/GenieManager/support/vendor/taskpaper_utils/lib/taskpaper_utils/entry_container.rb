module TaskpaperUtils

  # Container for {Entry} objects.
  #
  # Groups methods included into {Entry} and {Document} and provides most
  # of the public API for working with taskpaper objects.
  module EntryContainer
    include Enumerable

    # Class methods mixed into host when EntryContainer is included
    #
    # @api private
    module Generators
      def contains_entries_of_type(*types)
        types.each do |type|
          define_method("#{type.to_s}s") { children_of_type type }
        end
      end
    end

    # @api private
    def self.included(klass)
      klass.extend Generators
    end

    # Iterate over child entries.
    #
    # @return [Enumerator] if no block is given
    # @yield [Entry] each child entry nested under this one
    #
    # see Enumerable#each
    def each(&block)
      children.each(&block)
    end

    # Adds an entry to the container
    #
    # @param entry [Entry]
    # @return [Entry] the added entry.
    def add_entry(entry)
      children << entry
      entry.parent = self
      entry
    end

    alias_method :<<, :add_entry

    # Yields the whole subtree of raw text (starting at this entry) to the
    # block passed in.  First yields own raw_text (if it exists) and then
    # calls #dump an any children, resulting in the block being called
    # 0 to (children.size + 1) times.
    #
    # @yield [String] the raw text of this entry as well as the raw text of
    #   any of its children correctly indented.
    def dump(&block)
      yield raw_text if respond_to?(:raw_text)
      children.each { |entry| entry.dump(&block) }
    end

    # Locate a child entry by its text.
    #
    # @example
    #   # a project:
    #   #   - a task
    #   #   - another
    #   document['a project']['another']   # returns the second Task
    #
    # @param text [String] of the entry to be found without any
    #   type identifiers such as `- ` for tasks and `:` for projects
    def [](text)
      children.detect { |entry| entry.matches?(text) }
    end

    # Find all child entries with a matching tag (and optional value).
    # Searches the whole entry tree nested under this one.
    # Matching entries are returned with *all* their children intact,
    # regardless of whether those children individually match the search criteria.
    #
    # @example
    #   # Given this document:
    #   # project:
    #   #   - @tagged task
    #   #     - @tagged subtask
    #   #     - subtask without tag
    #   #   - task without tag
    #   # second project:
    #   #   - another task
    #   #     - another @tagged subtask
    #   found = document.tagged(:tagged)
    #   found.size              # => 2
    #   found.map(&:text)       # => ['@tagged task', 'another @tagged subtask']
    #   found.first.map(&:text) # => ['@tagged subtask', 'subtask without tag']
    #
    # @param tag [String|Symbol] the tag to search for
    # @param value [String|Symbol] the optional tag(value).
    #   If provided only entries matching the tag and exact value will be returned.
    # @return [Array] Matching entries
    def tagged(tag, value = nil)
      children.flat_map do |entry|
        entry.tag?(tag, value) ? entry : entry.tagged(tag, value)
      end
    end

    # @param (see #type?)
    # @return [Array] Children of the specified type
    #
    # @api private
    def children_of_type(entry_type)
      children.select { |entry| entry.type?(entry_type) }
    end

    # @param of [:project, :task, :note]
    # @return whether this Entry is of the specified type
    def type?(of)
      type == of
    end

    # @return [Fixnum] the number of child entries nested under this one
    def size
      children.size
    end

    # @return [Entry] the most recently added child entry or `nil`
    def last
      children.last
    end

    # @api private
    def children
      @children ||= []
    end
  end
end
