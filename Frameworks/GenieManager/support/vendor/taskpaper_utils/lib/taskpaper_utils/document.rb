module TaskpaperUtils

  # Root object representing a taskpaper formated document.
  # Contains nested {Entry} objects that represent projects, tasks and notes.
  class Document
    include EntryContainer

    # @!method projects
    #   @return [Array<Entry>] any children of type :project
    # @!macro contains_tasks
    #   @!method tasks
    #     @return [Array<Entry>] any children of type :task
    # @!macro contains_notes
    #   @!method notes
    #     @return [Array<Entry>] any children of type :note
    contains_entries_of_type :project, :task, :note

    # @return [nil] nil since this is the root object
    #
    # @api private
    def parent
      nil
    end

    # @return [Document] self
    #
    # @api private
    def document
      self
    end

    # @return [Symbol] :document
    #
    # @api private
    def type
      :document
    end

  end
end
