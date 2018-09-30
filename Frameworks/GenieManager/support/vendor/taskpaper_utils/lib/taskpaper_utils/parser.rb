module TaskpaperUtils

  # Parses a taskpaper formated document (accepted as an Enumerable)
  # Produces an object graph rooted at {Document}
  #
  # @api private
  class Parser

    def parse(enum)
      @current = document = Document.new
      enum.each { |line| parse_line(line) }
      document
    end

    def parse_line(line)
      @preceding = @current
      @current = Entry.parse(line)
      identify_parent << @current
      @current
    end

    private

      def identify_parent   # rubocop:disable IndentationWidth
        parent_if_preceding_is_document ||
        parent_if_preceding_less_indented ||
        parent_if_preceding_equally_indented ||
        parent_if_preceding_more_indented
      end

      def parent_if_preceding_is_document
        @preceding if @preceding.type? :document
      end

      def parent_if_preceding_less_indented
        @preceding if @preceding.indentation < @current.indentation
      end

      def parent_if_preceding_equally_indented
        if @preceding.indentation == @current.indentation
          parent_if_unindented_project ||
          parent_if_unindented_child_of_a_project ||
          @preceding.parent
        end
      end

      def parent_if_preceding_more_indented
        if @preceding.indentation > @current.indentation
          @preceding = @preceding.parent
          identify_parent   # recurse
        end
      end

      def parent_if_unindented_project
        @preceding.document if @current.unindented && @current.type?(:project)
      end

      def parent_if_unindented_child_of_a_project
        @preceding if @current.unindented && @preceding.type?(:project)
      end

  end
end
