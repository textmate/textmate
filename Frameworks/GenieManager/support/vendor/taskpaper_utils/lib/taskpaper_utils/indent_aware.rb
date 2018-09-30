module TaskpaperUtils

  # Expects host object to provide a #raw_text method
  #
  # @api private
  module IndentAware

    def indentation
      raw_text[/\A\t*/].size
    end

    def unindented
      indentation == 0
    end

  end
end
