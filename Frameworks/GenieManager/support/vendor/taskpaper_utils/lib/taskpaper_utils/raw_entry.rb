module TaskpaperUtils

  # Encapsulates the parsing of a single line entry and captures the parsed elements.
  # Used to instantiate an {Entry}.
  #
  # @api private
  class RawEntry

    TAG_ATOM = /@(\w+)(?:\((.+?)\))?/

    IDENTIFIERS = {
      project: /:\Z/,
      task:    /\A(\s*)?- /,
      note:    ''
    }

    attr_reader :raw_text, :type, :text, :trailing_tags, :tags

    def initialize(raw_text)
      @raw_text = raw_text
      @text, @trailing_tags = split_text_and_trailing_tags
      @type = identify_type
      @text = strip_identifier
      @tags = parse_tags
    end

    def split_text_and_trailing_tags
      str = @raw_text.strip
      if matched = /( #{TAG_ATOM})+\Z/.match(str) # rubocop:disable AssignmentInCondition
        [matched.pre_match, matched[0]]
      else
        [str, '']
      end
    end

    def identify_type
      identifiable_as(:task) || identifiable_as(:project) || :note
    end

    def identifiable_as(type)
      @text =~ IDENTIFIERS[type] ? type : false
    end

    def strip_identifier
      text.sub(IDENTIFIERS[type], '')
    end

    def parse_tags
      @raw_text.scan(TAG_ATOM)
    end

  end
end
