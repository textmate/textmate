require 'spec_helper'

describe 'Tag parsing' do
  include SpecHelpers

  describe 'tags in a single entry' do

    it 'parses a tag at the end of the line' do
      expect('last @word').to be_parsed_into_tags(['word', nil])
    end

    it 'parses a tag in the middle of a line' do
      expect('before @and after').to be_parsed_into_tags(['and', nil])
    end

    it 'allows tags to have a value' do
      expect('@tag(with value)').to be_parsed_into_tags(['tag', 'with value'])
    end

    it 'parses multiple tags' do
      expect('numbers: @one(1) and @two(2) but not three)')
      .to be_parsed_into_tags(%w(one 1), %w(two 2))
    end

    RSpec::Matchers.define :be_parsed_into_tags do |*array_of_tags|
      match do |string|
        # do we need to check no extraneous tags are parsed?
        entry = parse_entry(string)
        @unmatched = array_of_tags.reject { |tag, value| entry.tag?(tag, value) }
        @unmatched.empty?
      end

      failure_message_for_should do |string|
        "Tags #{@unmatched} were not parsed from '#{string}'"
      end
    end

  end

end
