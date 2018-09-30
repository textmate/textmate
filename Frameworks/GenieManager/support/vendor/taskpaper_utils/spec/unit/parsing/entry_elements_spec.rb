require 'spec_helper'

describe 'Elements parsed from a single entry' do
  include SpecHelpers

  describe '#text' do
    it 'strips leading dash and indents from tasks' do
      expect(parse_entry("\t\t- task").text).to eq('task')
    end

    it 'strips trailing colon from project' do
      expect(parse_entry('things:').text).to eq('things')
    end

    it 'just returns the input text for a note' do
      expect(parse_entry('any text').text).to eq('any text')
    end
  end

  describe '#tag?' do

    let(:entry) { parse_entry('with @empty tag and one @with(value)') }

    describe 'when the tag does not exist' do
      specify '#tag? returns false' do
        expect(entry.tag?('nope')).to equal(false)
      end

      specify '#tag_value returns nil' do
        expect(entry.tag_value('nope')).to be_nil
      end
    end

    describe 'when the tag exists without a value' do
      specify '#tag? returns true' do
        expect(entry.tag?('empty')).to equal(true)
      end

      specify '#tag_value returns an empty string' do
        expect(entry.tag_value('empty')).to eq('')
      end
    end

    describe 'when the tag exists with a value' do

      describe '#tag?' do
        it 'returns true when given no value' do
          expect(entry.tag?('with')).to equal(true)
        end

        it 'returns true given the matching value' do
          expect(entry.tag?('with', 'value')).to equal(true)
        end

        it 'returns false given a different value' do
          expect(entry.tag?('with', 'wrong value')).to equal(false)
        end

        it 'works with symbols' do
          expect(entry.tag?(:with, :value)).to equal(true)
        end
      end

      describe '#tag_value' do
        it 'returns the value' do
          expect(entry.tag_value('with')).to eq('value')
        end

        it 'works with a symbol' do
          expect(entry.tag_value(:with)).to eq('value')
        end
      end
    end
  end
end
