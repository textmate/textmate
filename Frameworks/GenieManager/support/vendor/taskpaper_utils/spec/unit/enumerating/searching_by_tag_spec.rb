require 'spec_helper'

describe 'Searching by tags and tag values' do
  include SpecHelpers

  describe 'tagged' do
    let(:document) do
      parse(
        "project a:
         \t- task without tags
         \t- task 1 with @tag(1)
         \t- task 2 with @tag(2)
         project b: @tag
         \t- parent task
         \t\t- subtask with @tag
         project c:
         \t- parent with @tag @in @c
         \t\t- subtask also with @tag")
    end

    let(:project_a) { document['project a'] }
    let(:project_b) { document['project b'] }

    # note: we use .map(&:text) here because it verifies both the size and
    # contents of the tagged entries

    it 'finds children with the given tag' do
      expect(project_a.tagged('tag').map(&:text)).to eq(['task 1 with', 'task 2 with'])
    end

    it 'finds children with the given tag and value' do
      expect(project_a.tagged('tag', '2').map(&:text)).to eq(['task 2 with'])
    end

    describe 'nested matches' do

      it 'finds matching nested children' do
        expect(project_b.tagged(:tag).map(&:text)).to eq(['subtask with'])
      end

      it 'does not dig into children of a matching parent' do
        # 'parent with @tag @in @c', but not 'subtask also with @tag'
        expect(document['project c'].tagged(:tag).map(&:text)).to eq(['parent with'])
      end

      it 'combines matching entries from different levels of the doc' do
        expect(document.tagged(:tag).map(&:text_with_trailing_tags))
          .to eq(['task 1 with @tag(1)',
                  'task 2 with @tag(2)',
                  'project b: @tag',
                  'parent with @tag @in @c'])
      end
    end
  end
end
