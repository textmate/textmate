require 'spec_helper'

describe 'Referencing child entries' do
  include SpecHelpers

  describe 'with simple text' do
    let(:doc)  { parse("\t- a task") }
    let(:task) { doc.first }

    it 'uses the text stripped of signifiers (such as the dash before a task)' do
      expect(doc['a task']).to eq(task)
    end

    it 'matches the whole text, not just a part of it' do
      expect(doc['a']).to be_nil
    end
  end

  describe 'with text containing trailing @tags' do
    let(:doc)  { parse('- with @a(tag)') }
    let(:task) { doc.first }

    it 'matches without trailing tag' do
      expect(doc['with']).to eq(task)
    end

    it 'matches with the whole text including tags' do
      expect(doc['with @a(tag)']).to eq(task)
    end
  end

  describe 'a project with trailing @tags' do
    let(:doc)     { parse('project: @with @tags') }
    let(:project) { doc.first }

    it 'matches without colon and trailing tags' do
      expect(doc['project']).to eq(project)
    end

    it 'includes the colon when matching against the whole text' do
      expect(doc['project: @with @tags']).to eq(project)
    end
  end
end
