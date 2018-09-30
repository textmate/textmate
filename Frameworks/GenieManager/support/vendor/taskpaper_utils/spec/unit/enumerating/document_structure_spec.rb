require 'spec_helper'

describe 'Document structure' do
  include SpecHelpers

  shared_examples_for 'it may contain' do |*types|
    types.each do |type|
      specify type do
        expect(subject.send(type)).to have(1).entry
      end
    end
  end

  shared_examples_for 'it may not contain projects' do
    it 'does not provide an accessor for projects' do
      expect(subject.public_methods).not_to include(:projects)
    end
  end

  describe 'a document' do
    subject(:doc) do
      parse(
        "note
         - task
         project:")
    end

    it_behaves_like 'it may contain', :projects, :tasks, :notes
  end

  describe 'a project' do
    subject(:project) do
      parse(
        "project:
        \t- task
        \tnote"
      ).first
    end

    it_behaves_like 'it may contain', :tasks, :notes

    it_behaves_like 'it may not contain projects'
  end

  describe 'a task' do
    subject(:task) do
      parse(
        "- task
         \tnote
         \t- subtask"
      ).first
    end

    it_behaves_like 'it may contain', :tasks, :notes, :subtasks

    it_behaves_like 'it may not contain projects'
  end

  describe 'a note' do
    subject(:note) do
      parse(
        "note
         \t- task
         \tanother note"
      ).first
    end

    it_behaves_like 'it may contain', :tasks, :notes

    it_behaves_like 'it may not contain projects'
  end
end
