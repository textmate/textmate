require 'spec_helper'

describe 'Parent identification' do
  include SpecHelpers

  describe 'simple indentation:' do

    let(:doc) do
      parse(
        "project:
         \t- task x
         \t- task y")
    end
    let(:project) { doc['project'] }

    describe 'entry indented relative to preceding entry' do
      it 'identifies the previous entry as the parent' do
        expect(project).to be_parent_of('task x')
      end
    end

    describe 'sibling entry (same indent as preceding line)' do

      it "identifies the preceding entry's parent as its own parent" do
        expect(project).to be_parent_of('task y')
      end

      it "appends to the end of parent's collection of children" do
        expect(project.last.text).to eq('task y')
      end

    end
  end

  describe 'unindented lines:' do

    describe 'not within a project' do

      specify 'are children of the Document' do
        doc = parse("one\ntwo")
        expect(doc).to be_parent_of('one')
        expect(doc).to be_parent_of('two')
        expect(doc.size).to eq(2)
      end

      specify 'including projects on consecutive lines' do
        doc = parse("project a:\nproject b:")
        expect(doc).to be_parent_of('project a')
        expect(doc).to be_parent_of('project b')
      end

      specify 'even when a project is preceded by an unindented task' do
        doc = parse(
          "project a:
           - unindented
           project b:")
        expect(doc).to be_parent_of('project b')
      end
    end

    describe 'under a project' do
      specify 'belong to the project even without indents' do
        doc = parse("project:\n- task")
        expect(doc['project']).to be_parent_of('task')
      end
    end
  end

  describe 'multiple indents and outdents:' do

    let(:doc) do
      parse(
        "project a:
         \t- task x
         \t\t- subtask of x
         \t- task y
         \t\t- subtask of y
         project b:")
    end

    specify 'subtasks are children of tasks' do
      expect(doc['project a']['task x']).to be_parent_of('subtask of x')
      expect(doc['project a']['task y']).to be_parent_of('subtask of y')
    end

    describe 'outdenting by one level' do
      it "means the entry is a sibling of the preceding entry's parent" do
        expect(doc['project a']).to be_parent_of('task y')
      end
    end

    describe 'outdenting by more than one level' do
      it 'should identify the correct parent' do
        expect(doc).to be_parent_of('project b')
      end
    end

  end

  RSpec::Matchers.define :be_parent_of do |child_text|
    match do |parent|
      child = parent[child_text]
      !child.nil? && child.parent == parent
    end
  end

end
