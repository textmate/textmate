require 'spec_helper'

describe 'Basic parsing' do
  include SpecHelpers

  describe 'type identification' do

    describe 'recognizes basic entry types' do
      specify('a project') { expect('a project:').to be_identified_as_a(:project) }
      specify('a task   ') { expect('- a task  ').to be_identified_as_a(:task) }
      specify('a note   ') { expect('a note    ').to be_identified_as_a(:note) }
    end

    describe 'edge cases:' do
      it 'recognizes tasks that end with a colon' do
        expect('- task or project?:').to be_identified_as_a(:task)
      end

      it 'recognizes projects with trailing tags' do
        expect('project: @with @trailing(tags)').to be_identified_as_a(:project)
      end
    end

    RSpec::Matchers.define :be_identified_as_a do |type|
      match do |raw_text|
        @actual = parse_entry(raw_text).type
        @actual == type
      end

      failure_message_for_should do |raw_text|
        "Expected '#{raw_text}' to be identified as a '#{type}', not '#{@actual}'"
      end
    end

  end

  describe 'a simple document' do

    let(:document) do
      parse(
        "Project A:
         - task one
         \t- subtask
         - task two
         a note
         \t- subtask of a note
         Project B:")
    end
    let(:project_a) { document['Project A'] }

    it 'contains projects' do
      expect(document).to have(2).projects
      expect(document.projects.map(&:text)).to eq ['Project A', 'Project B']
    end

    it 'contains tasks within projects' do
      expect(project_a).to have(2).tasks
      expect(project_a.tasks.map(&:text)).to eq ['task one', 'task two']
    end

    it 'contains subtasks nested under tasks' do
      task_one = project_a['task one']
      expect(task_one).to have(1).subtasks
      expect(task_one['subtask']).to_not be_nil
    end

    it 'contains notes within projects' do
      expect(project_a).to have(1).notes
      expect(project_a['a note']).to_not be_nil
    end
  end
end
