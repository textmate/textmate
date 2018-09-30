require 'spec_helper'

describe 'Document with notes and tasks outside of projects' do
  include SpecHelpers

  let(:document) do
    parse(
      "a note
       - a task
       another note
       a project:
       - with a task")
  end

  it 'adopts orphaned notes' do
    expect(document.notes.map(&:text)).to eq ['a note', 'another note']
  end

  it 'adopts orphaned tasks' do
    expect(document).to have(1).tasks
  end

  it 'correctly separates orphans from projects' do
    expect(document).to have(4).entries
    expect(document).to have(1).projects
  end

end
