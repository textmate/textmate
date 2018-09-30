require 'spec_helper'

describe 'Retrieving complete raw text from a document' do
  include SpecHelpers

  let(:doc) do
    parse(
      "note
       project a:
       \t- task
       \t\t- subtask
       project b:")
  end

  it 'walks the tree of nested entries, yielding raw text from each' do
    expect { |b| doc.dump(&b) }
      .to yield_successive_args(
        "note\n", "project a:\n", "\t- task\n", "\t\t- subtask\n", 'project b:')
  end
end
