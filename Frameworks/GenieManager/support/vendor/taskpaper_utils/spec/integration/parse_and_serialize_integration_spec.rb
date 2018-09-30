require 'spec_helper'
require 'tempfile'

module TaskpaperUtils
  describe TaskpaperUtils, 'integration:' do

    describe 'parsing and immediately saving the parsed object graph' do

      let(:exemplar_path) { File.join(__dir__, 'exemplar.taskpaper') }

      it 'results in a new file identical to the original' do
        # since the new file is based on the object graph, this serves as a good
        # integration test - anything we either don't parse or serialize correctly
        # will result in differing files
        document = TaskpaperUtils.parse_file(exemplar_path)
        Dir::Tmpname.create('taskpaper_utils_integration_spec-') do |new_file|
          begin
            TaskpaperUtils.save(document, new_file)
            expect(File.read(exemplar_path)).to eq(File.read(new_file))
          ensure
            File.delete(new_file) if File.exist?(new_file)
          end
        end
      end
    end
  end
end
