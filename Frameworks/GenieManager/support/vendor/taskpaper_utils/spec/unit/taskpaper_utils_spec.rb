require 'spec_helper'

module TaskpaperUtils
  describe TaskpaperUtils, 'Entry points' do

    # These specs are really not much use: they just set mocked expectations
    # for the current implementation and the code is already well covered by
    # integration specs.
    #
    # However, keeping these 'unit' specs allows spec coverage to stay at
    # 100% which is worth maintaining.

    let(:file)   { double(File) }

    describe '#parse' do

      let(:parser) { double(Parser) }

      it 'delegates to a new parser with an open file at the given path' do
        File.should_receive(:open).with('/path').and_yield(file)
        Parser.should_receive(:new).and_return(parser)
        parser.should_receive(:parse).with(file)
        TaskpaperUtils.parse_file('/path')
      end

    end

    describe '#save' do

      let(:document) { double(Document) }

      it 'calls document#dump and writes to an open file at the given path' do
        File.should_receive(:open).with('/path', 'w').and_yield(file)
        document.should_receive(:dump).and_yield('content')
        file.should_receive(:puts).with('content')
        TaskpaperUtils.save(document, '/path')
      end

    end
  end
end
