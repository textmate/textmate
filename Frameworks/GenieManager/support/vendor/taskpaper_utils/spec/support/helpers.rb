module SpecHelpers

  def parse(document_text)
    TaskpaperUtils.parse(lines(document_text))
  end

  def parse_entry(line)
    parse(line).first
  end

  def lines(string)
    string.gsub(/^ +/, '').lines
  end

end
