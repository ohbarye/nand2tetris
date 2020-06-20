require_relative "./jack_tokenizer.rb"

class CompilerEngine
  def initialize(path)
    @outfile = File.open(path.sub(".jack", ".impl.xml"), "w")
    @tokenizer = JackTokenizer.new(path)
  end

  def compile
  end
end