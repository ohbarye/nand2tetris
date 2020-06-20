require_relative "./jack_tokenizer.rb"

class CompilerEngine
  def initialize(path)
    @output = File.open(path, "w")
    @tokenizer = JackTokenizer.new(path)
  end

  def compile
    puts "compile"
  end
end