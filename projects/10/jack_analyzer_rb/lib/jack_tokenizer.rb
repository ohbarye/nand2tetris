class JackTokenizer
  def initialize(path)
    @file = File.open(path, "r")
  end
end