require_relative "./compiler_engine.rb"

class JackAnalyzer
  def initialize(target)
    path = File.expand_path(target)
    unless File.exist? path
      raise "Target is neither a file nor a directory: #{path}"
    end

    if path.end_with? ".jack"
      @targets = [path]
    else
      @targets = Dir.glob("#{path}/*").select{ |path| path.end_with? ".jack" }
    end
  end

  def run
    @targets.each do |path|
      puts "Compiling #{path}"
      CompilerEngine.new(path).compile
      puts "Finished compiling #{path}"
    end
  end
end
