class JackTokenizer
  def initialize(path)
    File.open(path, "r") do |file|
      @content = file.read
    end
    @tokens = tokenize
    advance
  end

  def current_token
    @current_token
  end

  def advance
    if has_more_tokens?
      @current_token = @tokens.shift
    else
      raise "No more tokens"
    end
  end

  def next_token
    @tokens.first
  end

  def has_more_tokens?
    !@current_token.nil? || !@tokens.empty?
  end

  def token_type
    case current_token
    in token if KEYWORDS.key(token)
      KEYWORD
    in token if SYMBOLS.key(token)
      SYMBOL
    in INT_CONST_REGEX if Integer(token) <= MAX_INT
      INT_CONST
    in IDENTIFIER_REGEX
      IDENTIFIER
    in STRING_CONST_REGEX
      STRING_CONST
    end
  end

  def int_val
    if token_type == INT_CONST
      Integer(current_token)
    else
      raise "current token is not INT_CONST"
    end
  end

  def string_val
    if token_type == STRING_CONST
      current_token[1..-2]
    else
      raise "current token is not STRING_CONST"
    end
  end

  def tokenize
    lines = without_multiline_comment(@content)
      .split("\n")
      .map{|line| line.split("//").first.to_s.strip } # remove single line comment
      .reject(&:empty?)
    tokenize_all_lines lines, []
  end

  def without_multiline_comment(content)
    if content.include? "/*"
      from = content.index("/*")
      to = content.index("*/") + 2
      without_multiline_comment content[0...from] + content[to..]
    else
      content
    end
  end

  def tokenize_all_lines(lines, tokens)
    case lines
    in []
      tokens
    in line, *rest
      tokenized = tokenize_line(line, tokens)
      tokenize_all_lines rest, tokenized
    end
  end

  def before_double_quote?(line)
    dq_index = line.index("\"")
    space_index = line.index(" ")
    dq_index && space_index && dq_index < space_index
  end

  def tokenize_line(line, tokens)
    case line
    in ""
      tokens
    in line if line.start_with? "\""
      index = line.index("\"", 1)
      rest_line = line[index+1..]
      tokens << line[0..index]
      tokenize_line rest_line, tokens
    in line if before_double_quote? line
      index = line.index("\"")
      unit = line[0...index]
      rest_line = line[index..].strip
      tokenize_line rest_line, (tokenize_unit unit, tokens)
    in line if line.include? " "
      index = line.index(" ")
      unit = line[0...index]
      rest_line = line[index+1..].strip
      tokenize_line rest_line, (tokenize_unit unit, tokens)
    in unit
      tokenize_line "", (tokenize_unit unit, tokens)
    end
  end

  def tokenize_unit(unit, tokens)
    case unit
    in ""
      tokens
    in unit if index = unit.index(SYMBOL_REGEX)
      if index != 0
        tokens << unit[0...index]
      end
      rest_unit = unit[index+1..]
      tokens << unit[index]
      tokenize_unit rest_unit, tokens
    in unit
      tokens << unit
      tokenize_unit "", tokens
    end
  end
end