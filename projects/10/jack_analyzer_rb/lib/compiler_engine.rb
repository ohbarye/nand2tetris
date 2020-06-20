require_relative "./jack_tokenizer.rb"
require_relative "./constants.rb"

class CompilerEngine
  def initialize(path)
    @outfile = File.open(path.sub(".jack", ".impl.xml"), "w")
    @tokenizer = JackTokenizer.new(path)
    @indention = 0
  end

  def compile
    compile_class
    @outfile.close
  end

  private

  def compile_class
    write_element "class" do
      write_element "keyword", value: CLASS
      compile_identifier
      compile_symbol

      while CLASS_VAR_DEC_KEYWORDS.include? @tokenizer.next_token
        # TODO
      end

      while SUBROUTINE_DEC_KEYWORDS.include? @tokenizer.next_token
        compile_subroutine_dec
      end

      compile_symbol
    end
  end

  def compile_subroutine_dec
    write_element "subroutineDec" do
      compile_keyword # constructor | function | method
      compile_keyword # void | type
      compile_identifier # subroutineName
      compile_symbol # (
      compile_parameter_list
      compile_symbol # )
      compile_subroutine_body
    end
  end

  def compile_parameter_list
    write_element "parameterList" do
    end
  end

  def compile_subroutine_body
    write_element "subroutineBody" do
      compile_symbol # {
      while @tokenizer.next_token == VAR
        compile_var_dec
      end
      compile_statements
      compile_symbol # }
    end
  end

  def compile_statements
    write_element "statements" do
      while STATEMENT_KEYWORDS.include? @tokenizer.next_token
        compile_statement
      end
    end
  end

  def compile_statement
    case @tokenizer.next_token
    in LET
      compile_let_statement
    in IF
      compile_let_statement
    in WHILE
      compile_let_statement
    in DO
      compile_let_statement
    in RETURN
      compile_let_statement
    end
  end

  def compile_let_statement
    write_element "letStatement" do
      compile_keyword # let
      compile_identifier # varName
      if @tokenizer.next_token == LEFT_SQUARE_BRACKET
        compile_symbol # [
        compile_expression
        compile_symbol # ]
      end
      compile_symbol # =
      compile_expression
      compile_symbol # ;
    end
  end

  def compile_expression
    write_element "expression" do
      compile_term
    end
  end

  def compile_term
    write_element "term" do
      # TODO
    end
  end

  def compile_var_dec
    write_element "varDec" do
      compile_keyword # var
      compile_type
      compile_identifier # varName
      while @tokenizer.next_token == COMMA
        compile_symbol # ,
        compile_identifier # varName
      end
      compile_symbol # ;
    end
  end

  def compile_type
    if [INT, CHAR, BOOLEAN].include? @tokenizer.next_token
      compile_keyword
    else
      compile_identifier
    end
  end

  def compile_keyword
    @tokenizer.advance
    write_element "keyword", value: @tokenizer.current_token
  end

  def compile_identifier
    @tokenizer.advance
    write_element "identifier", value: @tokenizer.current_token
  end

  def compile_symbol
    @tokenizer.advance
    value = case @tokenizer.current_token
      in LESS_THAN_SIGN
        "&lt;"
      in GREATER_THAN_SIGN
        "&gt;"
      in AMPERSAND
        "&amp;"
      in s
        s
      end
    write_element "symbol", value: value
  end

  def write_element(element_name, value: nil, &block)
    if block_given?
      @outfile << "#{'  ' * @indention}<#{element_name}>\n"
      @indention += 1
      yield
      @indention -= 1
      @outfile << "#{'  ' * @indention}</#{element_name}>\n"
    else
      @outfile << "#{'  ' * @indention}<#{element_name}> #{value} </#{element_name}>\n"
    end
  end
end
