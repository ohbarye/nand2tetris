require_relative "./jack_tokenizer.rb"

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
    @tokenizer.advance
    write_element "class" do
      write_element "keyword", value: CLASS
      compile_identifier
      compile_symbol

      while CLASS_VAR_DEC_KEYWORDS.include? @tokenizer.current_token
        # TODO
      end

      while SUBROUTINE_DEC_KEYWORDS.include? @tokenizer.current_token
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
      while @tokenizer.current_token == VAR
        compile_var_dec
      end
      compile_statements
      compile_symbol # }
    end
  end

  def compile_statements
    write_element "statements" do
      while STATEMENT_KEYWORDS.include? @tokenizer.current_token
        compile_statement
      end
    end
  end

  def compile_statement
    case @tokenizer.current_token
    in LET
      compile_let_statement
    in IF
      compile_let_statement
    in WHILE
      compile_while_statement
    in DO
      compile_do_statement
    in RETURN
      compile_return_statement
    end
  end

  def compile_let_statement
    write_element "letStatement" do
      compile_keyword # let
      compile_identifier # varName
      if @tokenizer.current_token == LEFT_SQUARE_BRACKET
        compile_symbol # [
        compile_expression
        compile_symbol # ]
      end
      compile_symbol # =
      compile_expression
      compile_symbol # ;
    end
  end

  def compile_while_statement
    write_element "whileStatement" do
      compile_keyword # while
      compile_symbol # (
      compile_expression
      compile_symbol # )
      compile_symbol # {
      compile_statements
      compile_symbol # }
    end
  end

  def compile_do_statement
    write_element "doStatement" do
      compile_keyword # do
      compile_subroutine_call
      compile_symbol # ;
    end
  end

  def compile_return_statement
    write_element "returnStatement" do
      compile_keyword # return
      while @tokenizer.current_token != SEMI_COLON
        compile_expression
      end
      compile_symbol # ;
    end
  end

  def compile_subroutine_call
    compile_identifier
    if @tokenizer.current_token == PERIOD
      compile_symbol # .
      compile_identifier
    end
    compile_symbol # (
    compile_expression_list
    compile_symbol # )
  end

  def compile_expression
    write_element "expression" do
      compile_term
      while [PLUS_SIGN, HYPHEN, ASTERISK, SLASH, AMPERSAND, VERTICAL_LINE, LESS_THAN_SIGN, GREATER_THAN_SIGN, EQUAL].include? @tokenizer.current_token
        compile_symbol
        compile_term
      end
    end
  end

  def compile_term
    write_element "term" do
      case @tokenizer.token_type
      in INT_CONST
        compile_integer_constant
      in STRING_CONST
        compile_string_constant
      in KEYWORD if [BOOLEAN_TRUE, BOOLEAN_FALSE, NULL, THIS].include? @tokenizer.current_token
        compile_keyword
      in IDENTIFIER
        compile_identifier
        if @tokenizer.current_token == LEFT_SQUARE_BRACKET
          compile_symbol # [
          compile_expression
          compile_symbol # ]
        elsif @tokenizer.current_token == LEFT_ROUND_BRACKET
          compile_symbol # (
          compile_expression_list
          compile_symbol # )
        elsif @tokenizer.current_token == PERIOD
          compile_symbol # .
          compile_identifier
          compile_symbol # (
          compile_expression_list
          compile_symbol # )
        end
      in SYMBOL if @tokenizer.current_token == LEFT_ROUND_BRACKET
        compile_symbol # (
        compile_expression
        compile_symbol # )
      in SYMBOL if [HYPHEN, TILDE].include? @tokenizer.current_token
        compile_symbol # -|~
        compile_term
      else
        # TODO raise "Unexpected token is given to compile_term: #{@tokenizer.current_token}"
      end
    end
  end

  def compile_expression_list
    write_element "expressionList" do
      while @tokenizer.current_token != RIGHT_ROUND_BRACKET
        compile_expression
        while @tokenizer.current_token == COMMA
          compile_symbol
          compile_expression
        end
      end
    end
  end

  def compile_integer_constant
    write_element "integerConstant", value: @tokenizer.int_val
    @tokenizer.advance
  end

  def compile_string_constant
    write_element "stringConstant", value: @tokenizer.string_val
    @tokenizer.advance
  end

  def compile_var_dec
    write_element "varDec" do
      compile_keyword # var
      compile_type
      compile_identifier # varName
      while @tokenizer.current_token == COMMA
        compile_symbol # ,
        compile_identifier # varName
      end
      compile_symbol # ;
    end
  end

  def compile_type
    if [INT, CHAR, BOOLEAN].include? @tokenizer.current_token
      compile_keyword
    else
      compile_identifier
    end
  end

  def compile_keyword
    write_element "keyword", value: @tokenizer.current_token
    @tokenizer.advance
  end

  def compile_identifier
    write_element "identifier", value: @tokenizer.current_token
    @tokenizer.advance
  end

  def compile_symbol
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
    @tokenizer.advance
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
