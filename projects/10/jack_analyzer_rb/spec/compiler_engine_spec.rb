RSpec.describe CompilerEngine do
  describe "#compile" do
    [
      "../ArrayTest/Main.jack",
      "../ExpressionLessSquare/Main.jack",
      "../ExpressionLessSquare/Square.jack",
      "../ExpressionLessSquare/SquareGame.jack",
      "../Square/Main.jack",
      "../Square/Square.jack",
      "../Square/SquareGame.jack",
    ].each do |path|
      it "compiles #{path}" do
        CompilerEngine.new(path).compile
        result = File.open(path.sub(".jack", ".impl.xml")).read
        expected = File.open(path.sub(".jack", ".xml")).read.gsub("\r\n", "\n")
        expect(result).to eq(expected)
      end
    end
  end
end
