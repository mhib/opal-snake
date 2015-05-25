module Board
  OPPOSITE_DIRECTION = {
    left: :right,
    right: :left,
    up: :down,
    down: :up
  }
  SIZE = 50
  extend self

  def generate
    prepare_matrix
  end

  def find(x, y = 0)
    if Hash === x && y == 0
      return find(x[:x], x[:y])
    end
    return :not_in_board if x < 1 || y < 1
    return :not_in_board if x > SIZE || y > SIZE
    @@matrix[y][x]
  end

  def matrix
    @@matrix
  end

  def part_of_snake?(x, y)
    find(x, y).snake
  end

  def random_square
    squares = []
    @@matrix.each do |row|
      next unless row
      row.each do |square|
        if square && !square.snake
          squares.push square
        end
      end
    end
    squares.sample
  end

  def add_food!
    random_square.food!
  end

  private

  def prepare_matrix
    @@matrix = []
    1.upto(SIZE) do |y|
      @@matrix[y] = []
      1.upto(SIZE) do |x|
        @@matrix[y][x] = Square.new(x, y, Element.find(".square-#{x}-#{y}"))
      end
    end
  end
end
