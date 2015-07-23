module Board
  OPPOSITE_DIRECTION = {
    left: :right,
    right: :left,
    up: :down,
    down: :up
  }
  SIZE = 50
  NUMBER_OF_CELLS = SIZE ** 2

  extend self

  def generate
    prepare_matrix
  end

  def find(x, y = nil)
    if Hash === x && y == nil
      return find(x[:x], x[:y])
    end
    x = transform(x)
    y = transform(y)
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
    square = random_square
    if square
      square.food!
    end
  end

  def paused?
    @paused ||= false
  end

  def toggle_pause!
    @paused = !@paused
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

  def transform(val)
    if val == 0
      return SIZE
    end
    if val > SIZE
      return val - SIZE
    end
    val
  end
end
