class Bone
  attr_reader :snake, :square

  MOVES = {
    up: { x: 0, y: -1 },
    down: { x: 0, y: 1 },
    left: { x: -1, y: 0 },
    right: { x: 1, y: 0 }
  }

  def initialize(snake, square)
    @snake = snake
    @square = square
    @square.add_bone_class
    @snake.add_bone(self)
  end

  def coords
    @square.coords
  end

  def new_coords(direction = snake.direction)
    dest = MOVES[direction]
    {
      x: @square.x + dest[:x],
      y: @square.y + dest[:y],
    }
  end

  def head?
    snake.head == self
  end
end
