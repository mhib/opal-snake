class Bone
  attr_reader :snake, :square, :direction

  MOVES = {
    up: { x: 0, y: -1 },
    down: { x: 0, y: 1 },
    left: { x: -1, y: 0 },
    right: { x: 1, y: 0 }
  }

  def initialize(snake, square, direction)
    @snake = snake
    @square = square
    @square.add_bone_class
    @direction = direction
    @old_square = nil
    if snake.bones.empty?
      @head = true
      @square.div.add_class('head')
    else
      @head = false
    end
    @snake.add_bone(self)
  end

  def move
    check_for_changes
    @old_square = @square
    @square = Board.find(new_coords)
    if @head && @square.snake && @square != snake.last.square
      @snake.lose!
    elsif @head && @square.food
      @snake.eat!(@square)
    end
  end

  def update_square
    @square.add_bone_class
    if @old_square && self == @snake.last
      @old_square.remove_bone_class
    end
    if @head
      change_head!
    end
  end

  def coords
    @square.coords
  end

  def new_coords
    dest = MOVES[@direction]
    {
      x: @square.x + dest[:x],
      y: @square.y + dest[:y],
    }
  end

  def head?
    @head
  end

  def check_for_changes
    if c = @snake.changes_cache[square.y][square.x]
      @direction = c.direction
      if self == @snake.last && !@snake.to_add
        @snake.remove_last_change
      end
    end
  end

  private

  def change_head!
    @old_square.unhead!
    @square.head!
  end
end
