class Snake
  attr_reader :bones, :lost, :direction
  def initialize
    @bones = []
    @lost = false
    @changes = []
    @to_add = nil
    @direction = :right
  end

  def add_bone(bone)
    head.square.unhead! if head
    @bones.unshift bone
    head.square.head!
  end

  def direction=(dir)
    @direction = dir if valid_direction?(dir)
  end

  def head
    @bones.first
  end

  def move!
    eaten = false
    new_square = Board.find(head.new_coords(@direction))

    if new_square.snake?
      return lose!
    end

    if new_square.food
      new_square.unfood!
      eaten = true
      Board.add_food!
    end

    Bone.new(self, new_square)

    pop_last unless eaten
  end

  def lose!
    @lost = true
    check_if_record
  end

  def won?
    @bones.size == Board::NUMBER_OF_CELLS
  end

  def check_if_record
    result = @bones.length
    if (r = LocalStorage['record'].to_i) && r < result
      alert "#{result} points!\nNew record!"
      LocalStorage['record'] = result
    end
  end

  private

  def pop_last
    bone = @bones.pop
    bone.square.remove_bone_class
  end

  def valid_direction?(dir)
    if dir == @direction
      return false
    end
    if dir == Board::OPPOSITE_DIRECTION[@direction]
      return false
    end
    true
  end
end
