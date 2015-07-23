class Snake
  attr_reader :bones, :lost, :direction
  def initialize(direction = :right)
    @bones = []
    @lost = false
    @direction = direction
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
    new_square = Board.find(head.new_coords)

    if new_square.snake?
      return lose!
    end

    new_square.add_bone_class

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

  def serialize
    {
      bones: @bones.map(&:coords),
      lost: @lost,
      direction: @direction
    }
  end

  def self.initialize_from_hash(hash)
    inst = new
    inst.instance_variable_set(:@lost, hash[:lost] == 'true')
    inst.instance_variable_set(:@direction, hash[:direction].to_sym)
    hash[:bones].reverse_each { |d| Bone.new(inst, Board.find(d)) }
    inst
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
