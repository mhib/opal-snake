class Snake
  attr_reader :bones, :lost, :changes, :changes_cache, :to_add
  def initialize
    @bones = []
    @lost = false
    @changes = []
    @changes_cache = Array.new(Board::SIZE + 1) { Array.new(Board::SIZE + 1, nil) }
    @to_add = nil
  end

  def add_bone(bone)
    @bones << bone
  end

  def add_change(direction)
    if direction == head.direction
      return
    end
    if direction == Board::OPPOSITE_DIRECTION[head.direction]
      return
    end
    if @changes.last && @changes.last.square == head.square
      return
    end
    change = Change.new(head.square, direction)
    @changes << change
    @changes_cache[head.coords[:y]][head.coords[:x]] = change
  end

  def head
    @bones.first
  end

  def last
    @bones.last
  end

  def remove_last_change
    change = @changes.shift
    @changes_cache[change.y][change.x] = nil
  end

  def move!
    @bones.each do |b|
      b.move
      return if @lost
    end
    @bones.each do |b|
      b.update_square
    end
    if @to_add
      generate_bone
    end
  end

  def lose!
    @lost = true
  end

  def eat!(square)
    square.unfood!
    @to_add = { square: last.square, direction: last.direction }
  end

  private

  def generate_bone
    bone = Bone.new(self, @to_add[:square], @to_add[:direction])
    @to_add = nil
    bone.check_for_changes
    Board.add_food!
  end
end
