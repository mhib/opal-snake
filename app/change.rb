class Change
  attr_reader :square, :direction
  def initialize(square, direction)
    @square = square
    @direction = direction
  end

  def x
    @square.x
  end

  def y
    @square.y
  end

  def coords
    @square.coords
  end
end
