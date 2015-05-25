class Square
  attr_reader :x, :y, :div
  attr_accessor :snake, :food
  def initialize(x, y, div = nil)
    @x = x
    @y = y
    @div = div
    @snake = false
    @food = false
  end

  def snake?
    @snake
  end

  def coords
    { x: x, y: y }
  end

  def add_bone_class
    @div.add_class('bone')
    @snake = true
  end

  def remove_bone_class
    @div.remove_class('bone')
    @snake = false
  end

  def food!
    @div.add_class('food')
    @food = true
  end

  def unfood!
    @div.remove_class('food')
    @food = false
  end

  def head!
    @div.add_class('head')
  end

  def unhead!
    @div.remove_class('head')
  end
end
