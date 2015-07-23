module KeyboardHandler
  MOVEMENTS = {
    # arrows
    38 => :up,
    40 => :down,
    37 => :left,
    39 => :right,
    # vim
    75 => :up,
    74 => :down,
    72 => :left,
    76 => :right,
    # wsad
    87 => :up,
    83 => :down,
    65 => :left,
    68 => :right,
    # numpad
    104 => :up,
    98 => :down,
    100 => :left,
    102 => :right
  }

  PAUSE = {
    80 => true
  }

  SAVE = {
    122 => true
  }

  LOAD = {
    123 => true
  }

  module_function

  def handle(snake, e)
    kcode = e.key_code.to_i
    if (direction = MOVEMENTS[kcode])
      Board.toggle_pause! if Board.paused?
      move_snake(snake, e, direction)
    elsif PAUSE.key? kcode
      Board.toggle_pause!
    elsif SAVE.key? kcode
      e.prevent_default
      Saver.save(snake)
    elsif LOAD.key? kcode
      e.prevent_default
      Saver.load
    end
  end

  def move_snake(snake, e, direction)
    e.prevent_default
    snake.direction = direction
  end
end
