snake = Snake.new
Document.ready? do
  Board.generate
  Bone.new(snake, Board.find(20, 20), :left)
  Bone.new(snake, Board.find(21, 20), :left)
  Bone.new(snake, Board.find(22, 20), :left)
  Bone.new(snake, Board.find(23, 20), :left)
  Bone.new(snake, Board.find(24, 20), :left)
  Bone.new(snake, Board.find(25, 20), :left)
  Document.on 'keydown' do |e|
    case e.key_code.to_i
    when 38
      e.prevent_default
      snake.add_change(:up)
    when 40
      e.prevent_default
      snake.add_change(:down)
    when 37
      e.prevent_default
      snake.add_change(:left)
    when 39
      e.prevent_default
      snake.add_change(:right)
    else
    end
  end
  snake.move!
  Board.add_food!
  main_loop = every 0.1 do
    snake.move!
    if snake.lost
      main_loop.abort
    elsif snake.won?
      alert "You won"
      mail_loop.abort
    end
  end
end
