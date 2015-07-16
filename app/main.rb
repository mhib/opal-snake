snake = Snake.new
Document.ready? do
  Board.generate
  Bone.new(snake, Board.find(20, 20))
  Bone.new(snake, Board.find(21, 20))
  Bone.new(snake, Board.find(22, 20))
  Bone.new(snake, Board.find(23, 20))
  Bone.new(snake, Board.find(24, 20))
  Bone.new(snake, Board.find(25, 20))
  Document.on 'keydown' do |e|
    case e.key_code.to_i
    when 38
      e.prevent_default
      snake.direction = :up
    when 40
      e.prevent_default
      snake.direction = :down
    when 37
      e.prevent_default
      snake.direction = :left
    when 39
      e.prevent_default
      snake.direction = :right
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
      snake.check_if_record
      mail_loop.abort
    end
  end
end
