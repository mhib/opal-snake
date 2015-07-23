Document.ready? do
  Board.snake = Snake.new
  Board.generate
  Bone.new(Board.snake, Board.find(20, 20))
  Bone.new(Board.snake, Board.find(21, 20))
  Bone.new(Board.snake, Board.find(22, 20))
  Bone.new(Board.snake, Board.find(23, 20))
  Bone.new(Board.snake, Board.find(24, 20))
  Bone.new(Board.snake, Board.find(25, 20))
  Document.on 'keydown' do |e|
    KeyboardHandler.handle(Board.snake, e)
  end
  Board.snake.move!
  Board.add_food!
  main_loop = every 0.1 do
    next if Board.paused?
    Board.snake.move!
    if Board.snake.lost
      main_loop.abort
    elsif Board.snake.won?
      alert "You won"
      Board.snake.check_if_record
      mail_loop.abort
    end
  end
end
