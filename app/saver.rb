module Saver
  module_function

  def save(snake)
    LocalStorage['save_snake'] = snake.serialize.to_json
    LocalStorage['save_board'] = Element.find('#board').html
  end

  def load
    if !LocalStorage['save_snake'] || !LocalStorage['save_board']
      return
    end
    Element.find('#board').html = LocalStorage['save_board']
    LocalStorage.delete('save_board')
    Board.generate
    Board.refresh_food
    Board.snake = Snake.initialize_from_hash JSON.parse(LocalStorage['save_snake'])
    LocalStorage.delete('save_snake')
    Board.toggle_pause!
  end
end
