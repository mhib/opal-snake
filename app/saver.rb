module Saver
  module_function

  def save(snake)
    LocalStorage['save_snake'] = snake.serialize.to_json
    LocalStorage['save_board'] = Element.find('#board').html
  end

  def load(snake)
    if LocalStorage['save_snake'].empty? || LocalStorage['save_board'].empty?
      return
    end
    Element.find('#board').html = LocalStorage['save_board']
    LocalStorage.delete('save_board')
    Board.generate
    food = Element.find('.food')
    Board.find(food.data('x').to_i, food.data('y').to_i).food!
    Board.snake = Snake.initialize_from_hash JSON.parse(LocalStorage['save_snake'])
    LocalStorage.delete('save_snake')
  end
end
