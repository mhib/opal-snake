require 'spec_helper'

describe Board do
  #describe 'reset_snake_cache' do
    #it 'generates cache' do
      #Board.reset_snake_cache
      #snake_cache = Board.snake_cache
      #false_proc = -> n { n == false }
      #expect(
        #snake_cache.inject(0) { |m, i| m + i.count(&false_proc) }
      #).to eq Board::SIZE ** 2
    #end
  #end

  describe 'generate' do
    Board::SIZE = 5
    html Views::FIVE

    before do
      Board.generate
    end

    it 'prepares matrix' do
      square = Board.find(1, 1)
      expect(square.x).to eq 1
      expect(square.y).to eq 1
    end
  end

  describe 'find' do
    Board::SIZE = 5
    html Views::FIVE

    before do
      Board.generate
    end

    context 'in board' do
      it 'finds square' do
        s = Board.find(3, 4)
        expect(s.x).to eq 3
        expect(s.y).to eq 4
      end
    end

    context 'not in board' do
      context 'less than 1' do
        it 'returns :not_in_board' do
          s = Board.find(1, 0)
          expect(s).to eq :not_in_board
        end
      end

      context 'more than SIZE' do
        it 'returns :not_in_board' do
          s = Board.find(6, 2)
          expect(s).to eq :not_in_board
        end
      end
    end

    context 'hash as param' do
      it 'finds square' do
        s = Board.find({ y: 2, x: 1 })
        expect(s.y).to eq 2
        expect(s.x).to eq 1
      end
    end
  end
end
