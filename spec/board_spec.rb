require 'spec_helper'

describe Board do
  Board::SIZE = 5
  html Views::FIVE
  describe 'generate' do
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
        it 'returns end of board' do
          s = Board.find(1, 0)
          expect(s).to eq Board.find(1, 5)
        end
      end

      context 'more than SIZE' do
        it 'returns beginning of board' do
          s = Board.find(6, 2)
          expect(s).to eq Board.find(1, 2)
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

  describe 'random_square' do
    before do
      Board.generate
    end
    it 'selects random square' do
      square = Board.random_square
      expect(square.class).to eq Square
    end
  end

  describe 'add_food!' do
    before do
      Board.generate
    end
    it 'adds food' do
      Board.add_food!
      expect(Element.find('.food').length).not_to eq 0
    end
  end
end
