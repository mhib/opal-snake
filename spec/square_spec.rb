require 'spec_helper'

describe Square do

  html Views::FIVE
  Board::SIZE = 5
  Board.generate

  describe 'initialize' do
    it 'assigns values' do
      square = Board.find(1, 2)
      expect(square.x).to eq 1
      expect(square.y).to eq 2
      expect(square.snake).to eq false
    end
  end

  describe '#coords' do
    it 'returns coords hash' do
      square = Board.find(1, 3)
      coords = square.coords
      expect(coords[:x]).to eq 1
      expect(coords[:y]).to eq 3
    end
  end

  describe '#add_bone_class' do
    before do
      @square = Board.find(1, 4)
    end

    it 'adds bone class to div' do
      @square.add_bone_class
      expect(@square.div.has_class?('bone')).to eq true
    end

    it 'sets @snake to true' do
      @square.add_bone_class
      expect(@square.snake).to eq true
    end
  end

  describe '#remove_bone_class' do
    before do
      @square = Board.find(1, 4)
      @square.add_bone_class
    end

    it 'removes bone class from div' do
      @square.remove_bone_class
      expect(@square.div.has_class?('bone')).to eq false
    end

    it 'sets @snake to false' do
      @square.remove_bone_class
      expect(@square.snake).to eq false
    end
  end

  describe '#food!' do
    before do
      @square = Board.find(1, 2)
    end

    it 'adds class to div' do
      expect(@square.div).to receive(:add_class).with('food')
      @square.food!
    end

    it 'sets @food to true' do
      @square.food!
      expect(@square.food).to eq true
    end
  end

  describe '#unfood!' do
    before do
      @square = Board.find(1, 2)
      @square.food!
    end

    it 'removes class from div' do
      expect(@square.div).to receive(:remove_class).with('food')
      @square.unfood!
    end

    it 'sets @food to true' do
      @square.unfood!
      expect(@square.food).to eq false
    end
  end

  describe '#head!' do
    before do
      @square = Board.find(1, 3)
    end

    it 'adds head to div' do
      expect(@square.div).to receive(:add_class).with('head')
      @square.head!
    end
  end

  describe '#unhead!' do
    before do
      @square = Board.find(1, 3)
    end

    it 'adds head to div' do
      expect(@square.div).to receive(:remove_class).with('head')
      @square.unhead!
    end
  end
end
