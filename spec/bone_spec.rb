require 'spec_helper'

describe Bone do
  html Views::FIVE
  Board::SIZE = 5
  Board.generate

  before do
    @snake = Snake.new
    @bone = Bone.new(@snake, Board.find(2,2))
  end

  describe 'initialize' do
    context 'head' do
      it 'assigns values' do
        expect(@snake.bones).to eq [@bone]
        expect(@bone.head?).to eq true
        expect(@bone.square.snake).to eq true
      end
    end

    context 'not head' do
      before do
        @second_bone = Bone.new(@snake, Board.find(3, 2))
      end

      it 'assigns values' do
        expect(@snake.bones).to eq [@second_bone, @bone]
        expect(@bone.head?).to eq false
        expect(@second_bone.square.snake).to eq true
      end
    end
  end

  describe '#coords' do
    it "is equal to square's coords" do
      expect(@bone.coords).to eq @bone.square.coords
    end
  end

end
