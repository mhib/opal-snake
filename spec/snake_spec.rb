require 'spec_helper'

describe Snake do
  html Views::FIVE
  Board::SIZE = 5
  Board.generate

  before do
    @snake = Snake.new
  end

  describe 'initialize' do
    it 'assigns values' do
      expect(@snake.bones).to eq []
      expect(@snake.lost).to eq false
    end
  end

  context '#add_bone' do
    it 'adds bone' do
      b = Bone.new(Snake.new, Board.find(2, 2), :up)
      b.instance_variable_set(:@snake, @snake)
      expect(@snake.bones.size).to eq 0
      @snake.add_bone(b)
      expect(@snake.bones.size).to eq 1
    end
  end

  describe '#lose!' do
    it 'losts' do
      expect(@snake.lost).to eq false
      @snake.lose!
      expect(@snake.lost).to eq true
    end
  end

  describe '#move!' do
    context 'no lost' do
      context 'eat' do
        html Views::FIVE
        before do
          Board.generate
          @snake = Snake.new
          @b = Bone.new(@snake, Board.find(1,1))
          @sb = Bone.new(@snake, Board.find(2,1))
          Board.find(3,1).food!
        end

        it 'moves and eats square' do
          expect(@b.square).not_to receive(:unbone!)
          @snake.move!
          expect(@snake.lost).to eq false
          expect(@snake.bones.size).to eq 3
        end
      end

      context 'no eat' do
        html Views::FIVE
        before do
          Board.generate
          @snake = Snake.new
          @b = Bone.new(@snake, Board.find(1,1))
          @sb = Bone.new(@snake, Board.find(2,1))
        end

        it 'moves' do
          expect(@b.square).to receive(:unbone!).once
          @snake.move!
          expect(@snake.lost).to eq false
          expect(@snake.bones.size).to eq 2
        end
      end
    end

    context 'lost' do
      before do
        @b = Bone.new(@snake, Board.find(2,2))
        @sb = Bone.new(@snake, Board.find(2,3))
        @snake.direction = :up
      end

      it 'ends gane' do
        @snake.move!
        expect(@snake.lost).to eq true
      end
    end
  end

  describe '#direction=' do
    subject { Snake.new }

    #
    # snake have :right direction by default
    #

    context 'valid direction' do
      it 'changes direction' do
        subject.direction = :up
        expect(subject.direction).to eq :up
      end
    end

    context 'invalid direction' do
      it 'does not change direction' do
        subject.direction = :left
        expect(subject.direction).not_to eq :left
      end
    end
  end

  describe '#pop_last' do
    before do
      @snake = Snake.new
      Bone.new(@snake, Board.find(2,1))
      Bone.new(@snake, Board.find(1,1))
    end

    it 'removes last bone' do
      expect(@snake.bones.size).to eq 2
      @snake.send :pop_last
      expect(@snake.bones.size).to eq 1
    end
  end
end
