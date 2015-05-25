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
      expect(@snake.changes).to eq []
      expect(@snake.changes_cache.size).to eq 6
      expect(@snake.changes_cache[0].size).to eq 6
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

  describe '#add_change' do
    before do
      Bone.new(@snake, Board.find(2,2), :up)
    end

    context 'direction same as head' do
      it 'does not add change' do
        @snake.add_change(:up)
        expect(@snake.changes.size).to eq 0
        expect(@snake.changes_cache[2][2]).to eq nil
      end
    end

    context 'direction opposite to head' do
      it 'does not add change' do
        @snake.add_change(:down)
        expect(@snake.changes.size).to eq 0
        expect(@snake.changes_cache[2][2]).to eq nil
      end
    end

    context 'valid direction' do
      context 'new change' do
        it 'adds change' do
          @snake.add_change(:left)
          expect(@snake.changes.size).to eq 1
          expect(@snake.changes_cache[2][2]).not_to eq nil
        end
      end

      context 'change already exists' do
        before do
          @snake.add_change(:left)
        end

        it 'does nothing' do
          expect(@snake.changes.size).to eq 1
          @snake.add_change(:right)
          expect(@snake.changes.size).to eq 1
          expect(@snake.changes_cache[2][2].direction).to eq :left
        end
      end
    end
  end

  describe '#remove_last_change' do
    before do
      Bone.new(@snake, Board.find(2,2), :up)
      @snake.add_change(:right)
    end

    it 'removes last change' do
      expect(@snake.changes.size).to eq 1
      @snake.remove_last_change
      expect(@snake.changes.size).to eq 0
      expect(@snake.changes_cache[2][2]).to eq nil
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
      before do
        @b = Bone.new(@snake, Board.find(2,2), :up)
        @sb = Bone.new(@snake, Board.find(2,3), :up)
      end
      it 'sens messages to all bones' do
        expect(@b).to receive(:move).once
        expect(@b).to receive(:update_square).once
        expect(@sb).to receive(:move).once
        expect(@sb).to receive(:update_square).once
        @snake.move!
        expect(@snake.lost).to eq false
      end
    end

    context 'lost' do
      before do
        @b = Bone.new(@snake, Board.find(2,1), :up)
        @sb = Bone.new(@snake, Board.find(2,2), :up)
      end
      it 'ends ganem' do
        expect(@b).not_to receive(:update_square)
        @snake.move!
        expect(@snake.lost).to eq true
      end
    end
  end

  describe '#eat!' do
    before do
      @b = Bone.new(@snake, Board.find(2,1), :up)
    end

    it 'eats square' do
      expect(Board.find(2,1)).to receive(:unfood!)
      @snake.eat!(Board.find(2, 1))
      h = {}
      h[:square] = Board.find(2, 1)
      h[:direction] = :up
      expect(@snake.to_add).to eq h
    end
  end

  describe '#generate_bone' do
    before do
      @snake.instance_variable_set(
        :@to_add,
        { square: Board.find(2, 1), direction: :up }
      )
    end
    it 'genaretes bone' do
      expect(Board).to receive(:add_food!)
      @snake.send(:generate_bone)
      expect(@snake.bones.size).to eq 1
      expect(@snake.to_add).to eq nil
    end
  end
end
