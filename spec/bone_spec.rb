require 'spec_helper'

describe Bone do
  html Views::FIVE
  Board::SIZE = 5
  Board.generate

  before do
    @snake = Snake.new
    @bone = Bone.new(@snake, Board.find(2,2), :up)
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
        @second_bone = Bone.new(@snake, Board.find(3, 2), :up)
      end

      it 'assigns values' do
        expect(@snake.bones).to eq [@bone, @second_bone]
        expect(@second_bone.head?).to eq false
        expect(@second_bone.square.snake).to eq true
      end
    end
  end

  describe '#coords' do
    it "is equal to square's coords" do
      expect(@bone.coords).to eq @bone.square.coords
    end
  end

  describe '#check_for_changes' do
    context 'no change' do
      it 'does nothing' do
        direction = @bone.direction
        @bone.send(:check_for_changes)
        expect(@bone.direction).to eq direction
      end
    end

    context 'change' do
      before do
        @snake.add_change(:left)
      end
      it 'changes direction' do
        direction = @bone.direction
        @bone.send(:check_for_changes)
        expect(@bone.direction).not_to eq direction
      end

      context 'last' do
        context '@snake.to_add' do
          before do
            @snake.instance_variable_set(:@to_add, { square: Board.find(1, 3), direction: :up })
          end
          it 'does not remove change from snake' do
            expect(@snake.changes.size).to eq 1
            @bone.send(:check_for_changes)
            expect(@snake.changes.size).to eq 1
          end
        end

        context 'no @snake.to_add' do
          it 'removes change from snake' do
            # change syntax does not work :C
            expect(@snake.changes.size).to eq 1
            @bone.send(:check_for_changes)
            expect(@snake.changes.size).to eq 0
          end
        end
      end

      context 'not last' do
        before do
          @second_bone = Bone.new(@snake, Board.find(3, 2), :up)
        end

        it 'does not remove change from snake' do
          expect(@snake.changes.size).to eq 1
          @bone.send(:check_for_changes)
          expect(@snake.changes.size).to eq 1
        end
      end
    end

    describe '#move' do
      context 'normal move' do
        it 'moves' do
          square = @bone.square
          @bone.move
          expect(@bone.instance_variable_get(:@old_square)).to eq square
          expect(@bone.square).to eq Board.find(2, 1)
        end
      end

      context 'head eats tail' do
        before do
          # Yep snake is not in one piece
          Bone.new(@snake, Board.find(2,1), :left)
          Bone.new(@snake, Board.find(3,2), :up)
        end

        it 'looses' do
          @bone.move
          expect(@snake.lost).to eq true
        end
      end

      context 'head touches tail' do
        before do
          Bone.new(@snake, Board.find(3,2), :up)
          # Yep snake is not in one piece AGAIN
          Bone.new(@snake, Board.find(2,1), :left)
        end

        it 'moves' do
          @bone.move
          expect(@snake.lost).to eq false
        end
      end
    end
  end

  describe '#update_square' do
    context 'head' do
      before do
        @bone.move
      end

      it 'updates square' do
        expect(@bone.square).to receive(:add_bone_class)
        expect(@bone.square.div).to receive(:add_class).with('head')
        expect(@bone.instance_variable_get(:@old_square).div).to(
          receive(:remove_class).twice
        )
        @bone.update_square
      end
    end

    context 'not head' do
      before do
        @s_bone = Bone.new(@snake, Board.find(3,2), :up)
        @bone.move
        @s_bone.move
      end

      it 'updates square' do
        expect(@s_bone.square).to receive(:add_bone_class)
        expect(@s_bone.square.div).not_to receive(:add_class)
        expect(@s_bone.instance_variable_get(:@old_square).div).to(
          receive(:remove_class).once
        )
        @s_bone.update_square
      end
    end
  end

  describe '#change_head!' do
    before do
      @square = double('square')
      @s_square = double('square')
      @bone.instance_variable_set(:@square, @square)
      @bone.instance_variable_set(:@old_square, @s_square)
    end

    it 'changes square head' do
      expect(@square).to receive(:head!).once
      expect(@s_square).to receive(:unhead!).once
      @bone.send(:change_head!)
    end
  end

end
