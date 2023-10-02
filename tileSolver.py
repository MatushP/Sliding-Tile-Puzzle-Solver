BLANK = 0
UP = 'up'
DOWN = 'down'
LEFT = 'left'
RIGHT = 'right'


class Board:
    def __init__(self, layout, size = 3):
        self.size = size
        self.layout = layout
        self.solved = list(range(1,(size*size)))
        self.solved.append(0)
        
        
    def buildBoard(self, layout):
        board = []
        for i in layout:
            board.append(i)
        
        return board
        
    def printBoard(self):
        board = self.board
        for y in range(self.size): # Iterate over each row.
            for x in range(self.size): # Iterate over each column.
                    print(str(board[y * self.size + x]) + ' ', end='')
            print() # Print a newline at the end of the row.

    def findEmptyTile(self):
        return self.board.index(BLANK)

    def getValidMoves(self, prevMove = None):
        emptyTile = self.findEmptyTile()
        validMoves = []

        emptyTileX = emptyTile % self.size
        emptyTileY = (emptyTile - emptyTileX) / self.size

        if emptyTileY < self.size - 1 and prevMove != DOWN:
            validMoves.append(UP)  

        if emptyTileY > 0 and prevMove != UP:
            validMoves.append(DOWN)

        if emptyTileX < self.size - 1 and prevMove != RIGHT:
            validMoves.append(LEFT)

        if emptyTileX > 0 and prevMove != LEFT:
            validMoves.append(RIGHT)
            

        return self.heuristicFunction(validMoves)
        #return validMoves

    def heuristicFunction(self, moves):
        emptyTile = self.findEmptyTile()

        sortedMoves = []

        for move in moves:
            tile = self.findNeighbouringTile(move)

            #Finds the relative location for the given value of the tile versus its location on the board
            relativePos = self.board[tile] - tile

            #Prioritize moves that push tiles towards their goal location
            if relativePos < 0 and (move == UP or move == LEFT):
                sortedMoves.insert(0, move) 
            elif relativePos > 0 and (move == DOWN or move == RIGHT):
                sortedMoves.insert(0, move) 
            else:
                sortedMoves.append(move)

        return sortedMoves

    def findNeighbouringTile(self,move):
        emptyTile = self.findEmptyTile()

        if move == UP:
            tile = emptyTile + self.size
        elif move == DOWN:
            tile = emptyTile - self.size
        elif move == LEFT:
            tile = emptyTile + 1
        elif move == RIGHT:
            tile = emptyTile - 1

        return tile

    def makeMove(self, move):
        emptyTile = self.findEmptyTile()

        swappedTile = self.findNeighbouringTile(move)

        self.board[emptyTile], self.board[swappedTile] = self.board[swappedTile], self.board[emptyTile]

    def attemptMove(self, moveList, movesRemaining, prevMove):
        # Base Case - Ran out of moves.
        if movesRemaining < 0:
            return False
        
        # Base Case - Solved the puzzle.
        if self.board == self.solved:
            return True
            
        # Recursive Case - Attempt each of the moves from getValidMoves:
        for move in self.getValidMoves(prevMove):
            self.makeMove(move)
            moveList.append(move)

            if self.attemptMove(moveList, movesRemaining - 1, move):
                self.undoMove(move) # Undo's last move
                return True

            # Undo the move to set up for the next move:
            self.undoMove(moveList.pop())

        return False 

    def undoMove(self, move):
        if move == UP:
            self.makeMove(DOWN)
        elif move == DOWN:
            self.makeMove(UP)
        elif move == LEFT:
            self.makeMove(RIGHT)
        elif move == RIGHT:
            self.makeMove(LEFT)


    def solveBoard(self, maxMoves):
        solution = []
        solvedBoard = self.attemptMove(solution, maxMoves, None)

        if solvedBoard:
            self.printBoard()
            for move in solution:
                print()
                print("Move: ", move)
                self.makeMove(move)
                self.printBoard()
                print()

        self.solution = solution
        return solvedBoard

        
        
def readFile():
    path = input("Please enter file path: ")
    try:
        with open(path) as reader:
            values = reader.read().split()
            output = []
            for i in values:
                if i.isdigit():
                    output.append(int(i))
                else:
                    raise Exception("Invalid Input Value")
            return output
    except FileNotFoundError:
        raise Exception("File could not be opened")

def main():
    try:
        input = readFile()
        
        if input != None:
            size = int(input.pop(0))
            
            if size*size == len(input):

                dup = input.copy()  
                for i in input:
                    dup.pop(0)
                    for d in dup:
                        if i == d:
                            raise Exception("Invalid Input - Duplicate Values")

                print(input)
                board = Board(input, size)
                board.board = board.buildBoard(board.layout)
                
                    
                maxMoves = 10
                while(True):
                    print("Attempting with maximum ", maxMoves, " moves.")
                    if board.solveBoard(maxMoves):
                        print("Solved with ", len(board.solution), " moves.")
                        print(board.solution)
                        break
                    
                    maxMoves += 1
            else:
                raise Exception("Invalid Input - Number of Values")
    except Exception as exception:
        print(exception)

            
    
if __name__ == "__main__": # this is for specifc use when runing as a script
    main()
