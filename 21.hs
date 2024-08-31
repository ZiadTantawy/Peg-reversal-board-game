type Position = (Int,Int)
data Color = W | B deriving (Eq, Show)
data Peg = Peg Position Color deriving (Eq, Show)
data Move = M Position deriving (Eq, Show)
type Board = [Peg]
data State = S Move Board deriving (Eq, Show)

allvalidPositions :: [Position]
allvalidPositions = [(x, y) | x <- [-3..3], y <- [-3..3], validPosition (x, y)]

validPosition :: Position -> Bool
validPosition (x, y) =(((abs x == 3 || abs x == 2)&& abs y<=1)||((abs y == 3 || abs y == 2)&& abs x<=1))||(abs x<= 1 && abs y<=1)

createBoard :: Position -> Board
createBoard pos
    | validPosition pos = [Peg (x, y) (if (x, y) == pos then W else B) | x <- [-3..3], y <- [-3..3], validPosition (x, y)]
    | otherwise = error "The position is not valid."
	
isAdjacentW :: Peg -> Board -> Bool
isAdjacentW (Peg (x,y) color) board = if (isBlack (x,y) board) && (isWhite (x, y+1) board || isWhite (x, y-1) board || isWhite (x+1, y) board || isWhite (x-1, y) board) then True else False

isWhite :: Position -> Board -> Bool
isWhite _ [] = False
isWhite (x,y) ((Peg (xp,yp) color):rest) = if x==xp && y==yp && color == W then True else (isWhite (x,y) rest)

isBlack :: Position -> Board -> Bool
isBlack _ [] = False
isBlack (x,y) ((Peg (xp,yp) color):rest) = if x==xp && y==yp && color == B then True else (isBlack (x,y) rest)

isValidMove :: Move -> Board -> Bool
isValidMove (M pos) board = (validPosition pos) && (isBlack pos board) && (isAdjacentW (Peg pos B) board)

isGoal:: Board -> Bool
isGoal [] = True
isGoal (Peg pos color:rest) = isWhite pos (Peg pos color:rest) && isGoal rest

showPossibleNextStates :: Board -> [State]
showPossibleNextStates board
    | isGoal board = error "No Possible States Exist."
    | otherwise = [S (M pos) (applyMove (M pos) board) | pos <- allvalidPositions, isValidMove (M pos) board]

applyMove :: Move -> Board -> Board
applyMove (M pos) board = map (\(Peg p c) -> if p == pos then Peg p W else Peg p c) board



