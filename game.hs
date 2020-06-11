module Game where
import Data.List

data Cell = Null | P | C  deriving (Eq)
instance Show Cell where
   show (Null) = "_"
   show (P) = "p"
   show (C) = "c"

type Matrix a = [[a]]
data Board = Board (Matrix Cell)
instance Show Board where
   show (Board (row:restRows)) = (showRow row) ++ "\n" ++ (show (Board restRows))
   show (Board []) = ""

showRow :: [Cell] -> String
showRow [] = ""
showRow (head:tail) = show head ++ " " ++ showRow tail

generateBoard :: Integer -> Board
generateBoard size = (Board  [ [Null | x <- [1..size]] | y <- [1..size]])


exampleBoard = Board [[Null,P,P,P,P,P,Null,C,P,P],[P,C,C,Null,Null,Null,Null,C,P,P],[Null,P,C,C,Null,Null,Null,Null,C,C]]

--linie, coloana, val, tabla
replaceValBoard :: Integer -> Integer -> Cell -> Board -> Board
replaceValBoard 0 x value (Board (row:restRows)) = (Board ((replaceValRow x value row) : restRows))
replaceValBoard n x value (Board (row:restRows)) = 
    if n < 0 
	then (Board (row:restRows))
	else case replaceValBoard (n-1) x value (Board restRows) of
	  Board (matrix) -> (Board (row:matrix))

--poz, valoare, lista in care se face replace
replaceValRow :: Integer -> Cell -> [Cell]-> [Cell] 
replaceValRow 0 value (head:tail) = value:tail
replaceValRow n value (head:tail) = 
    if n < 0 
	then (head:tail)
	else head: replaceValRow (n-1) value tail

getValBoard :: Integer -> Integer -> Board -> Cell
getValBoard n x (Board [[]]) = Null
getValBoard 0 x (Board (row:restRows)) = if x<0 || x>9 then 
                                           Null
										 else getValRow x row
getValBoard n x (Board (row:restRows)) = if n<0 || n>9 || x<0 || x>9 then
                                           Null
                                         else getValBoard (n-1) x (Board restRows)

getValRow :: Integer -> [Cell] -> Cell
getValRow 0 [] = Null
getValRow 0 (head:tail) = head
getValRow n (head:tail) = getValRow (n-1) tail

checkClearPos :: Integer -> Integer -> Board -> Bool
checkClearPos x y board = case getValBoard x y board of 
  Null -> True
  P -> False
  C -> False

type Pattern   = [Cell]

patterns :: Board -> [Pattern]
patterns (Board board) = board ++ (transpose board)

winner :: Board -> Cell
winner board
  | any (isInfixOf [C, C, C, C, C]) (patterns board) = C
  | any (isInfixOf [P, P, P, P, P]) (patterns board) = P
  | winDiag board 0 0 == P = P
  | winDiag board 0 0 == C = C
  | otherwise = Null


winDiag :: Board -> Integer -> Integer -> Cell
winDiag board l c = case getValBoard l c board of
  Null -> if c < 10 then winDiag board l (c+1) 
		            else if l < 10 then 
					       winDiag board (l+1) 0
					     else Null
  P -> if (checkFiveDiagRight board l c P) == P then 
         P
       else if (checkFiveDiagLeft board l c P) == P then
              P
            else if c < 10 then winDiag board l (c+1) 
		            else if l < 10 then 
					       winDiag board (l+1) 0
					     else Null			  
  C -> if (checkFiveDiagRight board l c C) == C then 
         P
       else if (checkFiveDiagLeft board l c C) == C then
              C
            else if c < 10 then winDiag board l (c+1) 
		            else if l < 10 then 
					       winDiag board (l+1) 0
					     else Null


checkFiveDiagRight :: Board -> Integer -> Integer -> Cell -> Cell
checkFiveDiagRight board l c val = let val1 = getValBoard (l+1) (c+1) board 
                                       val2 = getValBoard (l+2) (c+2) board
                                       val3 = getValBoard (l+3) (c+3) board
                                       val4 = getValBoard (l+4) (c+4) board  in 
                                      if val == val1 && val == val2 && val == val3 && val == val4 then 
									   val
								   	  else Null

checkFiveDiagLeft :: Board -> Integer -> Integer -> Cell -> Cell
checkFiveDiagLeft board l c val = let val1 = getValBoard (l+1) (c-1) board 
                                      val2 = getValBoard (l+2) (c-2) board
                                      val3 = getValBoard (l+3) (c-3) board
                                      val4 = getValBoard (l+4) (c-4) board  in 
                                     if val == val1 && val == val2 && val == val3 && val == val4 then 
									  val
								     else Null
	
--scor linie	
getInRowLeft :: Board -> Cell -> Integer -> Integer -> Integer -> Integer
getInRowLeft board player l c score = let left = getValBoard l (c-1) board in
										if left == player then 
										  getInRowLeft board player l (c-1) (score+1)
										else if (c-1) < 0 || left /= Null then
										       (-1) * score
									    else score
getInRowRight :: Board -> Cell -> Integer -> Integer -> Integer -> Integer
getInRowRight board player l c score = let right = getValBoard l (c+1) board in
										 if right == player then 
										   getInRowRight board player l (c+1) (score+1)
										else if (c+1) > 9 ||  right /= Null then
										       (-1) * score
									    else score
									 
getScorePosRow :: Board -> Cell -> Integer -> Integer -> Integer-> Integer
getScorePosRow board player l c score = let scoreLeft = getInRowLeft board player l c score
                                            scoreRight = getInRowRight board player l c score in
										  if scoreLeft < 0 && scoreRight < 0 then
										    0
										  else if scoreLeft < 0 then
										         (-1) * scoreLeft + scoreRight - 1
											   else  if scoreRight < 0 then 
												       (-1) * scoreRight + scoreLeft -1
                                                     else scoreLeft + scoreRight

--scor coloana
getInColumnUp :: Board -> Cell -> Integer -> Integer -> Integer -> Integer
getInColumnUp board player l c score = let up = getValBoard (l-1) c board in
										 if up == player then 
										  getInColumnUp board player (l-1) c (score+1)
										else if (l-1) < 0 || up /= Null then
										       (-1) * score
									    else score
getInColumnDown :: Board -> Cell -> Integer -> Integer -> Integer -> Integer
getInColumnDown board player l c score = let down = getValBoard (l+1) c board in
										   if down == player  then 
										     getInColumnDown board player (l+1) c (score+1)
										   else if (l+1) > 9 || down /= Null then
										          (-1) * score
									            else score 												 

getScorePosColumn :: Board -> Cell -> Integer ->Integer -> Integer -> Integer
getScorePosColumn board player l c score = let scoreUp = getInColumnUp board player l c score
                                               scoreDown = getInColumnDown board player l c score in
										     if scoreUp < 0 && scoreDown < 0 then
										        0
										     else if scoreUp < 0 then
										            (-1) * scoreUp + scoreDown - 1
											      else  if scoreDown < 0 then 
												          (-1) * scoreDown + scoreUp -1
                                                        else scoreUp + scoreDown
--scor diag principala 
getInDiagPrincUp :: Board -> Cell -> Integer -> Integer -> Integer -> Integer
getInDiagPrincUp board player l c score = let up = getValBoard (l-1) (c-1) board in
										    if up == player then 
										      getInDiagPrincUp board player (l-1) (c-1) (score+1)
										    else if (l-1) < 0 || (c-1) < 0 || up /= Null then
										           (-1) * score
									        else score
getInDiagPrincDown :: Board -> Cell -> Integer -> Integer -> Integer -> Integer
getInDiagPrincDown board player l c score = let down = getValBoard (l+1) (c+1) board in
										      if down == player  then 
										        getInDiagPrincDown board player (l+1) (c+1) (score+1)
										      else if (l+1) > 9 || (c+1) > 9 || down /= Null then
										             (-1) * score
									               else score													 

getScorePosDiagPrinc :: Board -> Cell -> Integer ->Integer -> Integer -> Integer
getScorePosDiagPrinc board player l c score = let scoreUp = getInDiagPrincUp board player l c score
                                                  scoreDown = getInDiagPrincDown board player l c score in
										        if scoreUp < 0 && scoreDown < 0 then
										          0
										        else if scoreUp < 0 then
										               (-1) * scoreUp + scoreDown - 1
											         else  if scoreDown < 0 then 
												             (-1) * scoreDown + scoreUp -1
                                                           else scoreUp + scoreDown

--scor diag secundara	

getInDiagSecunUp :: Board -> Cell -> Integer -> Integer -> Integer -> Integer
getInDiagSecunUp board player l c score = let up = getValBoard (l-1) (c+1) board in
										    if up == player then 
										      getInDiagSecunUp board player (l-1) (c+1) (score+1)
										    else if (l-1) < 0 || (c+1) > 9 ||  not (up == Null) then
										           (-1) * score
									        else score
getInDiagSecunDown :: Board -> Cell -> Integer -> Integer -> Integer -> Integer
getInDiagSecunDown board player l c score = let down = getValBoard (l+1) (c-1) board in
										      if down == player  then 
										        getInDiagSecunDown board player (l+1) (c-1) (score+1)
										      else if (l+1) > 9 || (c-1) < 0 || not (down == Null) then
										             (-1) * score
									               else score													 

getScorePosDiagSecun :: Board -> Cell -> Integer ->Integer -> Integer -> Integer
getScorePosDiagSecun board player l c score = let scoreUp = getInDiagSecunUp board player l c score
                                                  scoreDown = getInDiagSecunDown board player l c score in
										        if scoreUp < 0 && scoreDown < 0 then
										          0
										        else if scoreUp < 0 then
										               (-1) * scoreUp + scoreDown - 1
											         else  if scoreDown < 0 then 
												             (-1) * scoreDown + scoreUp -1
                                                           else scoreUp + scoreDown

--score-ul unei mutari
getScorePosition :: Board -> Cell -> Integer -> Integer -> Integer
getScorePosition board player l c = max (max (getScorePosRow board player l c 1) (getScorePosColumn board player l c 1)) (max (getScorePosDiagPrinc board player l c 1) (getScorePosDiagSecun board player l c 1))						 
	
--board,player,0,0, max, linmax, colmax -> (linmax,colmax)
getBestScoreMove :: Board -> Cell -> Integer ->Integer ->Integer ->Integer ->Integer -> (Integer,Integer)
getBestScoreMove  board player l c max lmax cmax
  | getValBoard l c board == Null = let score = getScorePosition (replaceValBoard l c player board) player l c in
                                      if score > max then 
									    if c<9 then 
                                          getBestScoreMove  board player l (c+1) score l c 
				                        else if c==9 && l==9 then
										        (l,c)
											 else getBestScoreMove  board player (l+1) 0 score l c 
								      else if c<9 then 
                                             getBestScoreMove  board player l (c+1) max lmax cmax
				                           else if c==9 && l==9 then
										          (lmax,cmax)
											    else getBestScoreMove  board player (l+1) 0 max lmax cmax
  | otherwise = if c<9 then 
                  getBestScoreMove  board player l (c+1) max lmax cmax
				else if c==9 && l==9 then
					   (lmax,cmax) 
					 else getBestScoreMove  board player (l+1) 0 max lmax cmax 

getBestScore :: Board -> Cell -> Integer ->Integer ->Integer -> Integer
getBestScore board player l c max 
  | getValBoard l c board == Null = let score = getScorePosition (replaceValBoard l c player board) player l c in
                                      if score > max then 
									    if c<9 then 
                                          getBestScore board player l (c+1) score  
				                        else if c==9 && l==9 then
										        score
											 else getBestScore board player (l+1) 0 score
								      else if c<9 then 
                                             getBestScore board player l (c+1) max 
				                           else if c==9 && l==9 then
										          max
											    else getBestScore board player (l+1) 0 max
  | otherwise = if c<9 then 
                  getBestScore board player l (c+1) max
				else if c==9 && l==9 then
					   max 
					 else getBestScore board player (l+1) 0 max
					 
scoremoveMinMax :: Board -> Cell -> Cell ->Integer -> Integer
scoremoveMinMax board _ _ 0 = 0
scoremoveMinMax board C playerInit depth = let scoreC = getBestScore board C 0 0 0 
                                               moveC = getBestScoreMove board C 0 0 0 0 0 in
					                         if playerInit == C then
								              (scoremoveMinMax (replaceValBoard (fst moveC) (snd moveC) C board) P playerInit (depth-1)) + scoreC
											 else (scoremoveMinMax (replaceValBoard (fst moveC) (snd moveC) C board) P playerInit (depth-1)) - scoreC
scoremoveMinMax board P playerInit depth = let scoreP = getBestScore board P 0 0 0 
                                               moveP = getBestScoreMove board P 0 0 0 0 0 in 
					                         if playerInit == P then
								              (scoremoveMinMax (replaceValBoard (fst moveP) (snd moveP) P board) C playerInit(depth-1)) + scoreP
											 else (scoremoveMinMax (replaceValBoard (fst moveP) (snd moveP) P board) C playerInit (depth-1)) - scoreP
moveComputer :: Board -> Board
moveComputer board = let scoreC = scoremoveMinMax board C C 3
                         scoreP = scoremoveMinMax board P P 3
                         moveC = getBestScoreMove board C 0 0 0 0 0
                         moveP = getBestScoreMove board P 0 0 0 0 0 in
					   if scoreC > scoreP then
					     replaceValBoard (fst moveC) (snd moveC) C board 
					   else replaceValBoard (fst moveP) (snd moveP) C board 
					   
--ex de test
{-
exBigBoard = replaceValBoard 0 1 C (generateBoard 10)
exDiagBoard1 = Board [[Null, P, P, P, P, Null, Null], [Null, Null, P, Null, Null, Null, Null],
               [Null, Null, Null, P, Null, Null, Null], [Null, Null, Null, Null, P, Null, Null], 
               [Null, Null, Null, Null, Null, Null, Null],[Null, Null, Null, Null, Null, P, Null]]

exColBoard = Board [[P,C,Null,Null,Null,Null], [P,C,Null,Null,Null,P], [Null,C,Null,P,Null,Null],
       	   [Null,C,Null,P,Null,Null], [P,C,Null,Null,P,Null], [Null, Null, Null, Null, Null, P, Null]]
		   -}
