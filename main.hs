-- import modul care se ocupa de logica jocului 
import Data.Char
import System.IO
import Game
--Gomoku - Five in a row
main :: IO ()
main = do 
       putStrLn "Gomoku: Bun venit!"
       play (generateBoard 10) 

play :: Board -> IO ()
play board 
  | winner board == P = print board >> putStrLn "Felicitari! Ai castigat!"
  | winner board == C = print board >> putStrLn "Calculatorul a castigat!"
  | otherwise = do 
                print board
                putStrLn "Alege mutarea! Linie:"
                lin <- getLine
                putStrLn "Coloana:"
                col <- getLine
                putStrLn ("pozitia aleasa: " ++ lin ++ " " ++ col)
                let x = read lin :: Integer
                    y = read col :: Integer
                    board' = replaceValBoard x y P board
                    computerMove = getBestScoreMove board' C 0 0 0 0 0
                    board'' = moveComputer board'
                if checkClearPos x y board && x>=0 && x <=9 && y>=0 && y<= 9 then 
                  putStrLn "Mutarea a fost realizata!" >> print board' >> putStrLn "Calculatorul a realizat mutarea! Este randul tau!" >> play board''
				else putStrLn "Mutare invalida! Incearca din nou!" >> play board			   