import Control.Monad (when)
import Data.Char (ord)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  let empty = replicate 3 (replicate 3 Z)
  go empty X

go :: [[Pip]] -> Pip -> IO ()
go pss p = do
  draw pss
  let options = map fst (filter (\(_,p') -> p' == Z) (zip (concat labels) (concat pss)))
  if null options
    then putStrLn "mehhhh...."
    else do
      l <- readMove options
      let pss' = write (\l' -> if l == l' then Just p else Nothing) pss
      let win' = winner pss'
      if win' /= Z
        then draw pss' >> putStrLn ("das weiner " ++ [pipChar win'])
        else go pss' (nextPlayer p)

readMove :: [Int] -> IO Int
readMove opts = do
  k <- getChar
  when (k == 'q') (error "user quit")
  let i = ord k - ord '1' + 1
  if i < 1 || i > 9 || not (i `elem` opts)
    then readMove opts
    else pure i
         
nextPlayer :: Pip -> Pip
nextPlayer X = O
nextPlayer O = X

labels :: [[Int]]
labels = [[1,2,3], [4,5,6], [7,8,9]]

write :: (Int -> Maybe a) -> [[a]] -> [[a]]
write f xss = zipWith (zipWith (\l x -> case f l of
                                          Just x' -> x'
                                          Nothing -> x )) labels xss

draw :: [[Pip]] -> IO ()
draw pss = mapM_ putStrLn (map (map pipChar) pss)

pipChar :: Pip -> Char
pipChar Z = '.'
pipChar X = 'X'
pipChar O = 'O'

data Pip = Z | X | O deriving (Eq)

winner :: [[Pip]] -> Pip
winner pss = let wins = runWinners pss in if null wins then Z else head wins

runWinners :: [[Pip]] -> [Pip]
runWinners pss = filter (/= Z) (map (\r -> runWinner (r pss)) runs)

runWinner :: [Pip] -> Pip
runWinner ps@(p:_) = if all (== p) ps then p else Z

transp :: [[Pip]] -> [[Pip]]
transp [[a,b,c], [d,e,f], [g,h,i]] = [[a,d,g], [b,e,h], [c,f,i]]

mirr :: [[Pip]] -> [[Pip]]
mirr = map reverse

diag :: [[Pip]] -> [Pip]
diag [[x,_,_], [_,y,_], [_,_,z]] = [x,y,z]

row :: Int -> [[Pip]] -> [Pip]
row n = (!! n)

type Run = [[Pip]] -> [Pip]

runs :: [Run]
runs = [diag, diag . mirr] ++ map row [0..2] ++ map (\n -> row n . transp) [0..2]
