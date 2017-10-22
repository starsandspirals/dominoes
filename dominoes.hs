module Dominoes where

  type Domino = (Int,Int)

  type Hand = [Domino]

  type Board = [Domino]

  data End = L | R
             deriving (Show)

  allDominoes = [(0,0),
                 (1,0),(1,1),
                 (2,0),(2,1),(2,2),
                 (3,0),(3,1),(3,2),(3,3),
                 (4,0),(4,1),(4,2),(4,3),(4,4),
                 (5,0),(5,1),(5,2),(5,3),(5,4),(5,5),
                 (6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6)]

  
  goesP :: Domino -> End -> Board -> Bool
  goesP _ _ [] = True
  
  goesP (f,s) L ( (f1,s1):_ ) = f==f1 || s==f1

  goesP (f,s) R [(f1,s1)] = f==s1 || s==s1
  goesP (f,s) R ( _:t ) = goesP (f,s) R t
  

  knockingP :: Hand -> Board -> Bool
  knockingP [] _ = True
  knockingP _ [] = False
  
  knockingP ( h:t ) b 
   | goesP h L b || goesP h R b = False
   | otherwise = knockingP t b


  playedP :: Domino -> Board -> Bool
  playedP _ [] = False

  playedP (f,s) ( (f1,s1):t )
    | f==f1 && s==s1 = True
    | f==s1 && s==f1 = True
    | otherwise = playedP (f,s) t


  possPlays :: Hand -> Board -> ( [Domino],[Domino] )
  possPlays [] _ = ( [],[] )
  possPlays h [] = ( h,h )
  
  possPlays (h:t) board
    | goesP h L board && goesP h R board = (h:l, h:r)
    | goesP h L board = (h:l, r)
    | goesP h R board = (l, h:r)
    where (l,r) = possPlays t board

  
  playDom :: Domino -> Board -> End -> Maybe Board
  playDom d [] _ = Just [d]

  playDom (f,s) board L
    | goesP (f,s) L board && s==f1 = Just ((f,s):board)
    | goesP (s,f) L board && f==f1 = Just ((s,f):board)
    | otherwise = Nothing
    where (f1,s1):t = board

  playDom (f,s) [d] R
    | goesP (f,s) R [d] && f==s1 = Just [d,(f,s)]
    | goesP (s,f) R [d] && s==s1 = Just [d,(s,f)]
    | otherwise = Nothing
    where (f1,s1) = d

  playDom (f,s) (a:b) R
    | isJust recur = Just (a:resMaybe recur)
    | otherwise = Nothing
    where recur = playDom (f,s) b R
 

  isJust :: (Maybe a) -> Bool
  isJust (Just _) = True
  isJust Nothing = False


  resMaybe :: (Maybe a) -> a 
  resMaybe (Just x) = x

  
  scoreBoard :: Board -> Int
  scoreBoard [] = 0

  scoreBoard [(f1,s1)]
    | score `mod` 3 == 0 = score `div` 3
    | score `mod` 5 == 0 = score `div` 5
    | otherwise = 0
    where score = f1+s1

  scoreBoard [(f1,f2),(s1,s2)]
    | score `mod` 3 == 0 && score `mod` 5 == 0 = score `div` 3 + score `div` 5
    | score `mod` 3 == 0 = score `div` 3
    | score `mod` 5 == 0 = score `div` 5
    | otherwise = 0
    where score
            | f1==f2 && s1==s2 = 2*(f1+s2)
            | f1==f2 = 2*f1 + s2
            | s1==s2 = f1 + 2*s2
            | otherwise = f1+s2

  scoreBoard b = scoreBoard [head b, last b]


  scoreN :: Board -> Int -> [ (Domino,End) ]
  
  scoreN b i = scoreNA b i allDominoes

  
  scoreNA :: Board -> Int -> [Domino] -> [ (Domino, End) ]
 
  scoreNA b i [d]
    | goesP d L b && goesP d R b && scoreL == i && scoreR == i = [ (d,L),(d,R) ]
    | goesP d L b && scoreL == i = [ (d,L) ]
    | goesP d R b && scoreR == i = [ (d,R) ]
    | otherwise = []
    where scoreL = scoreBoard (resMaybe (playDom d b L))
          scoreR = scoreBoard (resMaybe (playDom d b R))

  scoreNA b i (h:t) = scoreNA b i [h] ++ scoreNA b i t


    





