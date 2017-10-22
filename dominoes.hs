module Dominoes where

  type Domino = (Int,Int)

  type Hand = [Domino]

  type Board = [Domino]

  data End = L | R

  
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

  playDom d board L
    | goesP d L board = Just (d:board)
    | otherwise = Nothing

  playDom d (a:b) R
    | goesP d R (a:b) = playDom d b R
    | otherwise = Nothing

  
  scoreBoard :: Board -> Int
  scoreBoard [] = 0

  scoreBoard [(f1,s1)]
    | score `mod` 3 == 0 = score `div` 3
    | score `mod` 5 == 0 = score `div` 5
    | otherwise = 0
    where score = f1+s1

  scoreBoard [(f1,_),(_,s2)]
    | score `mod` 3 == 0 && score `mod` 5 == 0 = score `div` 3 + score `div` 5
    | score `mod` 3 == 0 = score `div` 3
    | score `mod` 5 == 0 = score `div` 5
    | otherwise = 0
    where score = f1+s2

  scoreBoard b = scoreBoard [head b, last b]


    





