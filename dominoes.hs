{- COM2001 Dominoes Assignment
   Daniel Marshall -}

module Dominoes where

  import MergeSort
  import System.Random

  type DomsPlayer = Hand -> Board -> (Domino, End)

  
  scoreDom :: Domino -> Board -> End -> Int
  
  scoreDom d board end = score
    where score = scoreBoard move
          move = resMaybe maybemove
          maybemove = playDom d board end

  
  simplePlayer :: DomsPlayer

  simplePlayer hand board = play
    where (lefts, rights) = possPlays hand board
          left:_ = lefts
          right:_ = rights
          play
            | null lefts = (right, R)
            | otherwise = (left, L)

 
  hsdPlayer :: DomsPlayer

  hsdPlayer hand board = play
    where (lefts, rights) = possPlays hand board
          moves = map (\x -> (x, L)) lefts ++ map (\x -> (x, R)) rights
          scores = map (\(d, end) -> scoreDom d board end) moves
          zipped = zip moves scores
          sorted = mergesort (\(_, n1) (_, n2) -> n1 > n2) zipped
          play:_ = map fst sorted
          

  shuffleDoms :: StdGen -> [Domino]
  
  shuffleDoms gen = shuffled
    where randomlist = take 28 (randoms gen :: [Int])
          zipped = zip allDominoes randomlist
          sorted = mergesort (\(_, n1) (_, n2) -> n1 < n2) zipped
          shuffled = map fst sorted

 
  playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
  
  playDomsRound play1 play2 seed = playDomsRoundA play1 play2 hand1 hand2 []
    where randgen = mkStdGen seed
          deck = shuffleDoms randgen
          hands = take 14 deck
          (hand1, hand2) = splitAt 7 hands


  playDomsRoundA :: DomsPlayer -> DomsPlayer -> Hand -> Hand ->
                      Board -> (Int, Int)
  
  playDomsRoundA play1 play2 hand1 hand2 board
    | knockingP hand1 board && knockingP hand2 board = (0, 0)
    | otherwise = (score1 + next1, score2 + next2)
    where (score1, nhand1, nboard1) = playTurn play1 hand1 board
          (score2, nhand2, nboard2) = playTurn play2 nhand1 nboard1
          (next1, next2) = playDomsRoundA play1 play2 nhand1 nhand2 nboard2

    
         
  playTurn :: DomsPlayer -> Hand -> Board -> (Int, Hand, Board)

  playTurn player hand board
    | knockingP hand board = (0, hand, board)
    | otherwise = (score, nhand, nboard)
    where (dom, end) = player hand board
          nhand = filter (\x -> x /= dom) hand
          nboard = resMaybe (playDom dom board end)
          score = scoreBoard nboard



  type Domino = (Int,Int)

  type Hand = [Domino]

  type Board = [Domino]

  data End = L | R
             deriving (Eq, Ord, Show)

  allDominoes = [(0,0),
                 (1,0), (1,1),
                 (2,0), (2,1), (2,2),
                 (3,0), (3,1), (3,2), (3,3),
                 (4,0), (4,1), (4,2), (4,3), (4,4),
                 (5,0), (5,1), (5,2), (5,3), (5,4), (5,5),
                 (6,0), (6,1), (6,2), (6,3), (6,4), (6,5), (6,6)]

  
  {- goesP checks if a given domino can be played at a given
     end of a given board, in either orientation. -}
  goesP :: Domino -> End -> Board -> Bool
  goesP _ _ [] = True
  
  {- If either of the domino's sides matches the left side of
   the first domino on the board, then it can be played on
   the left. -}
  goesP (f, s) L ( (f1,s1):_ ) = f == f1 || s == f1

  {- If there is one domino on the board, then the right case
     is similar to the left case. Otherwise, we check the tail
     until only one domino remains. -}
  goesP (f, s) R [ (f1,s1) ] = f == s1 || s == s1
  goesP (f, s) R ( _:t ) = goesP (f, s) R t
  

  {- knockingP returns true if a given hand contains no dominoes
     that can be played on a given board. -}
  knockingP :: Hand -> Board -> Bool
  knockingP [] _ = True
  knockingP _ [] = False
  
  {- We check the first domino to see if it can be played, and if
     it cannot we continue checking the rest of the dominoes in
     the hand. -}
  knockingP ( h:t ) b 
   | goesP h L b || goesP h R b = False
   | otherwise = knockingP t b


  {- playedP returns true if a given domino has already been played
     on a given board, in either orientation. -}
  playedP :: Domino -> Board -> Bool
  playedP _ [] = False

  {- We check the first domino on the board to see if it matches the
     given domino, and if not we continue checking the rest of the
     board. -}
  playedP (f, s) ( (f1, s1):t )
    | f == f1 && s == s1 = True
    | f == s1 && s == f1 = True
    | otherwise = playedP (f, s) t


  {- possPlays returns two lists, containing the dominoes in a given
     hand that can be played at the left end and the right end of a 
     given board. -}
  possPlays :: Hand -> Board -> ( [Domino], [Domino] )
  possPlays [] _ = ( [],[] )
  possPlays h [] = ( h,h )
  
  {- We check the first domino in the hand, and add it to the lists
     depending on which ends it can be played at. We then join this
     to a recursive call of possPlays, to check all other dominoes
     in the hand in a similar way. -}
  possPlays (h:t) board
    | goesP h L board && goesP h R board = (h:l, h:r)
    | goesP h L board = (h:l, r)
    | goesP h R board = (l, h:r)
    | otherwise = (l, r)
    where (l, r) = possPlays t board

  
  {- playDom plays a given domino onto a given end of a given board
     if possible, in the correct orientation, and returns Nothing if
     this is not possible. -}
  playDom :: Domino -> Board -> End -> Maybe Board
  playDom d [] _ = Just [d]

  {- On the left, we just join the domino to the head of the board in
     the orientation it can be played. -}
  playDom (f, s) board L
    | goesP (f, s) L board && s == f1 = Just ( (f, s):board )
    | goesP (s, f) L board && f == f1 = Just ( (s, f):board )
    | otherwise = Nothing
    where (f1, s1):t = board

  {- The right is more complicated; if there is one domino on the board,
     we can join the domino to the end similarly, but if there are more,
     we join the rest recursively to the beginning until we get to this
     case, returning Nothing again if the domino cannot be played. -}
  playDom (f, s) [d] R
    | goesP (f, s) R [d] && f == s1 = Just [ d, (f, s) ]
    | goesP (s, f) R [d] && s == s1 = Just [ d, (s, f) ]
    | otherwise = Nothing
    where (f1, s1) = d

  playDom (f, s) (a:b) R
    | isJust recur = Just ( a:resMaybe recur )
    | otherwise = Nothing
    where recur = playDom (f, s) b R
 

  -- Functions for working with Maybes taken from the notes.
  isJust :: (Maybe a) -> Bool
  isJust (Just _) = True
  isJust Nothing = False


  resMaybe :: (Maybe a) -> a 
  resMaybe (Just x) = x

  
  {- scoreBoard calculates the 5s-and-3s score for a given board. This
     means finding the sum of the two open ends of the board and then
     working out how many 3s or 5s goes into this total, with double
     the points being granted for a double domino. -}
  scoreBoard :: Board -> Int
  scoreBoard [] = 0

  {- One domino needs to be worked out as a special case, as we do not
     want to count it twice if it happens to be a double. -}
  scoreBoard [ (f1, s1) ]
    | score `mod` 3 == 0 = score `div` 3
    | score `mod` 5 == 0 = score `div` 5
    | otherwise = 0
    where score = f1 + s1

  {- All cases can be reduced to the case where there are two dominoes,
     as only the first and last domino are relevant for scoring. We can
     calculate the required score from these two dominoes, adjusting for
     either of them being doubles. -}
  scoreBoard [ (f1, f2),(s1, s2) ]
    | score `mod` 3 == 0 && score `mod` 5 == 0 = score `div` 3 + score `div` 5
    | score `mod` 3 == 0 = score `div` 3
    | score `mod` 5 == 0 = score `div` 5
    | otherwise = 0
    where score
            | f1 == f2 && s1 == s2 = 2 * (f1 + s2)
            | f1 == f2 = (2 * f1) + s2
            | s1 == s2 = f1 + (2 * s2)
            | otherwise = f1 + s2

  scoreBoard b = scoreBoard [head b, last b]


  {- scoreN takes a board and an integer and returns a list of dominoes
     that could be played to give the required score. We pass this on
     to an auxiliary function so that a list of all dominoes can be used. -}
  scoreN :: Board -> Int -> [ (Domino, End) ]
  
  scoreN b i = scoreNA b i allDominoes

  
  {- scoreNA is our auxiliary function that creates the list required by
     scoreN. -}
  scoreNA :: Board -> Int -> [Domino] -> [ (Domino, End) ]
 
  {- We recursively check through the list of all possible dominoes. For
     each possibility, check whether it can be played at each end of the
     board, and if it can, whether it gives the required score. If both
     of these are true, the domino is included in the list returned by
     the function, as well as the end it can be played at. -}
  scoreNA b i [d]
    | goesP d L b && goesP d R b && sL == i && sR == i = [ (d, L), (d, R) ]
    | goesP d L b && sL == i = [ (d, L) ]
    | goesP d R b && sR == i = [ (d, R) ]
    | otherwise = []
    where sL = scoreBoard (resMaybe (playDom d b L))
          sR = scoreBoard (resMaybe (playDom d b R))

  scoreNA b i (h:t) = scoreNA b i [h] ++ scoreNA b i t


    





