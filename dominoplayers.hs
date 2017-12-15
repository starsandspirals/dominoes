{- COM2001 Assignment 3
   Daniel Marshall -}
import DomsMatch
import MergeSort

{- I use three new data types for my player framework; situations, strategies
   and tactics. A situation takes the gamestate and returns true or false
   depending on whether the situation is appropriate for the tactic, and then
   the strategy returns the actual move. Keeping these separate allows for
   increased efficiency. -}
type Situation = GameState -> Bool

type Strategy = GameState -> Move

type Tactic = (Situation, Strategy)

{- A couple of useful constants are defined here, including a list of valuable
   dominoes and a list containing the 7 possible pip values. -}
dangerousDoms :: [Dom]
dangerousDoms = [(3,0),(4,1),(5,2),(6,3),(3,3),(6,6)]

pips :: [Int]
pips = [0..6]

{- The hsdTactic is the simplest tactic of all, and is used as a last resort,
   so that if all other tactics do not apply the hsdTactic simply plays the
   highest scoring domino. -}
hsdSituation :: Situation
hsdSituation _ = True

hsdStrategy :: Strategy
hsdStrategy (h,_,p,b,s) = hsdPlayer h b p s

hsdTactic :: Tactic
hsdTactic = (hsdSituation, hsdStrategy)

{- The firstDropTactic comes into play if the board is empty and the (5,4)
   domino is in the player's hand; then, we play this domino, as it scores
   3 points but only 2 points can be scored in response. -}
firstDropSituation :: Situation
firstDropSituation (h,_,_,InitBoard,_) = elem (5,4) h
firstDropSituation (_,_,_,b,_) = False

firstDropStrategy :: Strategy
firstDropStrategy _ = ((5,4),L)

firstDropTactic :: Tactic
firstDropTactic = (firstDropSituation, firstDropStrategy)

{- The score61Tactic is used if the player is getting close to winning. In this
   case, we try and play a domino that gets us to 61 if possible, as this ends
   the game, and if not we try and get to 59, as there are more ways of scoring
   2 than any other value. -}
score61Situation :: Situation
score61Situation (h,_,p,_,s) = (playerScore p s) > 53

score61Strategy :: Strategy
score61Strategy gs = scoreStrategy 61 gs

{- scoreStrategy is used to find the move that will get us to 61 or 59. We find
   the amount of points needed to get the required score, find all possible
   dominoes that could score this value using scoreN, and filter the hand for
   dominoes included in this list. If none exist, we either try and get to 59
   if we were trying to get to 61, or just play the highest scoring domino. -}
scoreStrategy :: Int -> Strategy
scoreStrategy i gs@(h,_,p,b,s) = move
  where
    score = playerScore p s
    difference = i - score
    scoringmoves = scoreN b difference
    moves = filter (\(x,_) -> elem x h) scoringmoves
    first:_ = moves
    move
      | null moves && i == 61 = scoreStrategy 59 gs
      | null moves = hsdPlayer h b p s
      | otherwise = first

score61Tactic :: Tactic
score61Tactic = (score61Situation, score61Strategy)

{- The dangerousTactic is used when there are valuable dominoes in the hand
   which cannot be knocked off. In this case, we try and play either dominoes
   that are not valuable or valuable dominoes that can be knocked off, as
   otherwise we could get into a dangerous situation. -}
dangerousSituation :: Situation
dangerousSituation (h,_,_,_,_) = not (null danger)
  where
    danger = filter (\x -> dangerous x h) h

dangerousStrategy :: Strategy
dangerousStrategy (h,_,p,b,s) = move
  where
    safe = filter (\x -> not (dangerous x h)) h
    move
      | knocking safe b = hsdPlayer h b p s
      | otherwise = hsdPlayer safe b p s

dangerousTactic :: Tactic
dangerousTactic = (dangerousSituation, dangerousStrategy)

{- The dangerous function decides if a domino is dangerous to play or not. If
   the domino is not valuable, it isn't dangerous; if it is valuable, then it
   is only dangerous if it cannot be knocked off, i.e. if there are no dominoes
   in the hand with one of the same pips. -}
dangerous :: Dom -> Hand -> Bool
dangerous (a,b) h
  | not (elem (a,b) dangerousDoms) = False
  | null knocks = True
  | otherwise = False
  where
    knocks = filter (\(x,y) -> samePips (x,y) (a,b)) rest
    rest = filter (\x -> x /= (a,b)) h

{- The majorityTactic is used when we have the majority of dominoes with one
   pip value in the hand. In this case, we try to play the double of this value
   if we have it, and otherwise we try and play a domino that has this pip
   value if possible. -}
majoritySituation :: Situation
majoritySituation (h,_,_,b,_) = max >= 4
  where
    sets = map (\x -> filterPips h x) pips
    lengths = map (\x -> length x) sets
    max = maximum lengths

majorityStrategy :: Strategy
majorityStrategy (h,_,p,b,s) = move
  where
    sets = map (\x -> (x, filterPips h x)) pips
    lengths = map (\(x,l) -> (x,length l)) sets
    (pip,_):_ = mergesort (\(_,l1) (_,l2) -> l1 > l2) lengths
    double = (pip, pip)
    [(_,set)] = filter (\(x,l) -> x == pip) sets
    move
      | elem double h && goesLP double b = (double, L)
      | elem double h && goesRP double b = (double, R)
      | knocking set b = hsdPlayer h b p s
      | otherwise = hsdPlayer h b p s

majorityTactic :: Tactic
majorityTactic = (majoritySituation, majorityStrategy)

{- The opponentNearTactic is used when the opponent is getting close to winning
   the game, and in this case, we try to find a domino that prevents them
   getting to 61 on the next move. We do this by finding all possible moves
   that could get them to 61 using dominoes they might have in their hand, and
   then looking for dominoes in our hand that will block them from being able
   to play these. -}
opponentNearSituation :: Situation
opponentNearSituation (_,_,_,InitBoard,s) = False
opponentNearSituation (_,_,p,b,s) = (opponentScore p s) > 53

opponentNearStrategy :: Strategy
opponentNearStrategy (h1,h2,p,b,s) = move
  where
    score = opponentScore p s
    difference = 61 - score
    scoringmoves = scoreN b difference
    opponentmoves = filter (\(x,_) -> elem x h2) scoringmoves
    (Board (e1,_) (_,e2) _) = b
    playeroptions = map (\(x,e) -> blockDom h1 x e e1 e2) opponentmoves
    playerlists = filter (\x -> not(null x)) playeroptions
    firstlist:_ = playerlists
    move
      | null playerlists = hsdPlayer h1 b p s
      | knocking firstlist b = hsdPlayer h1 b p s
      | otherwise = hsdPlayer firstlist b p s

opponentNearTactic :: Tactic
opponentNearTactic = (opponentNearSituation, opponentNearStrategy)

{- filterPips is a utility function for finding all dominoes in a hand that
   contain a particular pip value. -}
filterPips :: Hand -> Int -> Hand
filterPips h i = filter (\(a,b) -> a==i || b==i) h

{- samePips is a utility function for comparing two dominoes and seeing if
   either of their pips match up. -}
samePips :: Dom -> Dom -> Bool
samePips (a,b) (c,d) = a == c || b == c || a == d || b == d

{- blockDom is a utility function for finding dominoes in a hand that can be 
   played at an end of a board, such that a particular domino cannot be played
   at that end by the opponent. -}
blockDom :: Hand -> Dom -> End -> Int -> Int -> Hand
blockDom h (x1,y1) end e1 e2
  | end == L = filter (\(a,b) -> not((a == e1 && b /= x1 && b /= y1))
                              || (b == e1 && a /= x1 && a /= y1)) h
  | end == R = filter (\(a,b) -> not((a == e2 && b /= x1 && b /= y1))
                              || (b == e2 && a /= x1 && a /= y1)) h

{- playerFramework is the main function controlling the player, where all the
   action happens. A list of tactics is passed in by whichever particular
   player has been called, then on each turn we check all of the situations
   to see which tactics apply, and then call the first possible strategy to
   find our move. -}
playerFramework :: GameState -> [Tactic] -> Move

playerFramework (h1,h2,p,b,s) tactics = move
  where
    state = (h1, h2, p, b, s)
    tactic:_ = filter (\(situation, _) -> situation state) tactics
    (_, strategy) = tactic
    move = strategy state

{- opponentPlays is a utility function used by the players, which works out
   using the hand and history the possible dominoes that the opponent could
   have in their hand. -}
opponentPlays :: Hand -> DomBoard -> Hand

opponentPlays h InitBoard = plays
  where
    plays = filter (\x -> not (elem x h)) domSet
    
opponentPlays h b = plays
  where
    remaining = filter (\x -> not (elem x h)) domSet
    (Board _ _ history) = b
    used = map (\(x,_,_) -> x) history
    plays = filter (\x -> not (elem x used)) remaining

{- playerScore and opponentScore are two utility functions that, given a player
   and the tuple of scores, returns either the current player's score or their
   opponent's score respectively. -}
playerScore :: Player -> Scores -> Int
playerScore P1 (s,_) = s
playerScore P2 (_,s) = s

opponentScore :: Player -> Scores -> Int
opponentScore P1 (_,s) = s
opponentScore P2 (s,_) = s

{- easyPlayer, mediumPlayer and hardPlayer are the three actual players I have
   programmed; they differ only in that they have different lists of tactics
   that they pass to the playerFramework. dangerousTactic and majorityTactic
   are not used at all, as they make the players worse; other than this, the
   players increase in difficulty by adding more tactics as we go on. -}
easyPlayer :: DomsPlayer
easyPlayer h1 db p s = move
  where
    tactics = [hsdTactic]
    h2 = opponentPlays h1 db
    move = playerFramework (h1,h2,p,db,s) tactics

mediumPlayer :: DomsPlayer
mediumPlayer h1 db p s = move
  where
    tactics = [score61Tactic, hsdTactic]
    h2 = opponentPlays h1 db
    move = playerFramework (h1,h2,p,db,s) tactics

hardPlayer :: DomsPlayer
hardPlayer h1 db p s = move
  where
    tactics = [firstDropTactic, score61Tactic, opponentNearTactic, 
                hsdTactic]
    h2 = opponentPlays h1 db
    move = playerFramework (h1,h2,p,db,s) tactics

{- scoreN is a utility function that I imported from the first assignment, and
   edited to work with the DomsMatch code. It finds all possible moves that 
   could be played on a given board that will give a particular score value. -}
scoreN :: DomBoard -> Int -> [(Dom, End)]

scoreN b i = scoreNA b i domSet

scoreNA :: DomBoard -> Int -> [Dom] -> [(Dom, End)]
 
scoreNA b i [d]
  | goesLP d b && goesRP d b && sL == i && sR == i = [(d,L),(d,R)]
  | goesLP d b && sL == i = [(d,L)]
  | goesRP d b && sR == i = [(d,R)]
  | otherwise = []
  where sL = scoreboard (resMaybe (playDom P1 d L b))
        sR = scoreboard (resMaybe (playDom P1 d R b))

scoreNA b i (h:t) = scoreNA b i [h] ++ scoreNA b i t

{- isJust and resMaybe are utility functions for working with maybes, taken
   from the notes. -}
isJust :: (Maybe a) -> Bool
isJust (Just _) = True
isJust Nothing = False

resMaybe :: (Maybe a) -> a 
resMaybe (Just x) = x

{- The cheatingTactic and cheatingPlayer are an extra player I wrote for fun,
   that probably shouldn't be marked. It creates whatever imaginary domino
   would score exactly 61 points in one move and plays it, regardless of the
   contents of its hand or the rules of dominoes, causing it to win every game
   that it plays. :) -}
cheatList :: [Int]
cheatList = [171..183]

cheatingSituation :: Situation
cheatingSituation _ = True

cheatingStrategy :: Strategy

cheatingStrategy (_,_,_,InitBoard,_) = ((0,183),L)

cheatingStrategy (_,_,p,b,s) = move
  where
    (Board (c,d) (e,f) _) = b
    doms = map (\x -> (x,c)) cheatList
    scores = map (\x -> (x, scoreDom x L b)) doms
    winners = filter (\(_,s) -> s == 61) scores
    play:_ = map (\(x,_) -> x) winners
    move = (play, L)

cheatingTactic = (cheatingSituation, cheatingStrategy)

cheatingPlayer :: DomsPlayer
cheatingPlayer h1 db p s = move
  where
    tactics = [cheatingTactic]
    h2 = opponentPlays h1 db
    move = playerFramework (h1,h2,p,db,s) tactics




    
