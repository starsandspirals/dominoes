import DomsMatch
import Debug.Trace

type Situation = GameState -> Bool

type Strategy = GameState -> Move

type Tactic = (Situation, Strategy)

dangerousDoms :: [Dom]
dangerousDoms = [(3,0),(4,1),(5,2),(6,3),(3,3),(6,6)]

hsdSituation :: Situation
hsdSituation _ = True

hsdStrategy :: Strategy
hsdStrategy (h,_,p,b,s) = hsdPlayer h b p s

hsdTactic :: Tactic
hsdTactic = (hsdSituation, hsdStrategy)

firstDropSituation :: Situation
firstDropSituation (h,_,_,InitBoard,_) = elem (5,4) h
firstDropSituation (_,_,_,b,_) = False

firstDropStrategy :: Strategy
firstDropStrategy _ = ((5,4),L)

firstDropTactic :: Tactic
firstDropTactic = (firstDropSituation, firstDropStrategy)

score61Situation :: Situation
score61Situation (h,_,p,b,s) = (playerScore p s) > 53

score61Strategy :: Strategy
score61Strategy gs = scoreStrategy 61 gs

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

dangerousSituation :: Situation
dangerousSituation (h,_,_,_,_) = notempty
  where
    notempty = not (null safe)
    safe = filter (\x -> not (elem x dangerousDoms)) h

dangerousStrategy :: Strategy
dangerousStrategy (h,_,p,b,s) = move
  where
    plays = filter (\x -> not (dangerous x h)) h
    move
      | null plays = hsdPlayer h b p s
      | knocking plays b = hsdPlayer h b p s
      | otherwise = hsdPlayer plays b p s

dangerousTactic :: Tactic
dangerousTactic = (dangerousSituation, dangerousStrategy)

dangerous :: Dom -> Hand -> Bool
dangerous (a,b) h
  | not (elem (a,b) dangerousDoms) = False
  | null knocks = True
  | otherwise = False
  where
    knocks = filter (\(x,y) -> x == a || x == b || y == a || y == b) rest
    rest = filter (\x -> x /= (a,b)) h

playerFramework :: GameState -> [Tactic] -> Move

playerFramework (h1,h2,p,b,s) tactics = move
  where
    state = (h1, h2, p, b, s)
    tactic:_ = filter (\(situation, _) -> situation state) tactics
    (_, strategy) = tactic
    move = strategy state

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

playerScore :: Player -> Scores -> Int
playerScore P1 (s,_) = s
playerScore P2 (_,s) = s

easyPlayer :: DomsPlayer
easyPlayer h1 db p s = move
  where
    tactics = [hsdTactic]
    h2 = opponentPlays h1 db
    move = playerFramework (h1,h2,p,db,s) tactics

mediumPlayer :: DomsPlayer
mediumPlayer h1 db p s = move
  where
    tactics = [firstDropTactic, score61Tactic, hsdTactic]
    h2 = opponentPlays h1 db
    move = playerFramework (h1,h2,p,db,s) tactics

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

isJust :: (Maybe a) -> Bool
isJust (Just _) = True
isJust Nothing = False

resMaybe :: (Maybe a) -> a 
resMaybe (Just x) = x


    
