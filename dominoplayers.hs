import DomsMatch

type Plays = ([Dom], [Dom])

type Situation = Plays -> Player -> Scores -> DomBoard -> Bool

type Strategy = Plays -> DomsPlayer

type Tactic = (Situation, Strategy)

hsdSituation :: Situation
hsdSituation _ _ _ _ = True

hsdStrategy :: Strategy
hsdStrategy _ h b p s = hsdPlayer h b p s

hsdTactic :: Tactic
hsdTactic = (hsdSituation, hsdStrategy)

firstDropSituation :: Situation
firstDropSituation (h,_) _ _ InitBoard = elem (5,4) h
firstDropSituation _ _ _ board = False

firstDropStrategy :: Strategy
firstDropStrategy _ _ _ _ _ = ((5,4),L)

firstDropTactic :: Tactic
firstDropTactic = (firstDropSituation, firstDropStrategy)

score61Situation :: Situation
score61Situation (h,_) p (s1, s2) board
  | p == P1 && s1 > 53 = True
  | p == P2 && s2 > 53 = True
  | otherwise = False

score61Strategy :: Strategy
score61Strategy _ h b p (s1, s2) = move
  where
    score
      | p == P1 = s1
      | otherwise = s2
    difference = 61 - score
    scoringmoves = scoreN b difference
    moves = filter (\(x,_) -> elem x h) scoringmoves
    move
      | null moves = hsdPlayer h b p (s1,s2)
      | otherwise = head moves

score61Tactic :: Tactic
score61Tactic = (score61Situation, score61Strategy)
    
playerFramework :: Hand -> DomBoard -> Player -> Scores -> [Tactic] -> Move

playerFramework h InitBoard p s tactics = move
  where
    remainingDoms = filter (\x -> not (elem x h)) domSet
    plays = (h, remainingDoms)
    tactic:_ = filter (\(situation, _) -> situation plays p s InitBoard) tactics
    (_, strategy) = tactic
    move = strategy plays h InitBoard p s

playerFramework h board p s tactics = move
  where
    remainingDoms = filter (\x -> not (elem x h)) domSet
    (Board _ _ history) = board
    playedDoms = map (\(x, _, _) -> x) history
    unplayedDoms = filter (\x -> not (elem x playedDoms)) remainingDoms
    plays = (h, unplayedDoms)
    tactic:_ = filter (\(situation, _) -> situation plays p s board) tactics
    (_, strategy) = tactic
    move = strategy plays h board p s

easyPlayer :: DomsPlayer
easyPlayer h db p s = move
  where
    tactics = [hsdTactic]
    move = playerFramework h db p s tactics

mediumPlayer :: DomsPlayer
mediumPlayer h db p s = move
  where
    tactics = [firstDropTactic, score61Tactic, hsdTactic]
    move = playerFramework h db p s tactics

scoreN :: DomBoard -> Int -> [ (Dom, End) ]

scoreN b i = scoreNA b i domSet

scoreNA :: DomBoard -> Int -> [Dom] -> [ (Dom, End) ]
 
scoreNA b i [d]
  | goesLP d b && goesRP d b && sL == i && sR == i = [ (d, L), (d, R) ]
  | goesLP d b && sL == i = [ (d, L) ]
  | goesRP d b && sR == i = [ (d, R) ]
  | otherwise = []
  where sL = scoreboard (resMaybe (playDom P1 d L b))
        sR = scoreboard (resMaybe (playDom P1 d R b))

scoreNA b i (h:t) = scoreNA b i [h] ++ scoreNA b i t

isJust :: (Maybe a) -> Bool
isJust (Just _) = True
isJust Nothing = False

resMaybe :: (Maybe a) -> a 
resMaybe (Just x) = x


    

    
