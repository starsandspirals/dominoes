import DomsMatch

type Plays = ([Dom], [Dom])

type Situation = Plays -> DomBoard -> Bool

type Strategy = Plays -> DomsPlayer

type Tactic = (Situation, Strategy)

hsdSituation :: Situation
hsdSituation _ _ = True

hsdStrategy :: Strategy
hsdStrategy _ h b p s = hsdPlayer h b p s

hsdTactic :: Tactic
hsdTactic = (hsdSituation, hsdStrategy)

playerFramework :: Hand -> DomBoard -> Player -> Scores -> [Tactic] -> Move

playerFramework h InitBoard p s tactics = move
  where
    remainingDoms = filter (\x -> not (elem x h)) domSet
    plays = (h, remainingDoms)
    tactic:_ = filter (\(situation, _) -> situation plays InitBoard) tactics
    (_, strategy) = tactic
    move = strategy plays h InitBoard p s

playerFramework h board p s tactics = move
  where
    remainingDoms = filter (\x -> not (elem x h)) domSet
    (Board _ _ history) = board
    playedDoms = map (\(x, _, _) -> x) history
    unplayedDoms = filter (\x -> not (elem x playedDoms)) remainingDoms
    plays = (h, unplayedDoms)
    tactic:_ = filter (\(situation, _) -> situation plays board) tactics
    (_, strategy) = tactic
    move = strategy plays h board p s

basicPlayer :: DomsPlayer
basicPlayer h db p s = move
  where
    tactics = [hsdTactic]
    move = playerFramework h db p s tactics


    

    
