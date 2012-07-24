import Data.List
import Data.Set (Set)
import qualified Data.Set as S

optimalElevatorPath destinations initFloor minFloor maxFloor elevatorCapacity = bfs fr0 (S.fromList fr0)
  where
    (x, y, z) &: t = (x, y, z, (x, y, z) : t)
    
    fr0 = [ (initFloor, p0, destinations \\ p0) &: []
          | p0 <- filter ((<= elevatorCapacity) . length) $ subsequences
                $ filter ((== initFloor) . fst) destinations
          ]        
    
    genStates (floor, passangers, destinations, track)
      = [ ( f', (passangers ++ seatP) \\ leftP, 
            (destinations \\ seatP) ++ (map (\(_,d) -> (f',d)) $ filter ((/= f') . snd) leftP) 
          ) &: track                 
            
        | f' <- [floor + 1, floor - 1]
        , f' <= maxFloor && f' >= minFloor
        , leftP <- subsequences passangers
        , let freeP = elevatorCapacity - length passangers + length leftP
        , seatP <- filter ((<= freeP) . length) $ subsequences
                 $ filter ((== f') . fst) destinations
        ]
    
    bfs fringe visited = case find (\(_, p, d, _) -> null p && null d) fringe of
      Just (_, _, _, t) -> t
      Nothing           -> bfs newStates (S.union visited (S.fromList newStates))
        
      where 
        newStates = filter (not . (`S.member` visited)) (concatMap genStates fringe)

test a b c d e = mapM_ (putStrLn . show) $ optimalElevatorPath a b c d e

t1 = test [(1,2),(3,2),(2,4),(2,3),(4,1)] 1 1 4 2
t2 = test [(3,1),(3,1),(3,1),(3,1)] 1 1 4 2