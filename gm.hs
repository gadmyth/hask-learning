-- url = http://css.dzone.com/articles/game-life-haskell
import Data.List

data CellState = Dead | Alive
data Position = Position Integer Integer deriving (Eq, Show)
type Generation = Position -> CellState


is_alive :: CellState -> Bool
is_alive Alive = True
is_alive Dead = False


-- improved version from <url>
neighbors :: Position -> [Position]
neighbors position@(Position x y) = delete position [Position x' y' | x' <- [x, x+1, x-1], y' <- [y, y+1, y-1]]


alive_neighbors :: Generation -> Position -> Int
alive_neighbors generation position = length $ filter is_alive $ map generation $ neighbors position


evolution :: Generation -> Generation
-- evolution :: Generation -> Position -> CellState
evolution generation position =
          case (alive_neighbors generation position) of
               2 -> if (is_alive (generation position)) then Alive else Dead
               3 -> Alive
               _ -> Dead


visualize_generation generation = map (visualize_line generation) [1..10]

visualize_line :: Generation -> Integer -> String
visualize_line generation y = concat $ map (visualize_cell generation y) [1..10]

visualize_cell generation y x = case (generation (Position x y)) of
               Alive -> ['X']
               Dead -> [' ']


bar :: Position -> CellState
bar (Position 1 2) = Alive
bar (Position 2 2) = Alive
bar (Position 3 2) = Alive
bar (Position 3 3) = Alive
bar (Position _ _) = Dead

main = mapM_ print (visualize_generation bar) >>
     mapM_ print (visualize_generation (evolution bar)) >>
     mapM_ print (visualize_generation (evolution (evolution bar))) >>
     mapM_ print (visualize_generation (evolution (evolution (evolution bar)))) >>
     mapM_ print (visualize_generation (evolution (evolution (evolution (evolution bar)))))