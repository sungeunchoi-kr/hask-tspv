module Draw where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified Data.Map as Map
import TSPData

data TraversalData = TraversalData Float Float [Int] deriving (Show)
data SimulationState = SimulationState {
        graph :: Graph
        , cities :: [(TSPData.NodeLbl, TSPData.Loc)]
        , dataPast :: [TraversalData]
        , dataCurrent :: TraversalData
        , dataFuture :: [TraversalData]
    }

window :: Display
window = InWindow "Test" (400,400) (20,20)

background :: Color
background = white

drawing :: Picture
drawing = Circle 80

runVisualizer :: TSPData.Graph -> [TraversalData] -> IO ()
runVisualizer graph travs = simulate window background 50 (stateCons graph travs) render step 
    where
        stateCons :: TSPData.Graph -> [TraversalData] -> SimulationState
        stateCons g t = SimulationState g (Map.assocs g) [] (head t) (tail t)
    

--simulate :: Display -- ^ How to display the game.
--         -> Color   -- ^ Background color.
--         -> Int     -- ^ Number of simulation steps to take per second of real time.
--         -> a       -- ^ The initial state. 
--         -> (a -> Picture) -- ^ A function to render the state to a picture. 
--         -> (ViewPort -> Float -> a -> a) -- ^ A function to step the game once. 
--         -> IO ()

drawCities :: (TSPData.NodeLbl, TSPData.Loc) -> Picture
drawCities (label, Loc x y) = pictures [
    translate x y $ circleSolid 6]

drawTravelLines :: Graph -> [Int] -> [Picture]
drawTravelLines graph labelsPath =
    let points' = (\i -> Map.findWithDefault (Loc 0 0) i graph) <$> labelsPath
        points = (\(Loc x y) -> (x,y)) <$> points' in
    [Line points]

extractPath (TraversalData _ _ ps) = ps

render :: SimulationState -> Picture
render st = pictures $
       ( drawCities <$> (cities st) )
    ++ ( drawTravelLines (graph st) (extractPath $ dataCurrent st) )

step :: ViewPort -> Float -> SimulationState -> SimulationState
step viewport dt (SimulationState g c dp dc []) =
    SimulationState g c [] (head dp) (tail dp)
step viewport dt (SimulationState g c dp dc df) =
    SimulationState g c (dp ++ [dc]) (head df) (tail df)

