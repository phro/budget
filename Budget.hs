module Budget
  (Widget
  ,widget
  ,totalCost
  ) where
import Data.Time.Calendar
type Description = String
type Cost = Float
data Widget = Widget {getDay :: Day, getCost :: Cost, getDescription :: Description} deriving (Eq,Show)

widget :: Integer -> Int -> Int -> Cost -> Description -> Widget
widget y m d c de = Widget {getDay = (fromGregorian y m d), getCost = c, getDescription = de}

totalCost :: [Widget] -> Cost
totalCost = foldr (\w s -> getCost w + s) 0
-- Insert useful budgeting functions here

