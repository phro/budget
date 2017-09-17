module Budget
  (Widget
  ,widget
  ,day
  ,cost
  ,description
  ,timeSpan
  ,costPerDay
  ,costPerWeek
  ,costPerMonth
  ,since
  ) where
import Data.Time.Calendar
type Description = String
type Cost = Float
data Widget = Widget
  {day :: Day
  ,cost :: Cost
  ,description :: Description
  } deriving (Eq,Show)

widget :: Integer -> Int -> Int -> Cost -> Description -> Widget
widget y m d c de = Widget {day = (fromGregorian y m d), cost = c, description = de}

timeSpan :: [Widget] -> Integer
timeSpan = ((-) <$> maximum <*> minimum) . (map (toModifiedJulianDay . day))

costPerDay :: [Widget] -> Float
costPerDay = (/) <$> (sum . map cost) <*> ((1+) . fromInteger. timeSpan)

costPerWeek :: [Widget] -> Float
costPerWeek = (7*). costPerDay

costPerMonth :: [Widget] -> Float
costPerMonth = (30.4167*). costPerDay

since :: Integer -> Int -> Int -> [Widget] -> [Widget]
since y m d = filter $ (fromGregorian y m d <=) . day


--[costPerWeek $ filter (((fromGregorian 2017 09 n)<=).day) groceries | n <- [3..16]]

--cost :: [Widget] -> Cost
--cost = foldr (\w s -> getCost w + s) 0
-- Insert useful budgeting functions here

