module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day(..), fromGregorianValid, toGregorian)
import Data.List (find)
import Data.Maybe (fromJust, catMaybes)

data Schedule = First | Second | Third | Fourth | Last | Teenth deriving (Eq, Show)
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Show, Read)

instance Enum Weekday where
    toEnum i =
        case mod i 7 of
            0 -> Sunday
            1 -> Monday
            2 -> Tuesday
            3 -> Wednesday
            4 -> Thursday
            5 -> Friday
            _ -> Saturday
    fromEnum Monday = 1
    fromEnum Tuesday = 2
    fromEnum Wednesday = 3
    fromEnum Thursday = 4
    fromEnum Friday = 5
    fromEnum Saturday = 6
    fromEnum Sunday = 7
    enumFromTo wd1 wd2
        | wd1 == wd2 = [wd1]
    enumFromTo wd1 wd2 = wd1 : enumFromTo (succ wd1) wd2
    enumFromThenTo wd1 wd2 wd3
        | wd2 == wd3 = [wd1, wd2]
    enumFromThenTo wd1 wd2 wd3 = wd1 : enumFromThenTo wd2 (toEnum $ (2 * fromEnum wd2) - (fromEnum wd1)) wd3

type Year = Integer
type Month = Int

meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay s wd y m = scheduleDay s $ candidateDays (monthDays y m) wd

dayOfWeek :: Day -> Weekday
dayOfWeek (ModifiedJulianDay d) = toEnum $ fromInteger $ d + 3

monthDays :: Year -> Month -> [(Weekday, Day)]
monthDays y m = zip (map dayOfWeek d) d
    where d = catMaybes $ zipWith3 fromGregorianValid (repeat y) (repeat m) [1..31]

candidateDays :: [(Weekday, Day)] -> Weekday -> [Day]
candidateDays x weekday = map snd $ filter (\i -> fst i == weekday) x

scheduleDay :: Schedule -> [Day] -> Day
scheduleDay s xs 
    | s == First = head xs
    | s == Second = xs !! 1
    | s == Third = xs !! 2
    | s == Fourth = xs !! 3
    | s == Last = last xs
    | s == Teenth = fromJust $ find (teen . toGregorian) xs
    where   teen i = get3rd i >= 13 && get3rd i <=19
            get3rd (_,_,x) = x
