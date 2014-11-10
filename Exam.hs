module Exam where

import Data.List

--
-- Predefined types and a sample list of requests
--

type Name = String

type Address = String

type Time = Int

type TimeHM = (Int, Int)

type Request = (Name, Address, Time, Time)


rs :: [Request]
rs = [("Killer","11 Manton Av",1000,1130),
      ("Rex","4 Howard Court",1500,1615),
      ("Jaws","1 West Rd",1030,1115),
      ("Gnasher","16 Park St",1200,1330),
      ("Satan","Hamley Manor",900,1015),
      ("Fangs","19 Clover St",1400,1530),
      ("Preston","44 Main St",1145,1345),
      ("Chomp","9 Radley St",1545,1730)]

---------------------------------------------------------
--
-- HASKELL EXAM: FUNCTION HEADERS
-- Uncomment these headers as you go along. Do not otherwise change them.
--

startTime :: Request -> Time
-- Returns the start time of a request
startTime (_, _, st, _) = st

finishTime :: Request -> Time
-- Returns the finish time of a request
finishTime (_, _, _, ft) = ft

convertTime :: Int -> TimeHM
-- Converts time from HHMM format into (hrs, min) format
convertTime t = (div t 100, mod t 100)

subtractTimes :: Int -> Int -> Int
-- Finds the difference between 2 times in min
-- pre: t1 >= t2
subtractTimes t1 t2 = h*60 + m
  where
    (h1, m1) = convertTime t1
    (h2, m2) = convertTime t2
    (h, m)   = ( (h1 - h2), (m1 - m2) )

sortRequests :: [Request] -> [Request]
-- Sorts requests in ascending order of their finish time
sortRequests = (map swap) . sort . (map swap)
  where
    swap (a, b, c, d) = (d, b, c, a)

schedule :: [Request] -> [Request]
-- Returns the longest sequence of non-overlapping requests.
-- List needs to be reversed as stack accumulates in reverse order.
schdule  []   = []
schedule reqs = reverse (sch rs [r])
  where
   (r:rs) = sortRequests reqs 
   sch (x:xs) stack@(s:ss)
     | finishTime s < startTime x = sch xs (x:stack)
     | otherwise                  = sch xs (stack)
   sch _ stack = stack

breaks :: [Request] -> [Int]
-- Returns a list of time invervals between requests
-- pre: rs == schdule rs
breaks (r1:r2:rs) = timeDiff:( breaks (r2:rs) )
  where timeDiff  = subtractTimes (startTime r2) (finishTime r1)
breaks _          = []

