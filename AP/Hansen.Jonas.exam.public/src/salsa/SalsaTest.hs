{-
  Various test programs to compile and interpret
-}
module SalsaTest where

import qualified SalsaParser as SP
import qualified SalsaInterp as SI
import Gpx

tProgram1 :: String
tProgram1 = 
  "viewdef Default 400 400 \
  \rectangle box 10 400 20 20 green \
  \box -> (10, 200) \
  \box -> +(100, 0) \
  \box -> (110,400) \
  \box -> +(0-100, 0)"

tProgram2 :: String
tProgram2 = 
  "viewdef One 500 500 \
  \viewdef Two 400 400 \
  \group Both [One Two] \
  \view Both \
  \rectangle larry 10 350 20 20 blue \
  \rectangle fawn 300 350 15 25 plum \
  \view Two \
  \larry -> (300, 350) || fawn -> (10,350) \
  \view Both \
  \larry fawn -> +(0, 0 - 300)"

tProgram3 :: String  
tProgram3 = 
  "viewdef Main 400 400 \
  \circle a 10 200 50 red \
  \a -> (200, 350) \
  \rectangle b 20 20 100 15 blue \
  \rectangle c 380 400 10 300 plum \
  \c -> (200, 350) || a -> +(0-100, 0) \
  \rectangle d 195 205 10 10 green "
  
runWrite :: FilePath -> String -> Integer -> IO ()
runWrite filename p n = writeFile filename $ show $ run p n
    
run :: String -> Integer -> Animation
run p n =
  case SP.parseString p of
    Right ast -> SI.runProg n ast
    Left _ -> error "Parsing failed"
  