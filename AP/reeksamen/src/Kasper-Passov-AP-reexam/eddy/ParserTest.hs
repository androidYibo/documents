module ParserTest (test) where

import EddyParser
import EddyAst

testStrings :: [String]
testStrings = ["i 6|World! prev i 5 |Hello" --The two given examples
              ,"buffer world i 2|to i 2|my i 6|World! prev i 25|wonderful, yet terrifyingmacro top {prev * 5000 * 2} top i 7|Welcome"
              ,"    i        9  |   input     " -- arbitrary  whitespaces
              ,"del * 5 * 5" -- nested repeat 
              ,"*_A123_*/?." -- symbols for buffer 
              ]

resultStrings :: [Either Error Script]
resultStrings = [Right [Ins "World!", Prev, Ins "Hello"] -- right output of examples
                ,Right [Buffer "world", Ins "to", Ins "my", Ins "World!", Prev
                       ,Ins "wonderful, yet terrifying", Macro "top" [Repeat 2 (Repeat 5000 Prev)]
                       ,Call "top" ,Ins "Welcome"]
                ,Right [Ins "   input "] 
                ,Right [Repeat 5 (Repeat 5 Del)] 
                ,Right [Call "*_A123_*/?."]
                ]
test :: Bool
test = foldl (\ x y -> x && y ) True list 
      where list = runTests testStrings resultStrings

runTests :: [String] -> [Either Error Script] -> [Bool]
runTests = zipWith (\x y -> parse x == y) 


