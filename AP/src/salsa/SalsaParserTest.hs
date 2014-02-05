module SalsaParserTest (test) where

import SalsaParser
import SalsaAst

testStrings :: [String]
testStrings = ["{h -> (3+3,3)}", -- 
               "h -> (3,3) || a -> (1,2) @ Something", -- testing precedence of || and @
               "group Group [First Secound Third]", -- testing VIdents (multiple)
               "circle c 10 (13 - 41) a90_1.x red", -- testing circle and def
               "viewdef Default 400 400 rectangle box 10 400 20 20 green box -> (10, 200) box -> +(100, 0) box -> (110,400) box -> +(0-100, 0)", -- Appendix A
               "viewdef One 500 500 viewdef Two 400 400 group Both [One Two] view Both rectangle larry 10 350 20 20 blue rectangle fawn 300 350 15 25 plum view Two larry -> (300, 350) || fawn -> (10,350) view Both larry fawn -> +(0, 0 - 300)" -- Appendix B
              ]

resultStrings :: [Either Error Program]
resultStrings = [Right [Com (Move ["h"] (Abs (Plus (Const 3) (Const 3)) (Const 3)))],
                 Right [Com (Par (Move ["h"] (Abs (Const 3) (Const 3))) (At (Move ["a"] (Abs (Const 1) (Const 2))) "Something"))],
                 Right [Def (Group "Group" ["First","Secound","Third"])],
                 Right [Def (Circle "c" (Const 10) (Minus (Const 13) (Const 41)) (Xproj "a90_1") Red)],
                 Right [ Def (Viewdef "Default" (Const 400) (Const 400)), Def (Rectangle "box" (Const 10) (Const 400)(Const 20) (Const 20) Green), Com (Move ["box"] (Abs (Const 10) (Const 200))), Com (Move ["box"] (Rel (Const 100) (Const 0))), Com (Move ["box"] (Abs (Const 110) (Const 400))), Com (Move ["box"] (Rel (Minus (Const 0) (Const 100))(Const 0)))],
                 Right [ Def (Viewdef "One" (Const 500) (Const 500)), Def (Viewdef "Two" (Const 400) (Const 400)), Def (Group "Both" ["One","Two"]), Def (View "Both"), Def (Rectangle "larry" (Const 10) (Const 350)(Const 20) (Const 20) Blue), Def (Rectangle "fawn" (Const 300) (Const 350)(Const 15) (Const 25) Plum), Def (View "Two"), Com (Par (Move ["larry"] (Abs (Const 300) (Const 350)))(Move ["fawn"] (Abs (Const 10) (Const 350)))), Def (View "Both"), Com (Move ["larry","fawn"](Rel (Const 0) (Minus (Const 0) (Const 300))))]
                ]
type Error = String

test :: Bool
test = foldl (\ x y -> x && y) True list 
      where list = (runTests testStrings resultStrings)

runTests :: [String] -> [Either Error Program] -> [Bool]
runTests tests results = zipWith (\x y -> (parseString x) == y) tests results
