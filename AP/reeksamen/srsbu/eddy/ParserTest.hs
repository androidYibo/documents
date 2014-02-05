import EddyParser as EP
import EddyAst

testStrings = [
  "i 6|World!\nprev\ni 5 |Hello",
  "buffer myworld\ni 2|to\ni 2|my\ni 6|World!\nprev\ni 25|wonderful, yet terrifyingmacro top {prev * 5000 * 2}\ntop\ni 7|Welcome"
             ]

resultStrings = [
  [ Ins "World!", Prev, Ins "Hello"],
  [ Buffer "myworld", Ins "to", Ins "my", 
    Ins "World!", Prev, Ins "wonderful, yet terrifying", 
    Macro "top" [Repeat 2 (Repeat 5000 Prev)], 
    Call "top" ,Ins "Welcome"]
                ]

test = foldl (\ x y -> x ++ [run y]) [] testStrings

run p = case EP.parse p of
             Right ast -> ast
             Left _ -> error "Parsing failed"






































