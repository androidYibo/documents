module EddyAst where

type Script = [Command]
data Command = Ins String
             | Del
             | Next
             | Prev
             | Repeat Int Command
             | Buffer BufferName
             | Remove
             | Macro MacroName Script
             | Call MacroName
             deriving (Eq, Show)
type BufferName = String
type MacroName = String
