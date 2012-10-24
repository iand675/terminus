{-# LANGUAGE OverloadedStrings #-}
module Terminus where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.List
import Data.Monoid
import Data.Word

csi = string8 "\ESC["

lineIx l = csi <> word8Dec l

cursorPostion :: Word8 -> Word8 -> Builder
cursorPostion line col = csi <> word8Dec line <> char8 ';' <> word8Dec col <> char8 'H'

cursorUp, cursorDown, cursorBackward, cursorForward :: Word8 -> Builder

cursorUp l = lineIx l <> char8 'A'
cursorDown l = lineIx l <> char8 'B'
cursorBackward l = lineIx l <> char8 'C'
cursorForward l = lineIx l <> char8 'D'

saveCursorPosition = csi <> char8 's'
restoreCursorPosition = csi <> char8 'u'
eraseDisplay = csi <> string8 "2J"
eraseLine = csi <> char8 'K'

data ANSIColor = Black
               | Red
               | Green
               | Yellow
               | Blue
               | Magenta
               | Cyan
               | White

data TextAttribute = Normal
                   | Bold
                   | Italic
                   | Underline
                   | Blink
                   | InverseColors

data GraphicsMode = Foreground ANSIColor
                  | Background ANSIColor
                  | TextAttribute TextAttribute

graphicParam' :: GraphicsMode -> Word8
graphicParam' (Foreground color) = case color of
  Black   -> 30
  Red     -> 31
  Green   -> 32
  Yellow  -> 33
  Blue    -> 34
  Magenta -> 35
  Cyan    -> 36
  White   -> 37
graphicParam' (Background color) = graphicParam' (Foreground color) + 10
graphicParam' (TextAttribute attr) = case attr of
  Normal        -> 0
  Bold          -> 1
  Italic        -> 3
  Underline     -> 4
  Blink         -> 5
  InverseColors -> 7

graphicParam = word8Dec . graphicParam'

term = C.putStr . toLazyByteString
withStyle s m = term (graphicsMode s) >> m >> term (graphicsMode [TextAttribute Normal])

graphicsMode :: [GraphicsMode] -> Builder
graphicsMode ms = csi <> mconcat (intersperse (char8 ';') $ map graphicParam ms) <> char8 'm'

main = C.putStr (toLazyByteString $ cursorPostion 10 10) >> putStrLn "Hello!" >> C.putStr (toLazyByteString $ cursorUp 9) >> getLine
