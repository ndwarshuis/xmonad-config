module SendXMsg (sendXMsg, splitXMsg) where

import Data.Char

import Graphics.X11.Types
import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras

sendXMsg :: String -> String -> IO ()
sendXMsg magic tag = do
  dpy <- openDisplay ""
  root <- rootWindow dpy $ defaultScreen dpy
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    -- NOTE: This function is written such that the penultimate
    -- argument represents the first 40 bits of the 160 bit data
    -- field, and it also only takes a decimal digit, which means the
    -- string to be stored in the data field needs to be converted to
    -- its decimal equivalent. The penultimate argument will be used
    -- for the magic string and the last will be used for the tag.
    setClientMessageEvent e root bITMAP 8 m t
    sendEvent dpy root False substructureNotifyMask e
  flush dpy
  where
    m = str2digit magic
    t = str2digit $ tag ++ [garbageDelim]

-- WORKAROUND: setClientMessageEvent seems to put garbage on the end
-- of the data field (which is probably some yucky c problem I don't
-- understand). Easy solution, put something at the end of the tag to
-- separate the tag from the garbage
garbageDelim :: Char
garbageDelim = '~'

splitXMsg :: (Integral a) => [a] -> (String, String)
splitXMsg s = (magic, filter isAlphaNum . takeWhile (/= garbageDelim) $ tag)
  where
    (magic, tag) = splitAt 5 $ map (chr . fromInteger . toInteger) s

str2digit :: (Num a) => String -> a
str2digit = fromIntegral
  . sum
  . map (\(p, n) -> n * 256 ^ p)
  . zip [0 :: Int ..]
  . map fromEnum
