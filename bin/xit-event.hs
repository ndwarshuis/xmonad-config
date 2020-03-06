-- | Send a special event as a signal to the window manager
-- Specifically, this is meant to be run after applications exit which
-- will allow xmonad to react to processes closing. It takes two
-- arguments: a string called the "magic string" up to 5 characters
-- and a string up to 15 characters called the "tag." These will be
-- concatenated and sent to xmonad in a ClientRequest event of type
-- BITMAP (which hopefully will never do anything) to the root window.
-- Operationally, the magic string is meant to be used to
-- differentiate this event and the tag is meant to be a signal to be
-- read by xmonad.

import Graphics.X11.Types
import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras

import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse [magic, tag] = send magic tag >> exitSuccess
parse _ = exitFailure

send :: String -> String -> IO ()
send magic tag = do
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
    t = str2digit tag

str2digit :: (Num a) => String -> a
str2digit = fromIntegral
  . sum
  . map (\(p, n) -> n * 256 ^ p)
  . zip [0 :: Int ..]
  . map fromEnum
