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

import SendXMsg

import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse [magic, tag] = sendXMsg magic tag >> exitSuccess
parse _ = exitFailure
