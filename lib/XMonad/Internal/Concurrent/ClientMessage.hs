--------------------------------------------------------------------------------
-- | Core ClientMessage module to 'achieve' concurrency in XMonad
--
-- Since XMonad is single threaded, the only way to have multiple threads that
-- listen/react to non-X events is to spawn other threads the run outside of
-- XMonad and send ClientMessages back to it to be intercepted by the event
-- hook. This module has the core plumbing to make this happen.
--
-- The clientMessages to be sent will have a defined atom (that hopefully won't
-- do anything) and be sent to the root window. It will include two 'fields',
-- the first of which will represent the 'type' of message sent (meaning the
-- type of non-X event that was intercepted) and the second containing the data
-- pertaining to said event.

-- TODO come up with a better name than 'XMsg' since it sounds vague and too
-- much like something from X even though it isn't

module XMonad.Internal.Concurrent.ClientMessage
  ( XMsgType(..)
  , sendXMsg
  , splitXMsg
  ) where

import           Data.Char

import           Graphics.X11.Types
import           Graphics.X11.Xlib.Atom
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Event
import           Graphics.X11.Xlib.Extras

--------------------------------------------------------------------------------
-- | Data structure for the ClientMessage
--
-- These are the "types" of client messages to send; add more here as needed

-- TODO is there a way to do this in the libraries that import this one?
data XMsgType = ACPI
    | Workspace
    deriving (Eq, Show)

instance Enum XMsgType where
  toEnum 0 = ACPI
  toEnum 1 = Workspace
  toEnum _ = errorWithoutStackTrace "ACPI.Enum.ACPIEvent.toEnum: bad argument"

  fromEnum ACPI      = 0
  fromEnum Workspace = 1

--------------------------------------------------------------------------------
-- | Internal functions

str2digit :: String -> Time
str2digit = fromIntegral
  . sum
  . map (\(p, n) -> n * 256 ^ p)
  . zip [0 :: Int ..]
  . map fromEnum

-- WORKAROUND: setClientMessageEvent seems to put garbage on the end
-- of the data field (which is probably some yucky c problem I don't
-- understand). Easy solution, put something at the end of the tag to
-- separate the tag from the garbage
garbageDelim :: Char
garbageDelim = '~'

--------------------------------------------------------------------------------
-- | Exported API

-- | Given a string from the data field in a ClientMessage event, return the
-- type and payload
splitXMsg :: (Integral a) => [a] -> (XMsgType, String)
splitXMsg msg = (xtype, tag)
  where
    xtype = toEnum $ fromInteger $ toInteger $ head msg
    tag = filterGarbage $ mapToChr $ drop 5 msg
    filterGarbage = filter isAlphaNum . takeWhile (/= garbageDelim)
    mapToChr = map (chr . fromInteger . toInteger)

-- | Emit a ClientMessage event to the X server with the given type and payloud
sendXMsg :: XMsgType -> String -> IO ()
sendXMsg xtype tag = do
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
    setClientMessageEvent e root bITMAP 8 x t
    sendEvent dpy root False substructureNotifyMask e
  flush dpy
  closeDisplay dpy
  where
    x = fromIntegral $ fromEnum xtype
    t = str2digit $ tag ++ [garbageDelim]
