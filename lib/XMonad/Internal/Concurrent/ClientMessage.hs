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
    | Unknown
    deriving (Eq, Show)

instance Enum XMsgType where
  toEnum 0 = ACPI
  toEnum 1 = Workspace
  toEnum _ = errorWithoutStackTrace "ACPI.Enum.ACPIEvent.toEnum: bad argument"

  fromEnum ACPI      = 0
  fromEnum Workspace = 1
  fromEnum Unknown   = 2

--------------------------------------------------------------------------------
-- | Exported API

-- | Given a string from the data field in a ClientMessage event, return the
-- type and payload
splitXMsg :: (Integral a) => [a] -> (XMsgType, String)
splitXMsg [] = (Unknown, "")
splitXMsg (x:xs) = (xtype, tag)
  where
    xtype = toEnum $ fromInteger $ toInteger x
    tag = map (chr . fromInteger . toInteger) $ takeWhile (/= 0) xs

-- | Emit a ClientMessage event to the X server with the given type and payloud
sendXMsg :: XMsgType -> String -> IO ()
sendXMsg xtype tag = do
  dpy <- openDisplay ""
  root <- rootWindow dpy $ defaultScreen dpy
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    -- Set the client message for the root window to be something hacky and
    -- somewhat random that can be intercepted later in the main xmonad thread.
    --
    -- Use the bitmap type since this is (hopefully) not going to be used by any
    -- apps I'm using (let alone the root window). If this isn't the case, the
    -- logs will have some really weird messages in them. The format field is
    -- set to 8 (eg one byte) which allows direct conversion between Word8 types
    -- and chars/string. The last argument is a list of data, where the first
    -- character represents the message type (Workspace vs ACPI) and the
    -- remaining members represent the tag. Note that the data payload for the
    -- message is 20 bytes, so the tag can be 19 characters long. Anything
    -- longer will be clipped to 19, and anything less than 19 will be padded
    -- with 0 (note this used to be random garbage before). See this function
    -- for more details.
    setClientMessageEvent' e root bITMAP 8 (x:t)
    sendEvent dpy root False substructureNotifyMask e
  flush dpy
  closeDisplay dpy
  where
    x = fromIntegral $ fromEnum xtype
    t = fmap (fromIntegral . fromEnum) tag
