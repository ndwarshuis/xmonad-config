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

-- These are the "types" of client messages to send; add more here as needed
data XMsgType = ACPI
    | Workspace
    deriving (Eq, Show)

instance Enum XMsgType where
  toEnum 0 = ACPI
  toEnum 1 = Workspace
  toEnum _ = errorWithoutStackTrace "ACPI.Enum.ACPIEvent.toEnum: bad argument"

  fromEnum ACPI      = 0
  fromEnum Workspace = 1

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

str2digit :: String -> Time
str2digit = fromIntegral
  . sum
  . map (\(p, n) -> n * 256 ^ p)
  . zip [0 :: Int ..]
  . map fromEnum

splitXMsg :: (Integral a) => [a] -> (XMsgType, String)
splitXMsg msg = (xtype, tag)
  where
    xtype = toEnum $ fromInteger $ toInteger $ head msg
    tag = filterGarbage $ mapToChr $ drop 5 msg
    filterGarbage = filter isAlphaNum . takeWhile (/= garbageDelim)
    mapToChr = map (chr . fromInteger . toInteger)

-- WORKAROUND: setClientMessageEvent seems to put garbage on the end
-- of the data field (which is probably some yucky c problem I don't
-- understand). Easy solution, put something at the end of the tag to
-- separate the tag from the garbage
garbageDelim :: Char
garbageDelim = '~'
