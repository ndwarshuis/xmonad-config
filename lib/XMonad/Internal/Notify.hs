--------------------------------------------------------------------------------
-- | Functions for formatting and sending notifications
--
-- NOTE I use the DBus.Notify lib even though I don't actually use the DBus for
-- notifications (just formation them into 'notify-send' commands and spawn a
-- shell since that works more consistently with my current commands). If I ever
-- decide to switch to using the DBus it will be easy.

module XMonad.Internal.Notify
  ( Note(..)
  , Body(..)
  , defNote
  , defNoteInfo
  , defNoteError
  , fmtNotifyCmd
  ) where

import           Data.Maybe

import           DBus.Notify

import           XMonad.Internal.Shell

--------------------------------------------------------------------------------
-- | Some nice default notes

defNote :: Note
defNote = blankNote { summary = "\"xmonad\"" }

defNoteInfo :: Note
defNoteInfo = defNote
  { appImage = Just $ Icon "dialog-information-symbolic" }

defNoteError :: Note
defNoteError = defNote
  { appImage = Just $ Icon "dialog-error-symbolic" }

--------------------------------------------------------------------------------
-- | Format a 'notify-send' command to be send to the shell

parseBody :: Body -> Maybe String
parseBody (Text s) = Just s
parseBody _        = Nothing

fmtNotifyCmd :: Note -> String
fmtNotifyCmd note =
  fmtCmd "notify-send" $ getIcon note
    ++ getSummary note
    ++ getBody note
  where
    -- TODO add the rest of the options as needed
    getSummary = (:[]) . quote . summary
    getIcon n = maybe [] (\i -> ["-i", case i of { Icon s -> s; File s -> s }])
      $ appImage n
    getBody n = maybeToList $ (fmap quote . parseBody) =<< body n
    quote s = "\"" ++ s ++ "\""
