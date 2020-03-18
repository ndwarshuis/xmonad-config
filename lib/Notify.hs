{-# LANGUAGE LambdaCase #-}

module Notify
  ( Note(..)
  , Body(..)
  , defNote
  , defNoteInfo
  , defNoteError
  , fmtNotifyCmd
  )
  where

import Shell

import Data.Maybe

import DBus.Notify

defNote = blankNote { summary = "\"xmonad\"" }

defNoteInfo = defNote
  { appImage = Just $ Icon "dialog-information-symbolic" }

defNoteError = defNote
  { appImage = Just $ Icon "dialog-error-symbolic" }

parseBody :: Body -> Maybe String
parseBody (Text s) = Just s
parseBody _ = Nothing

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
