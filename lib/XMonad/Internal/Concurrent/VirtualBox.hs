{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- | VirtualBox-specific functions

module XMonad.Internal.Concurrent.VirtualBox
  ( vmExists
  , vmInstanceConfig
  , qual
  ) where

import           Control.Exception

import           Data.Internal.Dependency

import           Text.XML.Light

import           System.Directory
import           System.FilePath

import           XMonad.Internal.Shell

vmExists :: String -> IO (Maybe Msg)
vmExists vm = either (const Nothing) (Just . Msg Error) <$> vmInstanceConfig vm

vmInstanceConfig :: String -> IO (Either String FilePath)
vmInstanceConfig vmName = do
  either (return . Right) findInstance =<< vmDirectory
  where
    path = vmName </> (vmName ++ ".vbox")
    findInstance dir = do
      res <- findFile [dir] path
      return $ case res of
        Just p  -> Right p
        Nothing -> Left $ "could not find VM instance: " ++ singleQuote vmName

vmDirectory :: IO (Either String String)
vmDirectory = do
  p <- vmConfig
  (s :: Either IOException String) <- try $ readFile p
  return $ case s of
    (Left _) -> Left "could not read VirtualBox config file"
    (Right x) -> maybe (Left "Could not parse VirtualBox config file") Right
                 $ findDir =<< parseXMLDoc x
  where
    findDir e = findAttr (unqual "defaultMachineFolder")
      =<< findChild (qual e "SystemProperties")
      =<< findChild (qual e "Global") e

qual :: Element -> String -> QName
qual e n = (elName e) { qName = n }

vmConfig :: IO FilePath
vmConfig = getXdgDirectory XdgConfig "VirtualBox/VirtualBox.xml"
