{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- | VirtualBox-specific functions

module XMonad.Internal.Concurrent.VirtualBox
  ( vmExists
  ) where

import           Control.Exception

import           Text.XML.Light

import           System.Directory

import           XMonad.Internal.Shell

vmExists :: String -> IO (Maybe String)
vmExists vm = do
  d <- vmDirectory
  either (return . Just) findVMDir d
  where
    findVMDir vd = do
      vs <- listDirectory vd
      return $ if vm `elem` vs then Nothing
        else Just $ "could not find " ++ singleQuote vm

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
    qual e n = (elName e) { qName = n }

vmConfig :: IO FilePath
vmConfig = getXdgDirectory XdgConfig "VirtualBox/VirtualBox.xml"
