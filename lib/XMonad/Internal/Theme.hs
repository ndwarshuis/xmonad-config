--------------------------------------------------------------------------------
-- | Theme for XMonad and Xmobar

module XMonad.Internal.Theme
  ( baseColor
  , bgColor
  , fgColor
  , bordersColor
  , warningColor
  , errorColor
  , selectedFgColor
  , selectedBgColor
  , selectedBordersColor
  , backdropBaseColor
  , backdropFgColor
  , backdropTextColor
  , blend'
  , darken'
  , Slant(..)
  , Weight(..)
  , ThemeFont(..)
  , fmtFontXFT
  , font
  , tabbedTheme
  , promptTheme
  ) where

import           Data.Char
import           Data.Colour
import           Data.Colour.SRGB
import           Data.List

import qualified XMonad.Layout.Decoration as D
import qualified XMonad.Prompt            as P

--------------------------------------------------------------------------------
-- | Colors - vocabulary roughly based on GTK themes

baseColor :: String
baseColor = "#f7f7f7"

bgColor :: String
bgColor = "#d6d6d6"

fgColor :: String
fgColor = "#2c2c2c"

bordersColor :: String
bordersColor = darken' 0.3 bgColor

warningColor :: String
warningColor = "#ffca28"

errorColor :: String
errorColor = "#e53935"

selectedFgColor :: String
selectedFgColor = "#ffffff"

selectedBgColor :: String
selectedBgColor = "#3399ff"

selectedBordersColor :: String
selectedBordersColor = "#006cd9"

backdropBaseColor :: String
backdropBaseColor = baseColor

backdropTextColor :: String
backdropTextColor = blend' 0.95 fgColor backdropBaseColor

backdropFgColor :: String
backdropFgColor = blend' 0.75 fgColor bgColor

--------------------------------------------------------------------------------
-- | Color functions

blend' :: Float -> String -> String -> String
blend' wt c0 c1 = sRGB24show $ blend wt (sRGB24read c0) (sRGB24read c1)

darken' :: Float -> String -> String
darken' wt = sRGB24show . darken wt . sRGB24read

--------------------------------------------------------------------------------
-- | Fonts

data Slant = Roman
    | Italic
    | Oblique
    deriving (Eq, Show)

data Weight = Light
    | Medium
    | Demibold
    | Bold
    | Black
    deriving (Eq, Show)

data ThemeFont = ThemeFont
    { family    :: String
    , weight    :: Maybe Weight
    , slant     :: Maybe Slant
    , size      :: Maybe Int
    , pixelsize :: Maybe Int
    , antialias :: Maybe Bool
    }

fmtFontXFT :: ThemeFont -> String
fmtFontXFT ThemeFont
  { family = f
  , weight = w
  , slant = l
  , size = s
  , pixelsize = p
  , antialias = a
  } = intercalate ":" $ ["xft", f] ++ elems
  where
    elems = [ k ++ "=" ++ v | (k, Just v) <- [ ("weight", showLower w)
                                             , ("slant", showLower l)
                                             , ("size", showLower s)
                                             , ("pixelsize", showLower p)
                                             , ("antialias", showLower a)
                                             ]
                            ]
    showLower :: Show a => Maybe a -> Maybe String
    showLower = fmap (fmap toLower . show)

font :: ThemeFont
font = ThemeFont
  { family = "DejaVu Sans"
  , size = Just 10
  , antialias = Just True
  , weight = Nothing
  , slant = Nothing
  , pixelsize = Nothing
  }

--------------------------------------------------------------------------------
-- | Complete themes

tabbedTheme :: D.Theme
tabbedTheme = D.def
  { D.fontName              = fmtFontXFT font { weight = Just Bold }

  , D.activeTextColor       = fgColor
  , D.activeColor           = bgColor
  , D.activeBorderColor     = bgColor

  , D.inactiveTextColor     = backdropTextColor
  , D.inactiveColor         = backdropFgColor
  , D.inactiveBorderColor   = backdropFgColor

  , D.urgentTextColor       = darken' 0.5 errorColor
  , D.urgentColor           = errorColor
  , D.urgentBorderColor     = errorColor

  -- this is in a newer version
  -- , D.activeBorderWidth     = 0
  -- , D.inactiveBorderWidth   = 0
  -- , D.urgentBorderWidth     = 0

  , D.decoHeight            = 20
  , D.windowTitleAddons     = []
  , D.windowTitleIcons      = []
}

promptTheme :: P.XPConfig
promptTheme = P.def
  { P.font              = fmtFontXFT font
  , P.bgColor           = bgColor
  , P.fgColor           = fgColor
  , P.fgHLight          = selectedFgColor
  , P.bgHLight          = selectedBgColor
  , P.borderColor       = bordersColor
  , P.promptBorderWidth = 1
  , P.height            = 30
  , P.position          = P.CenteredAt 0.5 0.5
  }