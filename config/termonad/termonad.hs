{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromMaybe)

import Termonad
  ( FontConfig, FontSize(FontSizePoints), ShowTabBar(ShowTabBarAlways)
  , CursorBlinkMode(CursorBlinkModeOn), Option(Set)
  , ShowScrollbar(ShowScrollbarAlways), TMConfig, confirmExit, showTabBar
  , cursorBlinkMode, defaultConfigOptions, defaultFontConfig, defaultTMConfig
  , fontConfig, fontFamily, fontSize, options, showMenu, showScrollbar, start
  )

import Termonad.Config.Colour
  ( AlphaColour, ColourConfig, List8, Palette(ExtendedPalette), addColourExtension
  , createColour, createColourExtension, cursorBgColour, defaultColourConfig
  , defaultLightColours, foregroundColour, palette, mkList8, unsafeMkList8
  )

fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "Ubuntu Mono"
    , fontSize = FontSizePoints 16
    }

myTMConfig :: TMConfig
myTMConfig =
  defaultTMConfig
    { options =
        defaultConfigOptions
          { fontConfig = fontConf
          , showTabBar = ShowTabBarAlways
          , showScrollbar = ShowScrollbarAlways
          , confirmExit = False
          , showMenu = False
          , cursorBlinkMode = CursorBlinkModeOn
          }
    }

myColourConfig :: ColourConfig (AlphaColour Double)
myColourConfig =
  defaultColourConfig
    { cursorBgColour = Set (createColour 233 84 16)
    , foregroundColour = Set (createColour 174 167 159)
    , palette = ExtendedPalette myStandardColours
                                (fromMaybe defaultLightColours myLightColours)
    }
  where
    myStandardColours :: List8 (AlphaColour Double)
    myStandardColours = unsafeMkList8
      [ createColour  42   0  30
      , createColour 180  62  20
      , createColour  40 192  20
      , createColour 180 192  20
      , createColour  40  62 120
      , createColour 180  62 120
      , createColour  40 192 120
      , createColour 180 192 120
      ]

    myLightColours :: Maybe (List8 (AlphaColour Double))
    myLightColours = mkList8
        [ createColour  70  92  40
        , createColour 220  62  20
        , createColour  40 242  20
        , createColour 220 232  20
        , createColour  40  62 180
        , createColour 140  62  80
        , createColour  50 232 160
        , createColour 220 232 150
        ]

main :: IO ()
main = do
  myColourExt <- createColourExtension myColourConfig

  let newTMConfig = addColourExtension myTMConfig myColourExt

  start newTMConfig
