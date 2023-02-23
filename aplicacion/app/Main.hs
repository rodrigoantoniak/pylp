{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.GI.Base
import Data.Char (toUpper, toLower)
import qualified Data.Text as T (pack, unpack)
import qualified GI.Gtk as Gtk
import System.Directory (getHomeDirectory)
import System.Posix.User (getEffectiveUserName)
import System.Process

main :: IO ()
main = do
  Gtk.init Nothing

  home <- getHomeDirectory
  user <- getEffectiveUserName

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  set win
    [ #borderWidth          := 10
    , #title                := "Sesión"
    , #defaultWidth         := 750
    , #defaultHeight        := 225
    , #resizable            := False
    , #windowPosition       := Gtk.WindowPositionCenter
    , #decorated            := False
    ]

  Gtk.onWidgetDestroy win Gtk.mainQuit

  grid <- Gtk.gridNew
  set grid
    [ #columnSpacing        := 10
    , #rowSpacing           := 10
    , #columnHomogeneous    := True
    ]

  #add win grid

  let prefix  = "/Descargas/byebye/img/"
      suffix  = ".png"
      choices = [ ("Cancelar",     pure ())
                , ("CerrarSesión", callCommand "killall xmonad-x86_64-linux")
                , ("Reiniciar",    callCommand "reboot")
                , ("Apagar",       callCommand "poweroff")
                , ("Suspender",    callCommand "systemctl suspend")
                , ("Hibernar",     callCommand "systemctl hibernate")
                , ("Bloquear",     callCommand "gnome-screensaver-command -al")
                ]

  let buttonWithImageAndLabel :: [(String, IO ())] -> IO ()
      buttonWithImageAndLabel [] = return ()
      buttonWithImageAndLabel (x:xs) = do
        image <- Gtk.imageNewFromFile $ home ++ prefix ++ (fst x) ++ suffix
        label <- Gtk.labelNew Nothing
        set label
          [ #label     := T.pack $ "<b>" ++ (fst x) ++ "</b>"
          , #useMarkup := True
          ]
        button <- Gtk.buttonNew
        set button
          [ #relief  := Gtk.ReliefStyleNone
          , #image   := image
          , #hexpand := False
          ]
        on button #clicked $ do
          Gtk.widgetDestroy win
          snd x
        let a = (length choices) - 1
            b = length xs
            col = fromIntegral $ a - b
        #attach grid button col 0 1 1
        #attach grid label  col 1 1 1
        buttonWithImageAndLabel xs

  buttonWithImageAndLabel choices

  #showAll win
  Gtk.main
