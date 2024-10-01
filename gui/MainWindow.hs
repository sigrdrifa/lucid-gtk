{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | MainWindow module definition, acting as the top level container
of the GUI application.
-}
module MainWindow where

import AppState (AppState)
import ConvertView (ConvertView (convertViewBox), initConvertView)
import Data.Text qualified as T
import GI.Adw (AttrOp ((:=)), new)
import GI.Adw qualified as Adw
import GI.Gio (MenuItem (..), menuItemSetLabel)
import GI.Gio qualified as Gtk
import GI.Gtk qualified as Gtk

-- | MainWindow ADT that holds boxed pointers to relevant components
data MainWindow
  = MainWindow
  { application :: !Adw.Application
  , appState :: AppState
  , toastOverlay :: !Adw.ToastOverlay
  , window :: !Adw.ApplicationWindow
  , convertView :: !ConvertView
  }

{- | Initialises the MainWindow by instantiating the sub-views and
the Adwaita ApplicationWindow instance.
-}
initMainWindow :: Adw.Application -> AppState -> IO MainWindow
initMainWindow app state = do
  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  overlay <- new Adw.ToastOverlay [#child := content]
  stack <- new Adw.ViewStack [#hexpand := True]
  window <-
    new
      Adw.ApplicationWindow
      [ #application := app
      , #content := overlay
      , #defaultWidth := 1220
      , #defaultHeight := 800
      ]

  -- instantiate sub-views
  let welcomeTitle = "Lucid-gtk " <> "0.1"

  welcomeBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

  convertView <- initConvertView state overlay
  let convViewBox = convertViewBox convertView

  welcomePage <-
    new
      Adw.StatusPage
      [ #iconName := "org.gnome.Adwaita1.Demo"
      , #title := T.pack welcomeTitle
      , #description := "Convert HTML5 markup to "
          <> "valid Lucid2 Haskell code inside a GUI"
      , #child := welcomeBox
      ]
  Adw.viewStackAddTitledWithIcon
    stack
    welcomePage
    (Just "welcome-page")
    "Load"
    "audio-x-generic"
  Adw.viewStackAddTitledWithIcon
    stack
    convViewBox
    (Just "convert-page")
    "Convert"
    "sound-wave-symbolic"
  viewSwitcherBar <- new Adw.ViewSwitcherBar [#stack := stack]
  viewSwitcherTitle <- new Adw.ViewSwitcherTitle [#stack := stack]
  headerBar <- new Adw.HeaderBar [#titleWidget := viewSwitcherTitle]

  -- configure top left menu
  menuModelItem <- new MenuItem []
  menuItemSetLabel menuModelItem (Just $ T.pack "Toggle Light/Dark")
  menuModelItemAbout <- new MenuItem []
  menuItemSetLabel menuModelItemAbout (Just $ T.pack "About")

  menuModel <- new Gtk.Menu []
  Gtk.menuAppendItem menuModel menuModelItem
  Gtk.menuAppendItem menuModel menuModelItemAbout
  menuBtn <- new Gtk.MenuButton [#menuModel := menuModel, #iconName := "open-menu-symbolic"]
  Adw.headerBarPackStart headerBar menuBtn
  content.append headerBar
  content.append stack
  content.append viewSwitcherBar
  let mw = MainWindow app state overlay window convertView
  pure mw