--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, PP(..))
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doCenterFloat, doFullFloat)
import XMonad.Layout.Fullscreen (fullscreenSupport)
import XMonad.Layout.IndependentScreens
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import XMonad.Wallpaper
import Data.Maybe (isJust)
import Data.Tree
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Exit

import qualified XMonad.Actions.TreeSelect as TS
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.Tuple.Extra as TE

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal :: String
myTerminal = "termonad-linux-x86_64"

myOtherTerminal :: String
myOtherTerminal = "st -f 'Ubuntu Mono-18' "

-- The preferred font
myFont :: String
myFont = "xft:Ubuntu Mono:pixelsize=20"

-- The preferred text editor
myEditor :: String
myEditor = "gedit -s "

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = True

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#2c001e"
myFocusedBorderColor = "#e95420"

------------------------------------------------------------------------
-- Tree Select. Add, modify or remove menu options here.
myPlayers :: [(String, String, String)]
myPlayers = [ ("MPV", "mpv --player-operation-mode=pseudo-gui --", "Reproductor multimedia MPV")
            , ("VLC", "vlc", "Reproductor multimedia VideoLAN Client")
            ]

myBrowsers :: [(String, String, String)]
myBrowsers = [ ("Firefox", "firefox", "Navegador Web Firefox")
             ]
             
myConfigs :: [(String, String, String)]
myConfigs = [ ("Bash", myEditor ++ "~/.bashrc", "Configuración de bash")
            , ("HTop", myEditor ++ "~/.config/htop/htoprc", "Configuración de htop")
            , ("NeoFetch", myEditor ++ "~/.config/neofetch/config.conf", "doom emacs init")
            , ("Rofi", myEditor ++ "~/.config/rofi/config.rasi", "Configuración de Rofi")
            , ("Termonad", myEditor ++ "~/.config/termonad/termonad.hs", "Configuración de Termonad")
            , ("XMobar", myEditor ++ "~/.config/xmobar/xmobarrc", "Configuración de XMobar")
            , ("XMonad", myEditor ++ "~/.xmonad/xmonad.hs", "Configuración de XMonad")
            ]
            
myControls :: [(String, String, String)]
myControls = [ ("Entradas", myOtherTerminal ++ "-T nspHtop -e nautilus", "Control de periféricos de entrada (USB)")
             , ("Internet", myOtherTerminal ++ "-T nspHtop -e nmtui", "Control de conexiones de Internet")
             , ("Audio", myOtherTerminal ++ "-T nspHtop -e alsamixer", "Control de volúmenes de audio")
             ]
            
myActions :: [(String, String, String)]
myActions = [ ("Bloquear", "gnome-screensaver-command -al", "Bloquear la sesión actual")
            , ("Cerrar Sesión", "killall xmonad-x86_64-linux", "Terminar la sesión actual")
            , ("Suspender", "systemctl suspend", "Suspender la sesión actual")
            , ("Hibernar", "systemctl hibernate", "Hibernar la sesión actual")
            , ("Reiniciar", "reboot", "Reiniciar la computadora")
            , ("Apagar", "poweroff", "Apagar la computadora")
            ]
            
treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
   [ Node (TS.TSNode "Reproductores" "Lista de reproductores multimedia" (return ()))
     [Node (TS.TSNode (TE.fst3 $ myPlayers !! n)
                      (TE.thd3 $ myPlayers !! n)
                      (spawn $ TE.snd3 $ myPlayers !! n)
           ) [] | n <- [0..(length myPlayers - 1)]
     ]
   , Node (TS.TSNode "Navegadores Web" "Lista de navegadores Web" (return ()))
     [Node (TS.TSNode(TE.fst3 $ myBrowsers !! n)
                     (TE.thd3 $ myBrowsers !! n)
                     (spawn $ TE.snd3 $ myBrowsers !! n)
           ) [] | n <- [0..(length myBrowsers - 1)]
     ]
   , Node (TS.TSNode "Controles" "Lista de controles" (return ()))
     [Node (TS.TSNode(TE.fst3 $ myControls !! n)
                     (TE.thd3 $ myControls !! n)
                     (spawn $ TE.snd3 $ myControls !! n)
           ) [] | n <- [0..(length myControls - 1)]
     ]
   , Node (TS.TSNode "Configuraciones" "Configuración de archivos" (return ()))
     [Node (TS.TSNode (TE.fst3 $ myConfigs !! n)
                      (TE.thd3 $ myConfigs !! n)
                      (spawn $ TE.snd3 $ myConfigs !! n)
           ) [] | n <- [0..(length myConfigs - 1)]
     ]
   , Node (TS.TSNode "Más" "Opciones posibles con la sesión actual" (return ()))
     [Node (TS.TSNode (TE.fst3 $ myActions !! n)
                      (TE.thd3 $ myActions !! n)
                      (spawn $ TE.snd3 $ myActions !! n)
           ) [] | n <- [0..(length myActions - 1)]
     ]
   ]

-- Color configuration is ARGB
tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xee2c001e
                              , TS.ts_font         = myFont
                              , TS.ts_node         = (0xffaea79f, 0xff5e2750)
                              , TS.ts_nodealt      = (0xffaea79f, 0xff77216f)
                              , TS.ts_highlight    = (0xffe95420, 0xffaea79f)
                              , TS.ts_extra        = 0xffaea79f
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 25
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    ]

------------------------------------------------------------------------
-- Named ScratchPads. Add, modify or remove scratchpads here.
--
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "htop" spawnHtop findHtop defaultFloating
                , NS "nmtui" spawnNM findNM defaultFloating
                , NS "alsamixer" spawnAlsa findAlsa  defaultFloating
                ]
  where
    spawnHtop  = myOtherTerminal ++ "-T nspHtop -e htop"
    spawnNM    = myOtherTerminal ++ "-T nspNmtui -e nmtui"
    spawnAlsa  = myOtherTerminal ++ "-T nspAlsa -e alsamixer"
    findHtop   = title =? "nspHtop"
    findNM     = title =? "nspNmtui"
    findAlsa   = title =? "nspAlsa"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((mod1Mask,           xK_Return), spawn $ XMonad.terminal conf)
    
    -- launch rofi in filebrowser mode
    , ((modm .|. shiftMask, xK_Return), spawn "rofi -show filebrowser")

    -- launch rofi in run mode
    , ((modm,               xK_p     ), spawn "rofi -show run")

    -- launch rofi in drun mode
    , ((modm .|. shiftMask, xK_p     ), spawn "rofi -show drun -run-shell-command '{terminal} -e bash -ic \"{cmd} && read\"'")
    
    -- launch treeselect
    , ((mod1Mask .|. shiftMask, xK_Return ), treeselectAction tsDefaultConfig)
    
    -- launch htop scratchpad
    , ((mod1Mask, xK_h ), namedScratchpadAction myScratchPads "htop")
    
    -- launch nmtui scratchpad
    , ((mod1Mask, xK_n ), namedScratchpadAction myScratchPads "nmtui")
    
    -- launch alsamixer scratchpad
    , ((mod1Mask, xK_a ), namedScratchpadAction myScratchPads "alsamixer")
   
    -- take screenshot
    , ((0,           xK_Print),  spawn "scrot -q100 -m -e 'eog $n'")
    
    -- mute volume
    , ((0, xF86XK_AudioMute), spawn "amixer -q -D pulse sset Master toggle")
    
    -- decrease volume
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q -D pulse sset Master 10%-")
    
    -- increase volume
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q -D pulse sset Master 10%+")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Shift the focused window until being the master window
    , ((modm,               xK_Tab   ), windows W.shiftMaster)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), spawn "byebye-exe")

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]



------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts(tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

-- Firefox se fuerza a trasladarse al espacio de trabajo 3
-- Parece ser un bug
myManageHook = composeAll
    [ resource =? "rofi"               --> doCenterFloat
    , resource =? "eog"                --> doFloat
    , title =? "nspHtop"               --> doCenterFloat
    , title =? "nspNmtui"              --> doCenterFloat
    , title =? "nspAlsa"               --> doCenterFloat
    , isFullscreen                     --> doFullFloat
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = fullscreenEventHook

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
         spawnOnce "compton &"
         spawnOnce "gnome-screensaver-command --exit"
         spawnOnce "gnome-screensaver &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main =
        setRandomWallpaper ["/usr/share/backgrounds"] >>
        spawn "setxkbmap -layout latam" >>
        spawnPipe "xmobar -x 0 /home/rodrigo/.config/xmobar/xmobarrc" >>= \xmproc ->
	xmonad $ docks $ ewmh $ desktopConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = manageDocks <+> myManageHook,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP {
                                 ppCurrent = xmobarColor "#e95420" "" . wrap "[" "]"
                               , ppVisible = xmobarColor "#e95420" ""
                               , ppHidden = xmobarColor "#aea79f" ""
                               , ppHiddenNoWindows = xmobarColor "#e95420" ""
                               , ppTitle = (\str -> "")
                               , ppOrder = \(ws:l:t:_) -> [ws]
                               , ppOutput = hPutStrLn xmproc
                             },
        startupHook        = myStartupHook
    }

