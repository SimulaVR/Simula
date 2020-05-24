-- For questions about Simula's configuration, check out our discord: https://discordapp.com/invite/a4PnP7n

let Configuration =
  { _defaultWindowResolution = { _1 = 900, _2 = 900 } -- When apps launch, they default to this (square) resolution.
  , _defaultWindowScale      = 1.0 : Double           -- In addition to resolution, apps can be scaled up or down by this factor
  , _keyBindings             = [ -- For keyboard shortcuts, use keynames from
                                 -- https://github.com/godotengine/godot/blob/35e700e931f565aa37040055126fa61f02424ae0/core/os/keyboard.h 
                                 -- 
                                 -- Simula has the following special functions (explained in the README):
                                { _keyCombination = ["KEY_MASK_META", "KEY_BACKSPACE"]  , _keyAction = "terminateWindow" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_ESCAPE"]     , _keyAction = "toggleGrabMode"}
                              , { _keyCombination = ["KEY_MASK_META", "KEY_SLASH"]      , _keyAction = "launchTerminal" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_APOSTROPHE"] , _keyAction = "moveCursor" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_ENTER"]      , _keyAction = "clickLeft" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_ALT"]        , _keyAction = "grabWindow" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_F"]          , _keyAction = "orientSpriteTowardsGaze"  }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_9"]          , _keyAction = "scaleWindowDown" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_0"]          , _keyAction = "scaleWindowUp" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MINUS"]      , _keyAction = "zoomOut" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_EQUAL"]      , _keyAction = "zoomIn" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_COMMA"]      , _keyAction = "pullWindow" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_PERIOD"]     , _keyAction = "pushWindow" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_W"]          , _keyAction = "launchHMDWebCam" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_R"]          , _keyAction = "reloadConfig" }

                                -- Anything else is parsed as a shell command:
                              , { _keyCombination = ["KEY_MASK_META", "KEY_K"]          , _keyAction = "firefox -new-window" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_G"]          , _keyAction = "google-chrome-stable --new-window google.com" }
                              ]
  }

in Configuration
