-- For requests/questions about Simula's configuration, check out our discord: https://discordapp.com/invite/a4PnP7n

let Configuration =
       -- Simula can handle up to 5 default starting apps (parsed as shell commands).
       -- You can also use the following special commands:
       --   "launchTerminal" to launch the default terminal
       --   "launchHMDWebcam" to launch the HMD webcam view
       --   "launchUsageInstructions" to launch Simula's usage instructions
       -- To omit launching an app in a spot, use `None Text` instead of `Some "cmd"`.
     { _startingApps = { _center = Some "./result/bin/xfce4-terminal"
                       , _right  = Some "launchUsageInstructions"
                       , _bottom = Some "launchHMDWebcam"
                       , _left   = Some "launchTerminal"
                       , _top    = Some "launchTerminal"
                       }
  , _defaultWindowResolution = { _1 = 900, _2 = 900 } -- When apps launch, they default to this (square) resolution.
  , _defaultWindowScale      = 1.0 : Double           -- In addition to resolution, apps can be scaled up or down by this factor
  , _keyBindings             = [ -- For keyboard shortcuts, use keynames from
                                 -- https://github.com/SimulaVR/godot/blob/3.2-simula/core/os/keyboard.h
                                 -- For modifiers, use `KEY_MASK_*`. For everything else, use `KEY_*`.
                                 -- 
                                 -- Simula has the following special functions (explained in the README): https://github.com/SimulaVR/Simula#mouse--keyboard-controls
                                { _keyCombination = ["KEY_MASK_META", "KEY_BACKSPACE"]                 , _keyAction = "terminateWindow"         }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_ESCAPE"]                    , _keyAction = "toggleGrabMode"          }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_SLASH"]                     , _keyAction = "launchTerminal"          }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_APOSTROPHE"]                , _keyAction = "moveCursor"              }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_ENTER"]                     , _keyAction = "clickLeft"               }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_ALT_R"]                     , _keyAction = "grabWindow"              }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_ALT_L"]                     , _keyAction = "grabWindow"              }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_E"]                         , _keyAction = "cycleEnvironment"        }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_F"]                         , _keyAction = "orientSpriteTowardsGaze" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_9"]                         , _keyAction = "scaleWindowDown"         }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_0"]                         , _keyAction = "scaleWindowUp"           }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MINUS"]                     , _keyAction = "zoomOut"                 }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_EQUAL"]                     , _keyAction = "zoomIn"                  }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_COMMA"]                     , _keyAction = "pullWindow"              }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_PERIOD"]                    , _keyAction = "pushWindow"              }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_W"]                         , _keyAction = "launchHMDWebCam"         }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_R"]                         , _keyAction = "reloadConfig"            }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_ESCAPE"]  , _keyAction = "terminateSimula"         }

                                -- Anything else is parsed as a shell command:
                              , { _keyCombination = ["KEY_MASK_META", "KEY_K"] , _keyAction = "firefox -new-window"                          }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_G"] , _keyAction = "google-chrome-stable --new-window google.com" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_J"] , _keyAction = "gvim"                                         }
                              ]

    , _keyRemappings         = [ -- Simula allows you to remap keys (`KEY_*`). For example, remapping `KEY_HYPER_L` to `KEY_ESCAPE`:
                                 -- { _keyOriginal =  "KEY_HYPER_L" , _keyMappedTo = "KEY_ESCAPE" }
                                 --
                                 -- If you'd like to disable a key, map it to `KEY_NULL`, as in
                                 -- , { _keyOriginal =  "KEY_ESCAPE" , _keyMappedTo = "KEY_NULL" }
                               ] : List { _keyOriginal : Text, _keyMappedTo : Text }
  -- Environments must be local to Simula's root directory
  , _environmentsDirectory = "./environments"
  , _environmentDefault    = "./environments/AllSkyFree_Sky_EpicBlueSunset_Equirect.png"
  }

in Configuration
