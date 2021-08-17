-- For requests/questions about Simula's configuration, check out our discord: https://discordapp.com/invite/a4PnP7n

let Configuration =
    -- Simula can handle up to 5 default starting apps (parsed as shell commands).
    -- You can also use the following special commands:
    --   "launchTerminal" to launch the default terminal
    --   "launchHMDWebcam" to launch the HMD webcam view
    --   "launchUsageInstructions" to launch Simula's usage instructions
    -- To omit launching an app in a spot, use `None Text` instead of `Some "cmd"`.
  {   _backend = "OpenVR" -- Supported options: "OpenVR", "OpenXR"
  ,   _startingApps = { _center = Some "./result/bin/xfce4-terminal"
                      , _right  = Some "launchUsageInstructions"
                      , _bottom = Some "launchHMDWebcam"
                      , _left   = Some "launchTerminal"
                      , _top    = Some "launchTerminal"
                      }
  , _defaultWindowResolution = Some { _1 = 900, _2 = 900 } -- New windows default this (typically square) resolution
                                                           -- Set to `None { _1 : Natural, _2 : Natural }` for windows to launch with their default (typically non-square) resolutions

  , _defaultWindowScale      = 1.0 : Double                -- In addition to resolution, apps can be scaled up or down by this factor upon launch
  , _axisScrollSpeed         = 0.02 : Double
  , _mouseSensitivityScaler  = 1.00 : Double
  , _keyBindings             = [ -- For keyboard shortcuts, use keynames from
                                 -- https://github.com/SimulaVR/godot/blob/3.2-simula/core/os/keyboard.h
                                 -- For modifiers, use `KEY_MASK_*`. For everything else, use `KEY_*`.
                                 -- 
                                 -- Simula has the following special functions (explained in the README): https://github.com/SimulaVR/Simula#mouse--keyboard-controls
                                { _keyCombination = ["KEY_MASK_META", "KEY_BACKSPACE"]                 , _keyAction = "terminateWindow"            }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_Z"]                    , _keyAction = "toggleGrabMode"             }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_SLASH"]                     , _keyAction = "launchTerminal"             }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_APOSTROPHE"]                , _keyAction = "moveCursor"                 }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_ENTER"]                     , _keyAction = "clickLeft"                  }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_ALT_R"]                     , _keyAction = "grabWindow"                 }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_ALT_L"]                     , _keyAction = "grabWindow"                 }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_M"]                         , _keyAction = "grabWindows"                }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_M"]       , _keyAction = "grabWorkspaces"             }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_A"]                         , _keyAction = "launchAppLauncher"          }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_E"]                         , _keyAction = "cycleEnvironment"           }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_F"]                         , _keyAction = "orientWindowTowardsGaze"    }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_9"]                         , _keyAction = "scaleWindowDown"            }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_0"]                         , _keyAction = "scaleWindowUp"              }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MINUS"]                     , _keyAction = "zoomOut"                    }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_EQUAL"]                     , _keyAction = "zoomIn"                     }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_LEFT"]                      , _keyAction = "contractWindowHorizontally" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_RIGHT"]                     , _keyAction = "extendWindowHorizontally"   }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_UP"]                        , _keyAction = "contractWindowVertically"   }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_DOWN"]                      , _keyAction = "extendWindowVertically"     }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_S"]                         , _keyAction = "resizeWindowToDefaultSize"  } -- Resizes window to `_defaultWindowResolution`
                              , { _keyCombination = ["KEY_MASK_META", "KEY_COMMA"]                     , _keyAction = "pullWindow"                 }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_PERIOD"]                    , _keyAction = "pushWindow"                 }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_W"]                         , _keyAction = "launchHMDWebCam"            }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_R"]                         , _keyAction = "reloadConfig"               }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_ESCAPE"]  , _keyAction = "terminateSimula"            }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_ALT", "KEY_UP"]        , _keyAction = "increaseTransparency"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_ALT", "KEY_DOWN"]      , _keyAction = "decreaseTransparency"       }
                              , { _keyCombination = ["KEY_PRINT"]                                      , _keyAction = "toggleScreenshotMode"       }
                              , { _keyCombination = ["KEY_MASK_SHIFT", "KEY_PRINT"]                    , _keyAction = "takeScreenshotGlobal"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_1"]                         , _keyAction = "workspace1"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_2"]                         , _keyAction = "workspace2"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_3"]                         , _keyAction = "workspace3"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_4"]                         , _keyAction = "workspace4"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_5"]                         , _keyAction = "workspace5"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_6"]                         , _keyAction = "workspace6"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_7"]                         , _keyAction = "workspace7"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_8"]                         , _keyAction = "workspace8"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_1"]       , _keyAction = "sendToWorkspace1" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_2"]       , _keyAction = "sendToWorkspace2" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_3"]       , _keyAction = "sendToWorkspace3" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_4"]       , _keyAction = "sendToWorkspace4" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_5"]       , _keyAction = "sendToWorkspace5" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_6"]       , _keyAction = "sendToWorkspace6" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_7"]       , _keyAction = "sendToWorkspace7" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_8"]       , _keyAction = "sendToWorkspace8" }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_MASK_SHIFT", "KEY_0"]       , _keyAction = "sendToWorkspacePersistent" }


                                -- Anything else is parsed as a shell command:
                              , { _keyCombination = ["KEY_MASK_META", "KEY_K"] , _keyAction = "firefox -new-window"                                }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_G"] , _keyAction = "google-chrome-stable --new-window google.com"       }
                              , { _keyCombination = ["KEY_MASK_META", "KEY_J"] , _keyAction = "gvim"                                               }
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
  -- Configuration of Simula's HUD (in i3status format).
  -- See https://i3wm.org/docs/i3status.html
  , _hudConfig = "./config/i3status.config"

  }

in Configuration
