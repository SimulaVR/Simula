{-# LANGUAGE LambdaCase, DataKinds, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.Input where

import           Data.Coerce
import qualified Data.Map.Strict             as M
import           Data.Bits

import qualified Godot.Methods               as G
import qualified Godot.Gdnative.Internal.Api as Api

import           Foreign
import           Foreign.C

import           Debug.C
import           Debug.Marshal

import           System.IO.Unsafe

import           Plugin.Imports
import           Plugin.Types

import           Graphics.Wayland.Server
import           Graphics.Wayland.Internal.Server
import           Graphics.Wayland.Internal.SpliceServerTypes
import           Graphics.Wayland.WlRoots.Input
import           Graphics.Wayland.WlRoots.Input.Keyboard
import           Graphics.Wayland.WlRoots.Seat

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

initializeSimulaCtxAndIncludes

-- i don't want to touch godot-haskell for proprietary changes

-- get_raw_keycode :: Method "get_raw_keycode" cls sig => cls -> sig
-- get_raw_keycode = runMethod @"get_raw_keycode"

-- bindInputEventKey_get_raw_keycode
--   = unsafePerformIO $
--       withCString "InputEventKey" $
--         \ clsNamePtr ->
--           withCString "get_raw_keycode" $
--             \ methodNamePtr ->
--               Api.godot_method_bind_get_method clsNamePtr methodNamePtr

-- {-# NOINLINE bindInputEventKey_get_raw_keycode #-}

-- instance Method "get_raw_keycode" GodotInputEventKey (IO Int) where
--         runMethod cls
--           = withVariantArray []
--               (\ (arrPtr, len) ->
--                  Api.godot_method_bind_call bindInputEventKey_get_raw_keycode (coerce cls)
--                    arrPtr
--                    len
--                    >>= \ (err, res) -> throwIfErr err >> fromGodotVariant res)

-- -- | This function passes Godot key events to wlroots. It doesn't yet input lock
-- -- | keys. It also uses wlr_seat_keyboard_notify_* instead of
-- -- | wlr_seat_keyboard_send_*, which I'm not sure is correct.
-- processKeyEvent :: GodotSimulaServer -> GodotInputEventKey -> IO ()
-- processKeyEvent gss evk = do
--   whenM (withGodotMethodBind bindInputEventKey_get_raw_keycode $ \ptr -> return $ ptr == coerce nullPtr) $ error "You need a patched Godot version to run this"
--   x11Code <- get_raw_keycode evk

--   -- See: https://github.com/swaywm/wlroots/blob/master/backend/x11/input_device.c#L79
--   let code = x11Code - 8

--   altPressed <- fromEnum <$> G.get_alt evk
--   shiftPressed <- fromEnum <$> G.get_shift evk
--   ctrlPressed <- fromEnum <$> G.get_control evk
--   superPressed <- fromEnum <$> G.get_metakey evk
--   let mods = fromIntegral $ shiftPressed + superPressed * 2 + ctrlPressed * 4 + altPressed * 8

--   pressed <- G.is_pressed evk
--   now32 <- getNow32

--   -- TODO: Fix lock keys:
--       ---- mask/values => numlock: 1, capslock: 2
--       ---- also, small note: watch out for the indent on that case statement, it's not immediately obvious what parses and what doesn't
--       --let lockMask = case code of
--       --      69 -> 1
--       --      58 -> 2
--       --      _ -> 0
--       --let lockValue = _ -- need to store lock values!
--       --weston_keyboard_set_locks kbd lockMask lockValue

--   -- See: ftp://www.x.org/pub/X11R7.7/doc/kbproto/xkbproto.html#Locking_and_Latching_Modifiers_and_Groups
--   let keyboardModifiers = Modifiers { modDepressed = mods :: Word32  -- HACK: We use mods repeatedly; need to fix lock/latched, etc.
--                                     , modLatched = mods :: Word32
--                                     , modLocked = 0 :: Word32
--                                     , modGroup = mods :: Word32
--                                     }
--   -- We could use either wlr_seat_keyboard_send_modifiers or
--   -- wlr_seat_keyboard_notify_modifiers (which "respects keyboard grabs"); we
--   -- choose the latter
--   with keyboardModifiers (\ptrKeyboardModifiers -> 
--                             keyboardNotifyModifiers 
--                               (gss ^. gssSeat) 
--                               ptrKeyboardModifiers)

--   -- We could use wlr_seat_keyboard_send_key or wlr_seat_keyboard_notify_key; we
--   -- experiment with the latter first since the former isn't in hsroots
--   keyboardNotifyKey (gss ^. gssSeat) now32 (fromIntegral code) (toKeyState pressed)

--   where
--     getNow32 = do
--       time <- getTime Realtime
--       let msec = fromIntegral $ toNanoSecs time `div` 1000000
--       return msec
--     -- See [[file:~/hsroots/src/Graphics/Wayland/WlRoots/Input/Keyboard.hsc::data%20KeyState]] 
--     toKeyState pressed | pressed = KeyPressed
--                        | otherwise = KeyReleased

setInputHandled :: (GodotNode :< a) => a -> IO ()
setInputHandled self = do
  st <- G.get_tree (safeCast self :: GodotNode)
  G.set_input_as_handled st


pattern OVR_Button_Touchpad :: Int
pattern OVR_Button_Touchpad = 14

pattern OVR_Button_Trigger :: Int
pattern OVR_Button_Trigger = 15

pattern OVR_Button_AppMenu :: Int
pattern OVR_Button_AppMenu = 1

pattern OVR_Button_Grip :: Int
pattern OVR_Button_Grip = 2

-- | A mapping from Godot to Linux keycodes.
-- | See https://gist.github.com/georgewsinger/bd52db93ce6567877d4826161415fcc1
keyTranslation :: M.Map Int Int
keyTranslation = M.fromList
  [ ((1 `shiftL` 24) .|. 0x01, 1)       -- KEY_ESCAPE         ~ KEY_ESC
  , ((1 `shiftL` 24) .|. 0x02, 15)      -- KEY_TAB            ~ KEY_TAB
  , ((1 `shiftL` 24) .|. 0x04, 14)      -- KEY_BACKSPACE      ~ KEY_BACKSPACE
  , ((1 `shiftL` 24) .|. 0x05, 28)      -- KEY_ENTER          ~ KEY_ENTER
  , ((1 `shiftL` 24) .|. 0x06, 96)      -- KEY_KP_ENTER       ~ KEY_KPENTER
  , ((1 `shiftL` 24) .|. 0x07, 110)     -- KEY_INSERT         ~ KEY_INSERT
  , ((1 `shiftL` 24) .|. 0x08, 111)     -- KEY_DELETE         ~ KEY_DELETE
  , ((1 `shiftL` 24) .|. 0x09, 119)     -- KEY_PAUSE          ~ KEY_PAUSE
  , ((1 `shiftL` 24) .|. 0x0A, 210)     -- KEY_PRINT          ~ KEY_PRINT
  , ((1 `shiftL` 24) .|. 0x0B, 99)      -- KEY_SYSREQ         ~ KEY_SYSRQ
  , ((1 `shiftL` 24) .|. 0x0C, 0x163)   -- KEY_CLEAR          ~ KEY_CLEAR
  , ((1 `shiftL` 24) .|. 0x0D, 102)     -- KEY_HOME           ~ KEY_HOME
  , ((1 `shiftL` 24) .|. 0x0E, 107)     -- KEY_END            ~ KEY_END
  , ((1 `shiftL` 24) .|. 0x0F, 105)     -- KEY_LEFT           ~ KEY_LEFT
  , ((1 `shiftL` 24) .|. 0x10, 103)     -- KEY_UP             ~ KEY_UP
  , ((1 `shiftL` 24) .|. 0x11, 106)     -- KEY_RIGHT          ~ KEY_RIGHT
  , ((1 `shiftL` 24) .|. 0x12, 108)     -- KEY_DOWN           ~ KEY_DOWN
  , ((1 `shiftL` 24) .|. 0x13, 104)     -- KEY_PAGEUP         ~ KEY_PAGEUP
  , ((1 `shiftL` 24) .|. 0x14, 109)     -- KEY_PAGEDOWN       ~ KEY_PAGEDOWN
  , ((1 `shiftL` 24) .|. 0x15, 42)      -- KEY_SHIFT          ~ KEY_LEFTSHIFT
  , ((1 `shiftL` 24) .|. 0x16, 29)      -- KEY_CONTROL        ~ KEY_LEFTCTRL
  , ((1 `shiftL` 24) .|. 0x17, 125)     -- KEY_META           ~ KEY_LEFTMETA
  , ((1 `shiftL` 24) .|. 0x18, 56)      -- KEY_ALT            ~ KEY_LEFTALT
  , ((1 `shiftL` 24) .|. 0x19, 58)      -- KEY_CAPSLOCK       ~ KEY_CAPSLOCK
  , ((1 `shiftL` 24) .|. 0x1A, 69)      -- KEY_NUMLOCK        ~ KEY_NUMLOCK
  , ((1 `shiftL` 24) .|. 0x1B, 70)      -- KEY_SCROLLLOCK     ~ KEY_SCROLLLOCK
  , ((1 `shiftL` 24) .|. 0x1C, 59)      -- KEY_F1             ~ KEY_F1
  , ((1 `shiftL` 24) .|. 0x1D, 60)      -- KEY_F2             ~ KEY_F2
  , ((1 `shiftL` 24) .|. 0x1E, 61)      -- KEY_F3             ~ KEY_F3
  , ((1 `shiftL` 24) .|. 0x1F, 62)      -- KEY_F4             ~ KEY_F4
  , ((1 `shiftL` 24) .|. 0x20, 63)      -- KEY_F5             ~ KEY_F5
  , ((1 `shiftL` 24) .|. 0x21, 64)      -- KEY_F6             ~ KEY_F6
  , ((1 `shiftL` 24) .|. 0x22, 65)      -- KEY_F7             ~ KEY_F7
  , ((1 `shiftL` 24) .|. 0x23, 66)      -- KEY_F8             ~ KEY_F8
  , ((1 `shiftL` 24) .|. 0x24, 67)      -- KEY_F9             ~ KEY_F9
  , ((1 `shiftL` 24) .|. 0x25, 68)      -- KEY_F10            ~ KEY_F10
  , ((1 `shiftL` 24) .|. 0x26, 87)      -- KEY_F11            ~ KEY_F11
  , ((1 `shiftL` 24) .|. 0x27, 88)      -- KEY_F12            ~ KEY_F12
  , ((1 `shiftL` 24) .|. 0x28, 183)     -- KEY_F13            ~ KEY_F13
  , ((1 `shiftL` 24) .|. 0x29, 184)     -- KEY_F14            ~ KEY_F14
  , ((1 `shiftL` 24) .|. 0x2A, 185)     -- KEY_F15            ~ KEY_F15
  , ((1 `shiftL` 24) .|. 0x2B, 186)     -- KEY_F16            ~ KEY_F16
  , ((1 `shiftL` 24) .|. 0x81, 55)      -- KEY_KP_MULTIPLY    ~ KEY_KPASTERISK
  , ((1 `shiftL` 24) .|. 0x82, 98)      -- KEY_KP_DIVIDE      ~ KEY_KPSLASH
  , ((1 `shiftL` 24) .|. 0x83, 74)      -- KEY_KP_SUBTRACT    ~ KEY_KPMINUS
  , ((1 `shiftL` 24) .|. 0x84, 83)      -- KEY_KP_PERIOD      ~ KEY_KPDOT
  , ((1 `shiftL` 24) .|. 0x85, 78)      -- KEY_KP_ADD         ~ KEY_KPPLUS
  , ((1 `shiftL` 24) .|. 0x86, 82)      -- KEY_KP_0           ~ KEY_KP0
  , ((1 `shiftL` 24) .|. 0x87, 79)      -- KEY_KP_1           ~ KEY_KP1
  , ((1 `shiftL` 24) .|. 0x88, 80)      -- KEY_KP_2           ~ KEY_KP2
  , ((1 `shiftL` 24) .|. 0x89, 81)      -- KEY_KP_3           ~ KEY_KP3
  , ((1 `shiftL` 24) .|. 0x8A, 75)      -- KEY_KP_4           ~ KEY_KP4
  , ((1 `shiftL` 24) .|. 0x8B, 76)      -- KEY_KP_5           ~ KEY_KP5
  , ((1 `shiftL` 24) .|. 0x8C, 77)      -- KEY_KP_6           ~ KEY_KP6
  , ((1 `shiftL` 24) .|. 0x8D, 71)      -- KEY_KP_7           ~ KEY_KP7
  , ((1 `shiftL` 24) .|. 0x8E, 72)      -- KEY_KP_8           ~ KEY_KP8
  , ((1 `shiftL` 24) .|. 0x8F, 73)      -- KEY_KP_9           ~ KEY_KP9
  , ((1 `shiftL` 24) .|. 0x2E, 139)     -- KEY_MENU           ~ KEY_MENU
  , ((1 `shiftL` 24) .|. 0x31, 138)     -- KEY_HELP           ~ KEY_HELP
  , ((1 `shiftL` 24) .|. 0x32, 153)     -- KEY_DIRECTION_L    ~ KEY_DIRECTION
  , ((1 `shiftL` 24) .|. 0x33, 153)     -- KEY_DIRECTION_R    ~ KEY_DIRECTION
  , ((1 `shiftL` 24) .|. 0x40, 158)     -- KEY_BACK           ~ KEY_BACK
  , ((1 `shiftL` 24) .|. 0x41, 159)     -- KEY_FORWARD        ~ KEY_FORWARD
  , ((1 `shiftL` 24) .|. 0x42, 128)     -- KEY_STOP           ~ KEY_STOP
  , ((1 `shiftL` 24) .|. 0x43, 173)     -- KEY_REFRESH        ~ KEY_REFRESH
  , ((1 `shiftL` 24) .|. 0x44, 114)     -- KEY_VOLUMEDOWN     ~ KEY_VOLUMEDOWN
  , ((1 `shiftL` 24) .|. 0x45, 113)     -- KEY_VOLUMEMUTE     ~ KEY_MUTE
  , ((1 `shiftL` 24) .|. 0x46, 115)     -- KEY_VOLUMEUP       ~ KEY_VOLUMEUP
  , ((1 `shiftL` 24) .|. 0x47, 209)     -- KEY_BASSBOOST      ~ KEY_BASSBOOST
  , ((1 `shiftL` 24) .|. 0x51, 172)     -- KEY_HOMEPAGE       ~ KEY_HOMEPAGE
  , ((1 `shiftL` 24) .|. 0x52, 0x16c)   -- KEY_FAVORITES      ~ KEY_FAVORITES
  , ((1 `shiftL` 24) .|. 0x53, 217)     -- KEY_SEARCH         ~ KEY_SEARCH
  , ((1 `shiftL` 24) .|. 0x54, 142)     -- KEY_STANDBY        ~ KEY_SLEEP
  , ((1 `shiftL` 24) .|. 0x55, 134)     -- KEY_OPENURL        ~ KEY_OPEN
  , ((1 `shiftL` 24) .|. 0x56, 155)     -- KEY_LAUNCHMAIL     ~ KEY_MAIL
  , ((1 `shiftL` 24) .|. 0xFFFFFF, 240) -- KEY_UNKNOWN        ~ KEY_UNKNOWN
  , (0x0020, 57)                        -- KEY_SPACE          ~ KEY_SPACE
  , (0x0023, 0x20b)                     -- KEY_NUMBERSIGN     ~ KEY_NUMERIC_POUND
  , (0x0024, 0x1b2)                     -- KEY_DOLLAR         ~ KEY_DOLLAR
  , (0x0027, 40)                        -- KEY_APOSTROPHE     ~ KEY_APOSTROPHE
  , (0x002C, 51)                        -- KEY_COMMA          ~ KEY_COMMA
  , (0x002D, 12)                        -- KEY_MINUS          ~ KEY_MINUS
  , (0x002E, 52)                        -- KEY_PERIOD         ~ KEY_DOT
  , (0x0030, 11)                        -- KEY_0              ~ KEY_0
  , (0x0031, 2)                         -- KEY_1              ~ KEY_1
  , (0x0032, 3)                         -- KEY_2              ~ KEY_2
  , (0x0033, 4)                         -- KEY_3              ~ KEY_3
  , (0x0034, 5)                         -- KEY_4              ~ KEY_4
  , (0x0035, 6)                         -- KEY_5              ~ KEY_5
  , (0x0036, 7)                         -- KEY_6              ~ KEY_6
  , (0x0037, 8)                         -- KEY_7              ~ KEY_7
  , (0x0038, 9)                         -- KEY_8              ~ KEY_8
  , (0x0039, 10)                        -- KEY_9              ~ KEY_9
  , (0x003B, 39)                        -- KEY_SEMICOLON      ~ KEY_SEMICOLON
  , (0x003D, 13)                        -- KEY_EQUAL          ~ KEY_EQUAL
  , (0x003F, 214)                       -- KEY_QUESTION       ~ KEY_QUESTION
  , (0x0041, 30)                        -- KEY_A              ~ KEY_A
  , (0x0042, 48)                        -- KEY_B              ~ KEY_B
  , (0x0043, 46)                        -- KEY_C              ~ KEY_C
  , (0x0044, 32)                        -- KEY_D              ~ KEY_D
  , (0x0045, 18)                        -- KEY_E              ~ KEY_E
  , (0x0046, 33)                        -- KEY_F              ~ KEY_F
  , (0x0047, 34)                        -- KEY_G              ~ KEY_G
  , (0x0048, 35)                        -- KEY_H              ~ KEY_H
  , (0x0049, 23)                        -- KEY_I              ~ KEY_I
  , (0x004A, 36)                        -- KEY_J              ~ KEY_J
  , (0x004B, 37)                        -- KEY_K              ~ KEY_K
  , (0x004C, 38)                        -- KEY_L              ~ KEY_L
  , (0x004D, 50)                        -- KEY_M              ~ KEY_M
  , (0x004E, 49)                        -- KEY_N              ~ KEY_N
  , (0x004F, 24)                        -- KEY_O              ~ KEY_O
  , (0x0050, 25)                        -- KEY_P              ~ KEY_P
  , (0x0051, 16)                        -- KEY_Q              ~ KEY_Q
  , (0x0052, 19)                        -- KEY_R              ~ KEY_R
  , (0x0053, 31)                        -- KEY_S              ~ KEY_S
  , (0x0054, 20)                        -- KEY_T              ~ KEY_T
  , (0x0055, 22)                        -- KEY_U              ~ KEY_U
  , (0x0056, 47)                        -- KEY_V              ~ KEY_V
  , (0x0057, 17)                        -- KEY_W              ~ KEY_W
  , (0x0058, 45)                        -- KEY_X              ~ KEY_X
  , (0x0059, 21)                        -- KEY_Y              ~ KEY_Y
  , (0x005A, 44)                        -- KEY_Z              ~ KEY_Z
  , (0x005C, 43)                        -- KEY_BACKSLASH      ~ KEY_BACKSLASH
  , (0x0060, 41)                        -- KEY_QUOTELEFT      ~ KEY_GRAVE
  , (0x007B, 26)                        -- KEY_BRACELEFT      ~ KEY_LEFTBRACE
  , (0x007D, 27)                        -- KEY_BRACERIGHT     ~ KEY_RIGHTBRACE
  , (0x00A0, 0x19b)                     -- KEY_NOBREAKSPACE   ~ KEY_BREAK
  , (0x00A4, 0x1b3)                     -- KEY_CURRENCY       ~ KEY_EURO
  , (0x00A5, 124)                       -- KEY_YEN            ~ KEY_YEN
  , (0x00B1, 118)                       -- KEY_PLUSMINUS      ~ KEY_KPPLUSMINUS
  ]
