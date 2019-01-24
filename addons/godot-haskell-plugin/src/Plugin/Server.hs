{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.Server where

import           Plugin.Imports

import           Foreign
import           Foreign.C.Types
import           Control.Lens
import qualified Language.C.Inline as C
import           Debug.C as C
import           Debug.Marshal

C.initializeSimulaCtxAndIncludes

-- |The following type name convention is modeled after wlroots' tinywl compositor 
-- | Much like tinywl, we adopt the following naming convention:
-- |   (i) the master type is called `SimulaServer`
-- |  (ii) types that wrap `wlr_type` are named `SimulaType`
-- | See https://github.com/swaywm/wlroots/tree/master/tinywl for more information.

data SimulaCursorMode = SimulaCursorPassthrough | SimulaCursorMove |  SimulaCursorResize

-- |I'm starting with inline-C types for now for ease of translation; 
-- |we can change them to their higher level hsroots types as familiarity with
-- |the codebase is gained.
data SimulaServer = SimulaServer { _ssObj :: GodotObject
                                 , _ssBackend :: Ptr C'WlrBackend
                                 , _ssRenderer :: Ptr C'WlrRenderer
                             --  , _ssXdgShell :: Ptr C'XdgShell -- We aren't implementing xdg shell
                                 , _ssNewXdgSurface :: C'WlListener
                                 , _ssViews :: [Ptr C'WlrView]
                                 , _ssCursor :: Ptr C'WlrCursor
                                 , _ssCursorManager :: Ptr C'WlrXCursorManager
                                 , _ssCursorMotion :: C'WlListener
                                 , _ssCursorMotionAbsolute :: C'WlListener
                                 , _ssCursorButton :: C'WlListener
                                 , _ssCursorAxis :: C'WlListener
                                 , _ssSeat :: Ptr C'WlrSeat
                                 , _ssNewInput :: C'WlListener
                                 , _ssRequestCursor :: C'WlListener
                                 , _ssKeyboards :: [Ptr C'WlrKeyboard]
                                 , _ssCursorMode :: SimulaCursorMode
                                 , _ssGrabbedView :: Ptr SimulaView
                                 , _ssGrabX :: CDouble
                                 , _ssGrabY :: CDouble
                                 , _ssGrabWidth :: CInt
                                 , _ssResizeEdges :: CUInt
                                 , _ssOutputLayout :: Ptr C'WlrOutputLayout
                                 , _ssOutputs :: [Ptr C'WlrOutput]
                                 , _ssNewOutput :: Ptr C'WlListener
                                 }

-- |The C'WlList fields are almost certainly not needed, but I'm leaving
-- |them in for now.
data SimulaOuput = SimulaOutput { _soLink :: C'WlList
                                , _soServer :: SimulaServer
                                , _soWlrOutput :: Ptr C'WlrOutput
                                , _soFrame :: C'WlListener
                                }

data SimulaView = SimulaView { _svLink :: C'WlList
                             , _svServer :: SimulaServer
                        --   , _svXdgSurface :: Ptr C'WlrXdgSurface -- We aren't implementing xdg
                             , _svMap :: C'WlListener
                             , _svUnmap :: C'WlListener
                             , _svDestroy :: C'WlListener
                             , _svRequestMove :: C'WlListener
                             , _svRequestResize :: C'WlListener
                             , _svMapped :: CBool
                             , _svX :: CInt
                             , _svY :: CInt
                             }

data SimulaKeyboard = SimulaKeyboard { _skLink :: C'WlList
                                     , _skServer :: SimulaServer
                                     , _skDevice :: Ptr C'WlrInputDevice
                                     , _skModifiers :: C'WlListener
                                     , _skKey :: C'WlListener
                                     }