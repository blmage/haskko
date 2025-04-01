-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Event.Raw
-- Copyright   :  (C) 2025 Benoît Leulliette
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  Benoît Leulliette <benoit.leulliette@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.Event.Raw
  ( -- *** Smart constructors
    onRaw
  , onRawWithOptions
  -- *** Tagged events
  , TaggedEvent
  , onTaggedEvent
  , onTaggedEventWithOptions
  -- *** Mouse
  , onClickRaw
  , onDoubleClickRaw
  , onMouseDownRaw
  , onMouseUpRaw
  , onMouseEnterRaw
  , onMouseLeaveRaw
  , onMouseOverRaw
  , onMouseOutRaw
  -- *** Keyboard
  , onKeyDownRaw
  , onKeyPressRaw
  , onKeyUpRaw
  -- * Focus
  , onBlurRaw
  , onFocusRaw
  -- *** Drag
  , DragEvent(..)
  , onDragRaw
  , onDragLeaveRaw
  , onDragEnterRaw
  , onDragEndRaw
  , onDragStartRaw
  , onDragOverRaw
  -- *** Drop
  , onDropRaw
  ) where

-----------------------------------------------------------------------------
import           Control.Monad (when)
import           Control.Monad.Extra (whenMaybeM)
import qualified Data.Map.Strict as M
import           Data.Proxy (Proxy(..))
import           JSDOM.Event (Event)
import           JSDOM.FocusEvent (FocusEvent)
import           JSDOM.KeyboardEvent (KeyboardEvent)
import           JSDOM.MouseEvent (MouseEvent)
import           Language.Javascript.JSaddle hiding (val)
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.FFI (set, asyncCallback1, consoleError, hasPrototypeTagged)
import           Miso.Types ( Attribute (Event), LogLevel(..) )
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | The base function for creating "raw" event handlers.
--
-- See @onRawWithOptions@ for more details.
onRawWithOptionsM
  :: Options
  -> MisoString
  -> (Event -> JSM (Maybe action))
  -> Attribute action
onRawWithOptionsM options eventName toAction =
  Event $ \sink n logLevel events ->
     case M.lookup eventName events of
      Nothing ->
        when (logLevel `elem` [ DebugAll, DebugEvents ]) $
          consoleError $ mconcat
            [ "Event \""
            , eventName
            , "\" is not being listened on. To use this event, "
            , "add to the 'events' Map in 'App'"
            ]
      Just _ -> do
        eventObj <- getProp "events" n
        eventHandlerObject@(Object eo) <- create
        jsOptions <- toJSVal options
        cb <- asyncCallback1 $ \e -> do
            Just e' <- fromJSVal e
            maybe (pure ()) sink =<< toAction e'
        set "runEvent" cb eventHandlerObject
        set "options" jsOptions eventHandlerObject
        set eventName eo (Object eventObj)
-----------------------------------------------------------------------------
-- | @onRawWithOptions opts eventName toAction@ is an attribute
-- that will set the event handler of the associated DOM node to a function that
-- possibly converts the resulting events to an action using @toAction@ and then 
-- feeds that action back to the @update@ function.
--
-- @opts@ can be used to disable further event propagation.
--
-- > let clickHandler = onRawWithOptions defaultOptions "click" $ \_ -> Just Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
onRawWithOptions
  :: Options
  -> MisoString
  -> (Event -> Maybe action)
  -> Attribute action
onRawWithOptions options eventName toAction = 
  onRawWithOptionsM options eventName (pure . toAction)
-----------------------------------------------------------------------------
-- | Convenience wrapper for @onRawWithOptions defaultOptions@.
--
-- > let clickHandler = onRaw "click" $ \_ -> Just Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
onRaw :: MisoString
  -> (Event -> Maybe action)
  -> Attribute action
onRaw = onRawWithOptions defaultOptions
-----------------------------------------------------------------------------
-- | Represents a JS @event@ type associated with a specific prototype.
class FromJSVal event => TaggedEvent event where
  -- | Returns the name of the prototype associated with an @event@ type.
  tagOf :: Proxy event -> MisoString
-----------------------------------------------------------------------------
instance TaggedEvent FocusEvent where
  tagOf _ = "FocusEvent"
-----------------------------------------------------------------------------
instance TaggedEvent KeyboardEvent where
  tagOf _ = "KeyboardEvent"
-----------------------------------------------------------------------------
instance TaggedEvent MouseEvent where
  tagOf _ = "MouseEvent"
-----------------------------------------------------------------------------
-- | A variant of 'onRawWithOptions' for tagged events.
onTaggedEventWithOptions 
  :: forall event action
   . TaggedEvent event
  => Options
  -> MisoString
  -> (event -> action)
  -> Attribute action
onTaggedEventWithOptions options eventName toAction =
  onRawWithOptionsM options eventName $ \e -> do
    e' <- toJSVal e
    whenMaybeM (e' `hasPrototypeTagged` tagOf (Proxy @event))
      $ toAction <$> fromJSValUnchecked e'
-----------------------------------------------------------------------------
-- | A variant of 'onRaw' for tagged events.
onTaggedEvent
  :: forall event action
   . TaggedEvent event
  => MisoString
  -> (event -> action)
  -> Attribute action
onTaggedEvent = onTaggedEventWithOptions defaultOptions
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClickRaw :: (MouseEvent -> action) -> Attribute action
onClickRaw = onTaggedEvent "click"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClickRaw :: (MouseEvent -> action) -> Attribute action
onDoubleClickRaw = onTaggedEvent "dblclick"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDownRaw :: (MouseEvent -> action) -> Attribute action
onMouseDownRaw = onTaggedEvent "mousedown"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUpRaw :: (MouseEvent -> action) -> Attribute action
onMouseUpRaw = onTaggedEvent "mouseup"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnterRaw :: (MouseEvent -> action) -> Attribute action
onMouseEnterRaw = onTaggedEvent "mouseenter"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeaveRaw :: (MouseEvent -> action) -> Attribute action
onMouseLeaveRaw = onTaggedEvent "mouseleave"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOverRaw :: (MouseEvent -> action) -> Attribute action
onMouseOverRaw = onTaggedEvent "mouseover"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOutRaw :: (MouseEvent -> action) -> Attribute action
onMouseOutRaw = onTaggedEvent "mouseout"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDownRaw :: (KeyboardEvent -> action) -> Attribute action
onKeyDownRaw = onTaggedEvent "keydown"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPressRaw :: (KeyboardEvent -> action) -> Attribute action
onKeyPressRaw = onTaggedEvent "keypress"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUpRaw :: (KeyboardEvent -> action) -> Attribute action
onKeyUpRaw = onTaggedEvent "keyup"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/blur
onBlurRaw :: (FocusEvent -> action) -> Attribute action
onBlurRaw = onTaggedEvent "blur"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocusRaw :: (FocusEvent -> action) -> Attribute action
onFocusRaw = onTaggedEvent "focus"
-----------------------------------------------------------------------------
-- | A wrapper around 'MouseEvent', to specifically catch drag events.
--
-- Use 'JSDOM.MouseEvent.getDataTransfer' on the underlying 'MouseEvent' to
-- access the drag operation's data.
newtype DragEvent = DragEvent {toMouseEvent :: MouseEvent}
  deriving (PFromJSVal, PToJSVal, ToJSVal)
-----------------------------------------------------------------------------
instance FromJSVal DragEvent where
  fromJSVal val = fmap DragEvent <$> fromJSVal val
-----------------------------------------------------------------------------
instance TaggedEvent DragEvent where
  tagOf _ = "DragEvent"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDragRaw :: (DragEvent -> action) -> Attribute action
onDragRaw = onTaggedEvent "drag"
-----------------------------------------------------------------------------
onDragLeaveRaw :: (DragEvent -> action) -> Attribute action
onDragLeaveRaw = onTaggedEvent "dragleave"
-----------------------------------------------------------------------------
onDragEnterRaw :: (DragEvent -> action) -> Attribute action
onDragEnterRaw = onTaggedEvent "dragenter"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEndRaw :: (DragEvent -> action) -> Attribute action
onDragEndRaw = onTaggedEvent "dragend"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStartRaw :: (DragEvent -> action) -> Attribute action
onDragStartRaw = onTaggedEvent "dragstart"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOverRaw :: (DragEvent -> action) -> Attribute action
onDragOverRaw = onTaggedEvent "dragover"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDropRaw :: AllowDrop -> (DragEvent -> action) -> Attribute action
onDropRaw (AllowDrop allowDrop) =
  onTaggedEventWithOptions defaultOptions { preventDefault = allowDrop } "drop"