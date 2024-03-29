{-# LANGUAGE TemplateHaskell #-}

module Engine.Events where

import Control.Lens
import Data.IORef
import Engine.Skeleton
import Graphics.UI.GLUT
import Linear

data KeysState = MoveKeys {_left :: KeyState, _up :: KeyState, _right :: KeyState, _down :: KeyState} deriving (Show)

makeLenses ''KeysState

keyToLens :: Functor f => SpecialKey -> Maybe ((KeyState -> f KeyState) -> KeysState -> f KeysState)
keyToLens KeyLeft = Just left
keyToLens KeyUp = Just up
keyToLens KeyRight = Just right
keyToLens KeyDown = Just down
keyToLens _ = Nothing

charToLens :: Functor f => Char -> Maybe ((KeyState -> f KeyState) -> KeysState -> f KeysState)
charToLens 'w' = Just up
charToLens 'a' = Just left
charToLens 's' = Just down
charToLens 'd' = Just right
charToLens _ = Nothing

onMoveKeys :: KeyState -> IORef KeysState -> SpecialCallback
onMoveKeys state keysRef k _ =
  case keyToLens k of
    Nothing -> return ()
    Just key -> keysRef $~! (key .~ state)

onWasd :: KeyState -> IORef KeysState -> Char -> IO ()
onWasd state keysRef 'w' = keysRef $~! (up .~ state)
onWasd state keysRef 'a' = keysRef $~! (left .~ state)
onWasd state keysRef 's' = keysRef $~! (down .~ state)
onWasd state keysRef 'd' = keysRef $~! (right .~ state)
onWasd state keysRef 'W' = keysRef $~! (up .~ state)
onWasd state keysRef 'A' = keysRef $~! (left .~ state)
onWasd state keysRef 'S' = keysRef $~! (down .~ state)
onWasd state keysRef 'D' = keysRef $~! (right .~ state)
onWasd _ _ _ = return ()

onMouse :: IORef Transform -> IORef (Maybe Position) -> MotionCallback
onMouse camRef lastRef pos@(Position x y) = do
  (_, Size sizex sizey) <- get viewport
  let speed = 10
      (centerx, centery) = (sizex `div` 2, sizey `div` 2)
  last <- get lastRef
  (Transform _ rot) <- get camRef
  camRef
    $~! ( rotation
            .~ updateRot
              rot
              ( case last of
                  Nothing -> (0, 0)
                  Just (Position lastx lasty) -> (- speed * fromIntegral (x - lastx) / fromIntegral sizex, - speed * fromIntegral (y - lasty) / fromIntegral sizey)
              )
        )
  lastRef $= Just pos

onUpdate :: IORef Transform -> IORef KeysState -> IdleCallback
onUpdate camRef keysRef = do
  trans <- get camRef
  keys <- get keysRef
  camRef $~! (Engine.Skeleton.position .~ updatePos trans keys)
  postRedisplay Nothing

updateRot :: Quaternion GLdouble -> (GLdouble, GLdouble) -> Quaternion GLdouble
updateRot rot@(Quaternion w (V3 x y z)) (sx, sy) =
  let afterPitch = axisAngle (Linear.rotate rot $ V3 1 0 0) sy * rot
      newY = Linear.rotate afterPitch (V3 0 0 1) ^. _y
   in axisAngle (V3 0 1 0) sx
        * (if abs newY >= 0.99 && sy * newY <= 0 then rot else afterPitch)

updatePos :: Transform -> KeysState -> V3 GLdouble
updatePos (Transform pos rot) (MoveKeys l u r d) = pos + strafe * (speed r - speed l) + advance * (speed d - speed u)
  where
    speed k = if k == Down then 0.1 else 0
    strafe = Linear.rotate rot (V3 1 0 0)
    advance = Linear.rotate rot (V3 0 0 1)
