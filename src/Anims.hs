{-# LANGUAGE ImplicitParams #-}

module Anims where

import APrelude
import Linear (Additive ((^+^)), V3, (*^), (^*))
import Model
import Types (Animation, BodyPart, Number)
import Prelude hiding (head)

main :: Animation
main = idlePose ==> loop (kick ||| turnHips ||| turnArms)

-- idlePose
--   ==> ( kick
--           ||| turnHips
--           ||| turnArms
--           ==> ( original
--                   ==> idlePose
--                   ==> (rotate upper_Arm_L `around` ((?t *^ xAxis) ^+^ ((1 - ?t) *^ yAxis))) (to 90) `for` 1
--               )
--       )
--     ||| loop (agrandarCabeza |>>| 0.5)

cuadrado :: Animation
cuadrado = loop $ mover xAxis ==> mover zAxis ==> mover (negate xAxis) ==> mover (negate zAxis)

mover :: V3 Number -> Animation
mover v = translate hips (v ^* easeInOut ?t) `for` 1

agrandarCabeza :: Animation
agrandarCabeza = scale head (1 + ?t) `for` 1

turnArms :: Animation
turnArms =
  (rotate upper_Arm_L `around` yAxis) (to (-30))
    `andAlso` (rotate upper_Arm_R `around` yAxis) (to (-30))
      `for` 1
      ==> easeOutStrong
        ( (rotate upper_Arm_L `around` yAxis) (to 80)
            `andAlso` (rotate upper_Arm_R `around` yAxis) (to 80)
        )
        `for` 1

turnHips :: Animation
turnHips =
  easeInOut ((rotate spine `around` zAxis) (to 30)) `for` 1
    ==> easeOutBack ((rotate spine `around` zAxis) (to (-30))) `for` 1

idlePose :: Animation
idlePose =
  ( (rotate upper_Arm_R `around` xAxis) (-30)
      `andAlso` (rotate lower_Arm_R `around` xAxis) (-30)
      `andAlso` (rotate upper_Arm_L `around` xAxis) (-30)
      `andAlso` (rotate lower_Arm_L `around` xAxis) (-30)
  )
    `for` 0

kick :: Animation
kick =
  easeInOut
    ( (rotate upper_Leg_R `around` yAxis) (to (-30))
        `andAlso` (rotate lower_Leg_R `around` xAxis) (to 45)
    )
    `for` 1
    ==> easeOutStrong
      ( (rotate upper_Leg_R `around` yAxis) (to 90)
          `andAlso` (rotate lower_Leg_R `around` xAxis) (to (-45))
      )
      `for` 1
