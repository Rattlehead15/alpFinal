{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module EX where

import APrelude
import TH
import Types

forModel "model/chabon.dae"

kick :: Animation
kick =
  loop $
    (\t -> (rotate upper_Leg_R `around` yAxis) (-30 * easeInOut t)) `for` 1
      ||| (\t -> (rotate lower_Leg_R `around` xAxis) (45 * easeInOut t)) `for` 1
      ==> (\t -> (rotate upper_Leg_R `around` yAxis) (90 * easeInOut t)) `for` 1
        ||| (\t -> (rotate lower_Leg_R `around` xAxis) (-45 * easeInOut t)) `for` 1
