{-# LANGUAGE TemplateHaskell #-}

module Model where

import TH

-- We generate the lenses for the model in a separate module so we don't have to recompile it every time
-- (because it would imply having to read the model file every time)

forModel "model/chabon.dae"
