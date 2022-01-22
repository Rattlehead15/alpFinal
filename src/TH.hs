{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Collada (readCollada)
import Control.Lens (Lens', unsafeSingular, (%~), _head, _tail)
import Control.Monad (forM, join)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.Tree (Tree (Node))
import Data.Tree.Lens (branches, root)
import Language.Haskell.TH
  ( Dec,
    Exp,
    ExpQ,
    Q,
    litE,
    mkName,
    normalB,
    runIO,
    sigD,
    strTyLit,
    stringL,
    valD,
    varP,
  )
import Types (Collada (Collada), Joint (Joint), Skeleton)

forModel :: FilePath -> Q [Dec]
forModel fileName = do
  f <- runIO $ readFile fileName
  let (Collada _ _ _ _ _ _ _ hierarchy _) = fromJust $ readCollada f
  let nmodel = mkName "model"
  tmodel <- sigD nmodel [t|Collada|]
  model <- [d|$(varP nmodel) = fromJust $ readCollada $(litE (stringL f))|]
  lenses <- modelLenses [|id|] hierarchy
  return $ tmodel : model ++ lenses

modelLenses :: Q Exp -> Skeleton -> Q [Dec]
modelLenses soFar (Node (Joint name _) children) = do
  let coso = mkName ((_head %~ toLower) $ if take 4 name /= "Rig_" then name else drop 4 name)
  tf <- sigD coso [t|Lens' Skeleton Joint|]
  d <- valD (varP coso) (normalB [|unsafeSingular $ $soFar . root|]) []
  ds <- forM (zip children (map correspondingLens [1 .. (length children)])) (\(j, l) -> modelLenses [|$soFar . branches . $l|] j)
  return ([tf, d] ++ join ds)

correspondingLens :: Int -> ExpQ
correspondingLens i = [|foldr (.) id (replicate (i - 1) _tail) . _head|]
