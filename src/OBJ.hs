{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module OBJ where

import Control.Arrow
import Control.Monad
import Data.Either
import Data.Function
import Data.Text hiding (filter, head, length, map, tail, zip)
import Data.Text.Read
import GHC.Arr
import Graphics.UI.GLUT hiding (Triangle)
import Linear
import Skeleton (toVertex3)
import Prelude hiding (isPrefixOf, lines)

type VInfo = (Int, Int, Int)

data Poly = Tri VInfo VInfo VInfo | Quad VInfo VInfo VInfo VInfo | NGon [VInfo]

data OBJ = OBJ {_vertices :: Array Int (Vertex3 GLdouble), _normals :: Array Int (Normal3 GLdouble), _uvs :: Array Int (TexCoord2 GLdouble), _faces :: Array Int Poly}

orElse :: Either a (c, Text) -> c -> c
orElse r n = fst . fromRight (n, "") $ r

readOBJ :: FilePath -> IO OBJ
readOBJ path = do
  file <- readFile path
  let x = lines $ pack file
      verts =
        x
          & filter ("v " `isPrefixOf`)
          & map (splitOn " " >>> (\t -> Vertex3 (double (t !! 1) `orElse` 0) (double (t !! 2) `orElse` 0) (double (t !! 3) `orElse` 0)))
          & (\vs -> listArray (1, length vs) vs)
      uvs =
        x
          & filter ("vt " `isPrefixOf`)
          & map (splitOn " " >>> (\t -> TexCoord2 (double (t !! 1) `orElse` 0) (1 - (double (t !! 2) `orElse` 0))))
          & (\vs -> listArray (1, length vs) vs)
      normals =
        x
          & filter ("vn " `isPrefixOf`)
          & map (splitOn " " >>> (\t -> Normal3 (double (t !! 1) `orElse` 0) (double (t !! 2) `orElse` 0) (double (t !! 3) `orElse` 0)))
          & (\vs -> listArray (1, length vs) vs)
      faces =
        x
          & filter ("f " `isPrefixOf`)
          & map
            ( splitOn " "
                >>> ( \t ->
                        if
                            | length t == 4 -> Tri (toVInfo (t !! 1)) (toVInfo (t !! 2)) (toVInfo (t !! 3))
                            | length t == 5 -> Quad (toVInfo (t !! 1)) (toVInfo (t !! 2)) (toVInfo (t !! 3)) (toVInfo (t !! 4))
                            | otherwise -> (NGon (map toVInfo (tail t)))
                    )
            )
          & (\vs -> listArray (1, length vs) vs)
  return $ OBJ verts normals uvs faces

renderOBJ :: OBJ -> IO ()
renderOBJ obj@(OBJ _ _ _ faces) =
  renderPrimitive Triangles $
    forM_
      faces
      ( \case
          Tri x1 x2 x3 -> do renderVInfo obj x1; renderVInfo obj x2; renderVInfo obj x3
          Quad x0 x1 x2 x3 -> do renderVInfo obj x0; renderVInfo obj x1; renderVInfo obj x2; renderVInfo obj x0; renderVInfo obj x2; renderVInfo obj x3
          NGon xs -> (mapM_ (\(y, z) -> do renderVInfo obj (head xs); renderVInfo obj y; renderVInfo obj z) $ zip (tail xs) (tail (tail xs)))
      )

renderVInfo :: OBJ -> VInfo -> IO ()
renderVInfo (OBJ verts normals uvs _) (vIndex, uvIndex, nIndex) = do
  texCoord (uvs ! uvIndex)
  normal (normals ! nIndex)
  vertex (verts ! vIndex)

toVInfo :: Text -> VInfo
toVInfo = splitOn "/" >>> (\vi -> (decimal (head vi) `orElse` 0, decimal (vi !! 1) `orElse` 0, decimal (vi !! 2) `orElse` 0)) -- Vertex/UV/Normal
