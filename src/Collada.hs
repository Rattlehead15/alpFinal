{-# LANGUAGE OverloadedStrings #-}

module Collada where

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Attoparsec.Text hiding (take)
import Data.Either
import Data.Function
import Data.Functor
import Data.List
import Data.List.Split hiding (sepBy)
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import Data.Text (pack)
import Data.Tree
import Data.Tree.Lens
import GHC.Arr
import Graphics.UI.GLUT
import Linear
import Text.XML.Light
import Types
import Prelude hiding (words)

instance IsString QName where
  fromString s = QName s (Just "http://www.collada.org/2005/11/COLLADASchema") Nothing

readFloatArray :: Element -> String -> Maybe ([Double], String)
readFloatArray mesh id = do
  floatArray <-
    mesh
      & filterChild (findAttr (QName "id" Nothing Nothing) >>> (== Just id))
      >>= findChild "float_array"
  count <- findAttr (QName "count" Nothing Nothing) floatArray
  arr <-
    floatArray
      & strContent
      & pack
      & parseOnly (double `sepBy` char ' ')
      & fromRight []
      & return
  return (arr, count)

readCollada :: String -> Maybe Collada
readCollada file = do
  collada <- parseXMLDoc file
  geo <-
    collada
      & findChild "library_geometries"
      >>= findChild "geometry"
  geoId <- findAttr (QName "id" Nothing Nothing) geo
  mesh <- findChild "mesh" geo
  (vertsList, count) <- readFloatArray mesh (geoId <> "-positions")
  verticesV4 <-
    vertsList
      & chunksOf 3
      & map (\[x, y, z] -> V4 x y z 1)
      & listArray (0, read count `div` 3 - 1)
      & return
  (normalsList, count) <- readFloatArray mesh (geoId <> "-normals")
  normalsV4 <-
    normalsList
      & chunksOf 3
      & map (\[x, y, z] -> V4 x y z 0)
      & listArray (0, read count `div` 3 - 1)
      & return
  (uvsList, count) <- readFloatArray mesh (geoId <> "-map-0")
  uvs <-
    uvsList
      & chunksOf 2
      & map (\[u, v] -> TexCoord2 u (- v))
      & listArray (0, read count `div` 2 - 1)
      & return
  triangles <- mesh & findChild "triangles"
  count <- findAttr (QName "count" Nothing Nothing) triangles
  trianglesList <-
    triangles
      & findChild "p"
      >>= ( strContent
              >>> pack
              >>> parseOnly (decimal `sepBy` char ' ')
              >>> fromRight []
              >>> return
          )
  let triangles =
        trianglesList
          & chunksOf 9
          & map
            ( chunksOf 3
                >>> map (\[v, n, t] -> VInfo v n t)
                >>> (\[v1, v2, v3] -> (v1, v2, v3))
            )
          & listArray (0, read count - 1)
  controller <- collada & findChild "library_controllers" >>= findChild "controller"
  controllerId <- findAttr (QName "id" Nothing Nothing) controller
  skin <- findChild "skin" controller
  bindShape <-
    skin & findChild "bind_shape_matrix"
      >>= ( strContent
              >>> pack
              >>> parseOnly (double `sepBy` char ' ')
              >>> fromRight []
              >>> chunksOf 4
              >>> map (\[a, b, c, d] -> V4 a b c d)
              >>> (\[a, b, c, d] -> V4 a b c d)
              >>> return
          )
  let vertices = (\(V4 x y z _) -> Vertex3 x y z) . (bindShape !*) <$> verticesV4
  let normals = (\(V4 x y z _) -> Normal3 x y z) . (bindShape !*) <$> normalsV4
  nameArray <-
    skin
      & filterChild (findAttr (QName "id" Nothing Nothing) >>> (== Just (controllerId ++ "-joints")))
      >>= findChild "Name_array"
  count <- findAttr (QName "count" Nothing Nothing) nameArray
  bindPoses <-
    skin
      & filterChild (findAttr (QName "id" Nothing Nothing) >>> (== Just (controllerId ++ "-bind_poses")))
      >>= findChild "float_array"
      >>= (strContent >>> pack >>> parseOnly (double `sepBy` char ' ') >>> fromRight [] >>> return)
  let invBinds =
        bindPoses
          & chunksOf 4
          & map (\[a, b, c, d] -> V4 a b c d)
          & chunksOf 4
          & map (\[a, b, c, d] -> V4 a b c d)
          & listArray (0, read count - 1)
  jointNames <-
    nameArray
      & strContent
      & words
      & listArray (0, read count - 1)
      & return
  (weightsList, count) <- readFloatArray skin (controllerId ++ "-weights")
  let weights = weightsList & listArray (0, read count - 1)
  vertexWeights <- findChild "vertex_weights" skin
  count <- findAttr (QName "count" Nothing Nothing) vertexWeights
  vwCounts <-
    vertexWeights
      & findChild "vcount"
      >>= ( strContent
              >>> pack
              >>> parseOnly (decimal `sepBy` char ' ')
              >>> fromRight []
              >>> return
          )
  v <-
    vertexWeights & findChild "v"
      >>= ( strContent
              >>> pack
              >>> parseOnly (decimal `sepBy` char ' ')
              >>> fromRight []
              >>> return
          )
  let vertexWeights = makeVWList vwCounts v & map (chunksOf 2 >>> map (\[a, b] -> (a, b))) & listArray (0, read count - 1)
  rig <- collada & findChild "library_visual_scenes" >>= findChild "visual_scene" >>= filterChild (findAttr (QName "id" Nothing Nothing) >>> (== Just "Rig"))
  let jointTree = makeJointTree rig
  return $ Collada vertices normals uvs triangles weights vertexWeights jointNames jointTree invBinds

makeJointTree :: Element -> Tree Joint
makeJointTree e =
  Node
    ( Joint
        { _name = fromMaybe "" $ findAttr (QName "id" Nothing Nothing) e,
          _transform =
            maybe "" strContent (findChild "matrix" e)
              & pack
              & parseOnly (double `sepBy` char ' ')
              & fromRight []
              & chunksOf 4
              & map (\[a, b, c, d] -> V4 a b c d)
              & (\[a, b, c, d] -> V4 a b c d)
        }
    )
    (map makeJointTree (findChildren "node" e))

makeVWList :: [Int] -> [Int] -> [[Int]]
makeVWList (count : counts) vws = take (count * 2) vws : makeVWList counts (drop (count * 2) vws)
makeVWList _ _ = []

renderCollada :: Collada -> Skeleton -> IO ()
renderCollada dae@(Collada _ _ _ tris _ _ names _ invBinds) pose = do
  let jointMatrices = calculateJointMatrices identity invBinds names pose
  renderPrimitive Triangles $
    forM_
      tris
      ( \(x1, x2, x3) -> do renderVInfo dae jointMatrices x1; renderVInfo dae jointMatrices x2; renderVInfo dae jointMatrices x3
      )

calculateJointMatrices :: M44 GLdouble -> Array Int (M44 GLdouble) -> Array Int String -> Tree Joint -> Map.Map String (M44 GLdouble)
calculateJointMatrices mat invBinds names (Node (Joint name trans) joints) =
  let nTrans = (mat !*! trans)
      withInvBind = nTrans !*! (invBinds ! fromJust (arrIndex name names))
   in Map.insert (drop 4 name) withInvBind (Map.unions (map (calculateJointMatrices nTrans invBinds names) joints))

arrIndex :: String -> Array Int String -> Maybe Int
arrIndex n a = fst <$> find ((== n) . ("Rig_" ++) . snd) (assocs a)

renderVInfo :: Collada -> Map.Map String (M44 GLdouble) -> VInfo -> IO ()
renderVInfo (Collada verts normals uvs _ weights vertexWeights names _ _) jointMatrices (VInfo vIndex nIndex uvIndex) = do
  Graphics.UI.GLUT.texCoord (uvs ! uvIndex)
  let weightedT =
        foldr1 (!+!) $
          map
            ( \(jIndex, wIndex) ->
                (weights ! wIndex)
                  *!! (jointMatrices Map.! (names ! jIndex))
            )
            (vertexWeights ! vIndex)
  let V4 x y z _ = weightedT !* (let Vertex3 a b c = verts ! vIndex in V4 a b c 1)
  let V4 nx ny nz _ = weightedT !* (let Normal3 a b c = normals ! nIndex in V4 a b c 0)
  Graphics.UI.GLUT.normal (Normal3 nx ny nz)
  Graphics.UI.GLUT.vertex (Vertex3 x y z)
