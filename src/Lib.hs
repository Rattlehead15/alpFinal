{-# LANGUAGE TemplateHaskell #-}

module Lib (run) where

import Control.Lens
import Data.Bifunctor
import Data.Tree
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Interact

data Transform = Transform {_position :: Point, _rotation :: Float}

data Bone = Bone {_model :: Picture, _transform :: Transform}

type Skeleton = Tree Bone

makeLenses ''Transform
makeLenses ''Bone

canonical :: Transform
canonical = Transform {_position = (0, 0), _rotation = 0}

carita :: Picture
carita =
  pictures
    [ color red $ circleSolid 50,
      translate 20 10 . color white $ circleSolid 10,
      translate (-20) 10 . color white $ circleSolid 10,
      translate 19 10 . color black $ circleSolid 5,
      translate (-19) 10 . color black $ circleSolid 5,
      color white $ polygon [(-20, -20), (20, -20), (0, -30)]
    ]

lilguy :: Skeleton
lilguy =
  ( Node
      { rootLabel =
          ( Bone
              { _model = carita,
                _transform =
                  ( Transform
                      { _position = (0, 0),
                        _rotation = 0
                      }
                  )
              }
          ),
        subForest =
          [ ( Node
                { rootLabel =
                    ( Bone
                        { _model = translate 50 0 . color blue $ rectangleSolid 100 20,
                          _transform =
                            ( Transform
                                { _position = (-50, 0),
                                  _rotation = 180
                                }
                            )
                        }
                    ),
                  subForest =
                    [ ( Node
                          { rootLabel =
                              ( Bone
                                  { _model = translate 50 0 . color green $ rectangleSolid 100 20,
                                    _transform = (Transform {_position = (100, 0), _rotation = 90})
                                  }
                              ),
                            subForest = []
                          }
                      )
                    ]
                }
            ),
            ( Node
                { rootLabel =
                    ( Bone
                        { _model = translate 50 0 . color blue $ rectangleSolid 100 20,
                          _transform =
                            ( Transform
                                { _position = (50, 0),
                                  _rotation = -30
                                }
                            )
                        }
                    ),
                  subForest = []
                }
            )
          ]
      }
  )

windowSize :: (Int, Int)
windowSize = (800, 600)

windowPos :: (Int, Int)
windowPos = (0, 0)

fps :: Int
fps = 60

run :: IO ()
run =
  play
    (InWindow "Coso" windowSize windowPos)
    (makeColor 0 0 0 1)
    fps
    lilguy
    (render canonical)
    react
    update

render :: Transform -> Skeleton -> Picture
render t (Node bone@(Bone _ trans) trs) = pictures $ renderBone t bone : map (applyTransform t . render trans) trs

renderBone :: Transform -> Bone -> Picture
renderBone tParent (Bone pic t) = pic & applyTransform t & applyTransform tParent

applyTransform :: Transform -> Picture -> Picture
applyTransform (Transform (x, y) theta) = translate x y . rotate theta

react :: Event -> Skeleton -> Skeleton
react _ = id

update :: Float -> Skeleton -> Skeleton
update dt = id
