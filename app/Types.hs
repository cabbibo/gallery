{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Linear
import Control.Monad.State
import Control.Lens
import Data.Data
import Data.Maybe
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import Data.Fixed
import Debug.Trace
import System.Random
import Control.Monad.Random

import Halive.Utils


roomHeight :: GLfloat
roomHeight = 4

roomWidth :: GLfloat
roomWidth = 5

roomDepth :: GLfloat
roomDepth = 3

sculptureSize :: GLfloat
sculptureSize = 0.3

sculptureHeight :: GLfloat
sculptureHeight = 1.4

paintingHeight :: GLfloat
paintingHeight = 1.5

paintingSize :: GLfloat
paintingSize = 0.8

frameExtra :: GLfloat
frameExtra = 0.05


pedestalHeight :: GLfloat
pedestalHeight = sculptureHeight - (sculptureSize / 2)

startHeight :: GLfloat
startHeight = 1.5

-- Offset of frame behind painting
frameOffset :: V3 GLfloat
frameOffset = V3 0 paintingHeight (0.03)

-- Offset of pedestal beneath sculpture
pedestalOffset :: V3 GLfloat
pedestalOffset = V3 0 ((pedestalHeight/2) - 0.001) 0

-- Position of sculpture ( relative )
sculptureOffset :: V3 GLfloat
sculptureOffset = V3 0 sculptureHeight 0


-- Position of Painting ( relative )
paintingOffset :: V3 GLfloat
paintingOffset = V3 0 paintingHeight 0


-- Offset of frame behind painting
roomOffset :: V3 GLfloat
roomOffset = V3 0 (roomHeight/2) 0



{-

  Shapes:

  Keeping all the different shapes
  in a single lense, so we can pass them
  to the render function as a package,
  instead of one by one

-}

data Shapes u = Shapes
  { _shpPedestal        :: Shape u
  , _shpFrame           :: Shape u
  , _shpRoom            :: Shape u
  , _shpLight           :: Shape u
  , _shpSculptures      :: [Shape u]
  , _shpPaintings       :: [Shape u]
  }
makeLenses ''Shapes





{-

  Painting:
  A painting is a 2D Plane on one of the walls. 
  Should have a 'frame' , a 'title' , a 'description'
  ( all coming later )

-}

data Painting = Painting
  { _pntPose :: !(Pose GLfloat)
  --, _pntProgram :: !Program
  }
makeLenses ''Painting



{-

  Sculpture:
  A sculpture is a 3D Cube in the middle of the room. 
  Should have a 'pedestal' , a 'title' , a 'description'
  ( all coming later )

-}

data Sculpture = Sculpture
  { _scpPose :: !(Pose GLfloat)
  --, _scpProgram :: !Program
  }
makeLenses ''Sculpture




{-

  Room:
  The thing that is rendered around the entire scene.
  Seperated into own data structure, because eventually
  we will want to use the APIs provided by Vive to 
  dynamically scale room based on playable area

-}
data Room = Room
  { _romPose :: !(Pose GLfloat)
  }
makeLenses ''Room


{-

  World:

  This is where we keep the majority of our data. 
  If we pass the same world into our render function,
  We should get the same visual result *every* time!

-}


data World = World
  { _wldPaintings     :: !(Map Int Painting)
  , _wldSculptures    :: !(Map Int Sculpture)
  , _wldPlayer        :: !(Pose GLfloat)
  , _wldRoom          :: !Room
  , _wldTime          :: !Float
  , _wldLight         :: !(Pose GLfloat)
  }
makeLenses ''World



{-

  Uniforms:

  A Big list of uniforms we use across our programs
  

-}

data Uniforms = Uniforms
  { uModelViewProjection :: UniformLocation (M44 GLfloat)
  , uViewProjection      :: UniformLocation (M44 GLfloat)
  , uNormalMatrix        :: UniformLocation (M44 GLfloat)
  , uInverseModel        :: UniformLocation (M44 GLfloat)
  , uModel               :: UniformLocation (M44 GLfloat)
  , uEye                 :: UniformLocation (V3  GLfloat)
  , uHand1               :: UniformLocation (V3  GLfloat)
  , uHand2               :: UniformLocation (V3  GLfloat)
  , uLight               :: UniformLocation (V3  GLfloat)
  , uTime                :: UniformLocation GLfloat
  , uDimensions          :: UniformLocation (V3  GLfloat)
  } deriving (Data)


