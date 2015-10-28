{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Game.Pal
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


{-

TODO: Move to Types

-}
roomHeight :: GLfloat
roomHeight = 7

roomWidth :: GLfloat
roomWidth = 4

roomDepth :: GLfloat
roomDepth = 6

sculptureSize :: GLfloat
sculptureSize = 0.5

sculptureHeight :: GLfloat
sculptureHeight = 1.5

paintingHeight :: GLfloat
paintingHeight = 2

paintingSize :: GLfloat
paintingSize = 1

frameExtra :: GLfloat
frameExtra = 0.1


pedestalHeight :: GLfloat
pedestalHeight = sculptureHeight - (sculptureSize / 2)

startHeight :: GLfloat
startHeight = 2

-- Offset of frame behind painting
frameOffset :: V3 GLfloat
frameOffset = V3 0 paintingHeight (0.03)

-- Offset of pedestal beneath sculpture
pedestalOffset :: V3 GLfloat
pedestalOffset = V3 0 (pedestalHeight/2) 0

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
  , uCamera              :: UniformLocation (V3  GLfloat)
  , uHand1               :: UniformLocation (V3  GLfloat)
  , uHand2               :: UniformLocation (V3  GLfloat)
  , uTime                :: UniformLocation GLfloat
  } deriving (Data)





{-

  Main:

  Gets called at beginning of program. 
  Initializing everything here, and than calling our main game loop.

-}

enableDevices :: [GamePalDevices]
-- enableDevices = [UseOpenVR]
-- enableDevices = [UseOpenVR, UseHydra]
enableDevices = []

main :: IO ()
main = do

  gamePal@GamePal{..} <- reacquire 0 $ initGamePal "Gallery" NoGCPerFrame enableDevices

  {-

    Setting up resources that will be used across multiple 
    objects

  -}
  roomProg       <- createShaderProgram "app/shaders/world/room.vert" "app/shaders/world/room.frag"
  roomGeo        <- cubeGeometry (V3 roomWidth roomHeight roomDepth) (V3 1 1 1)
  roomShape      <- makeShape roomGeo roomProg

  frameProg      <- createShaderProgram "app/shaders/world/frame.vert" "app/shaders/world/frame.frag"
  frameGeo       <- planeGeometry (V2 ((1.618 * paintingSize)+frameExtra) (paintingSize+frameExtra)) (V3 0 0 (-1)) (V3 0 1 0) (V2 1 1)
  frameShape     <- makeShape frameGeo frameProg

  pedestalProg   <- createShaderProgram "app/shaders/world/pedestal.vert" "app/shaders/world/pedestal.frag"
  pedestalGeo    <- cubeGeometry ((V3 sculptureSize pedestalHeight sculptureSize)) (V3 1 1 1)
  pedestalShape  <- makeShape pedestalGeo pedestalProg

  sculptureGeo   <- cubeGeometry ((V3 sculptureSize sculptureSize sculptureSize)) (V3 1 1 1)
  paintingGeo    <- planeGeometry (V2 (1.618 * paintingSize) paintingSize ) (V3 0 0 (-1)) (V3 0 1 0) (V2 1 1)

  --pedestalShape  <- makeShape pedestalGeo pedestalProg--markerGeo markerProg

  s1  <- makeShape sculptureGeo pedestalProg
  s2  <- makeShape sculptureGeo frameProg
  s3  <- makeShape sculptureGeo roomProg

  p1  <- makeShape paintingGeo pedestalProg
  p2  <- makeShape paintingGeo frameProg
  p3  <- makeShape paintingGeo roomProg


  let shapes = Shapes{ _shpRoom        = roomShape
                     , _shpFrame       = frameShape
                     , _shpPedestal    = pedestalShape
                     , _shpSculptures  = [s1 , s2 , s3 , s1 , s2 , s3, s3, s3]
                     , _shpPaintings   = [p1 , p2 , p3 , p1 , p2 , p3]
                     }


  {-

    Setting up some gl information

  -}
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE





  {-

    Building our default world.

    Because of the "!" s in the type declaration
    we need to declare all parts of the world, 
    or it be break in right away

  -}

  let sculpturePrograms = [  frameProg , roomProg , pedestalProg ]




  let world = World 
        { _wldPaintings = Map.fromList $ flip map [0..5] $ 
                          \i -> let something = Painting
                                      { _pntPose  = getPaintingPose i
                                      }
                                in (i, something)
        , _wldSculptures = Map.fromList $ flip map [0..7] $ 
                          \i -> let something = Sculpture
                                      { _scpPose  = getSculpturePose i 
                                      }
                                in (i, something)
        , _wldPlayer  = newPose {_posPosition = V3 0 startHeight 2}
        , _wldRoom    = Room { _romPose = newPose {_posPosition = V3 0 0 0} }
        , _wldTime    = 0
        }







  {-

    Main Game Loop!

  -}
  void . flip runStateT world . whileWindow gpWindow $ do


    delta <- realToFrac <$> liftIO gpGetDelta
    wldTime += delta

    time <- use wldTime


    applyMouseLook gpWindow wldPlayer
    applyWASD gpWindow wldPlayer
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      applyGamepadJoystickMovement e wldPlayer



    -- controls for debugging 
    shiftDown <- (== KeyState'Pressed) <$> getKey gpWindow Key'LeftShift
    whenKeyPressed gpWindow Key'Z           $ liftIO $ putStrLn $ "oh" ++ show 5 ++ " yeah"

    viewMat <- viewMatrixFromPose <$> use wldPlayer


    -- Once we have set up all the neccesary information,
    -- Render away!
    renderWith gamePal viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render shapes )










{-

  Render function.

  This should be totally pure! only take in world state, and feed back
  delicious pixels on the screen

-}



render :: (MonadIO m, MonadState World m) 
       => Shapes Uniforms
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render shapes projection viewMat = do

  time <- use wldTime


  let roomShape     = shapes ^. shpRoom
  let pedestalShape = shapes ^. shpPedestal
  let frameShape    = shapes ^. shpFrame


  let sculptureShapes    = shapes ^. shpSculptures
  let paintingShapes     = shapes ^. shpPaintings



  {-

    Render the Room

  -}


  glCullFace GL_FRONT

  room <- use wldRoom

  useProgram (sProgram roomShape)

  withVAO (sVAO roomShape) $ do

    let model = transformationFromPose $ shiftBy roomOffset (room ^. romPose)  
    drawShape model projection viewMat roomShape






  {-

    Render the Frames

  -}

  glCullFace GL_BACK

  paintings <- use wldPaintings

  useProgram (sProgram frameShape)

  withVAO (sVAO frameShape) $ do

    forM_ ( zip [0..] ( Map.toList paintings ) ) $ \( i , (objID, obj) ) -> do

      let model = transformationFromPose $ shiftBy frameOffset (obj ^. pntPose)  
      drawShape model projection viewMat frameShape


  {-

    Render the Pedestals

  -}
  sculptures <- use wldSculptures

  useProgram (sProgram pedestalShape)

  withVAO (sVAO pedestalShape) $ do

    forM_ ( zip [0..] ( Map.toList sculptures ) ) $ \( i , (objID, obj) ) -> do

      let model = transformationFromPose $ shiftBy pedestalOffset (obj ^. scpPose)  
      drawShape model projection viewMat pedestalShape



  {-

    Render the Sculptures

  -}
  sculptures <- use wldSculptures

  forM_ ( zip [0..] ( Map.toList sculptures ) ) $ \( i , (objID, obj) ) -> do

    let shape = (sculptureShapes !! i)
    useProgram (sProgram shape)

    withVAO (sVAO shape) $ do

      let model = transformationFromPose $ shiftBy sculptureOffset (obj ^. scpPose)  
      drawShape model projection viewMat shape




  {-

    Render the Paintings

  -}
  paintings <- use wldPaintings

  forM_ ( zip [0..] ( Map.toList paintings ) ) $ \( i , (objID, obj) ) -> do

    let shape = (paintingShapes !! i)
    useProgram (sProgram shape)

    withVAO (sVAO shape) $ do

      let model = transformationFromPose $ shiftBy paintingOffset (obj ^. pntPose)  
      drawShape model projection viewMat shape









{-

  Helper functions for drawing


-}

drawShape :: MonadIO m  => M44 GLfloat -> M44 GLfloat -> M44 GLfloat ->  Shape Uniforms -> m ()
drawShape model projection view shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uViewProjection      (projection !*! view)
  uniformM44 uModelViewProjection (projection !*! view !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model
  uniformM44 uNormalMatrix        (transpose . safeInv44 $ view !*! model )

  let vc = vertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr




--getPaintingPose :: Int -> Pose
getPaintingPose i = pose

  where fI = fromIntegral i 

        id = mod' (fromIntegral i) 3
        side = ((fromIntegral i ) - id ) / 3 
        angle = ((-0.25) + (0.5 * side)) * 3.14159 * 2

        pose = Pose{ _posPosition    = V3 ((side - 0.5) * 3.9) 0 ((id-1) * 2)
                   , _posOrientation = axisAngle (V3 0 1 0) angle
                   }
--getSculpturePose :: Int -> Pose
getSculpturePose i = pose

  where fI = fromIntegral i 

        z = mod' (fromIntegral i) 4  
        x = (fromIntegral i ) - z 
        pose = Pose{ _posPosition    = V3 (((x / 4) - 0.5 ) * 1.2) 0 ((z -1.5) * 1.2)
                   , _posOrientation = axisAngle (V3 0 1 0) 0
                   }







 