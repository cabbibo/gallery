{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

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

import Types


{-

  Main:

  Gets called at beginning of program. 
  Initializing everything here, and than calling our main game loop.

-}

enableDevices :: [VRPalDevices]
--enableDevices = [UseOpenVR]
-- enableDevices = [UseOpenVR, UseHydra]
enableDevices = []

main :: IO ()
main = do

  gamePal@VRPal{..} <- reacquire 0 $ initVRPal "Gallery" NoGCPerFrame enableDevices

  {-

    Setting up resources that will be used across multiple 
    objects

  -}
  roomProg       <- createShaderProgram "app/shaders/world/room.vert" "app/shaders/world/room.frag"
  roomGeo        <- cubeGeometry (V3 roomWidth roomHeight roomDepth) (V3 30 30 30)
  roomShape      <- makeShape roomGeo roomProg

  frameProg      <- createShaderProgram "app/shaders/world/frame.vert" "app/shaders/world/frame.frag"
  frameGeo       <- planeGeometry (V2 ((1.618 * paintingSize)+frameExtra) (paintingSize+frameExtra)) (V3 0 0 (-1)) (V3 0 1 0) (V2 1 1)
  frameShape     <- makeShape frameGeo frameProg

  pedestalProg   <- createShaderProgram "app/shaders/world/pedestal.vert" "app/shaders/world/pedestal.frag"
  pedestalGeo    <- cubeGeometry ((V3 sculptureSize pedestalHeight sculptureSize)) (V3 20 30 20)
  pedestalShape  <- makeShape pedestalGeo pedestalProg


  lightProg   <- createShaderProgram "app/shaders/world/light.vert" "app/shaders/world/light.frag"
  lightGeo    <- icosahedronGeometry 0.6 4
  lightShape  <- makeShape lightGeo lightProg

  sculptureGeo   <- cubeGeometry ((V3 sculptureSize sculptureSize sculptureSize)) (V3 1 1 1)
  paintingGeo    <- planeGeometry (V2 (1.618 * paintingSize) paintingSize ) (V3 0 0 (-1)) (V3 0 1 0) (V2 1 1)

  --pedestalShape  <- makeShape pedestalGeo pedestalProg--markerGeo markerProg


  let vs = "app/shaders/template/raytrace.vert"
  tRaytraceProg  <- createShaderProgram vs "app/shaders/template/raytrace.frag"
  tFogProg       <- createShaderProgram vs "app/shaders/template/fogStep.frag"


  pSpectacles       <- createShaderProgram vs "app/shaders/paintings/spectacles.frag"
  pHexAndGoo        <- createShaderProgram vs "app/shaders/paintings/hexAndGoo.frag"
  pOwlex            <- createShaderProgram vs "app/shaders/paintings/owlex.frag"
  pReflectSpheres   <- createShaderProgram vs "app/shaders/paintings/reflectSpheres.frag"
  pRedRing          <- createShaderProgram vs "app/shaders/paintings/redRing.frag"
  pTree             <- createShaderProgram vs "app/shaders/paintings/tree.frag"
  pTunnel1          <- createShaderProgram vs "app/shaders/paintings/tunnel1.frag"
  pTunnel2          <- createShaderProgram vs "app/shaders/paintings/tunnel2.frag"
  pCubeAndSpheres   <- createShaderProgram vs "app/shaders/paintings/cubeAndSpheres.frag"

  sPit              <- createShaderProgram vs "app/shaders/sculptures/pit.frag"
  sNoiseStep        <- createShaderProgram vs "app/shaders/sculptures/noiseStep.frag"
  sWeirdHoles1      <- createShaderProgram vs "app/shaders/sculptures/weirdHoles1.frag"
  sFieldSub         <- createShaderProgram vs "app/shaders/sculptures/fieldSub.frag"
  sBubbles          <- createShaderProgram vs "app/shaders/sculptures/bubbles.frag"
  sCubeSubField     <- createShaderProgram vs "app/shaders/sculptures/cubeSubField.frag"
  sTessel           <- createShaderProgram vs "app/shaders/sculptures/tessel.frag"
  sTesselSphere     <- createShaderProgram vs "app/shaders/sculptures/tesselSphere.frag"


  s1  <- makeShape sculptureGeo sPit
  s2  <- makeShape sculptureGeo sNoiseStep
  s3  <- makeShape sculptureGeo sFieldSub
  s4  <- makeShape sculptureGeo sWeirdHoles1
  s5  <- makeShape sculptureGeo sBubbles
  s6  <- makeShape sculptureGeo sCubeSubField
  s7  <- makeShape sculptureGeo sTessel
  s8  <- makeShape sculptureGeo sTesselSphere


  p1  <- makeShape paintingGeo pOwlex
  p2  <- makeShape paintingGeo pSpectacles
  p3  <- makeShape paintingGeo pTree
  p4  <- makeShape paintingGeo pHexAndGoo
  p5  <- makeShape paintingGeo pTunnel2
  p6  <- makeShape paintingGeo pReflectSpheres


  let shapes = Shapes{ _shpRoom        = roomShape
                     , _shpFrame       = frameShape
                     , _shpLight       = lightShape
                     , _shpPedestal    = pedestalShape
                     , _shpSculptures  = [s1 , s2 , s3 , s4 , s5 , s6, s7, s8]
                     , _shpPaintings   = [p1 , p2 , p4 , p3 , p6 , p5]
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




  world <- reacquire 1 $ return World 
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
        , _wldPlayer  = Pose {_posOrientation = axisAngle (V3 0 1 0) 0 , _posPosition = V3 0 0 0}
        , _wldRoom    = Room { _romPose = newPose {_posPosition = V3 0 0 0} }
        , _wldTime    = 0
        , _wldLight   = newPose {_posPosition = V3 0 (roomHeight) 0}
        }







  {-

    Main Game Loop!

  -}
  void . flip runStateT world . whileWindow gpWindow $ do
    
    persistState 1

    delta <- realToFrac <$> liftIO gpGetDelta
    wldTime += delta

    time <- use wldTime


    --applyMouseLook gpWindow wldPlayer
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
      (render shapes)





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
  let lightShape    = shapes ^. shpLight


  let sculptureShapes    = shapes ^. shpSculptures
  let paintingShapes     = shapes ^. shpPaintings


  {-

    Render the Room

  -}

  glEnable GL_CULL_FACE
  glCullFace GL_FRONT

  room <- use wldRoom

  useProgram (sProgram roomShape)

  withVAO (sVAO roomShape) $ do


    let model = transformationFromPose $ shiftBy roomOffset (room ^. romPose)  
    drawShape' model projection viewMat roomShape


  {-

    Render the Light

  -}

  glCullFace GL_BACK

  light <- use wldLight

  useProgram (sProgram lightShape)

  withVAO (sVAO lightShape) $ do


    let model = transformationFromPose light 
    drawShape' model projection viewMat lightShape





  {-

    Render the Frames

  -}

  
  --glCullFace GL_BACK

  paintings <- use wldPaintings

  useProgram (sProgram frameShape)

  withVAO (sVAO frameShape) $ do

    forM_ ( zip [0..] ( Map.toList paintings ) ) $ \( i , (objID, obj) ) -> do


      let model = transformationFromPose $ shiftBy frameOffset (obj ^. pntPose)  
      drawShape' model projection viewMat frameShape


  {-

    Render the Pedestals

  -}
  sculptures <- use wldSculptures

  useProgram (sProgram pedestalShape)

  withVAO (sVAO pedestalShape) $ do
 
 

    forM_ ( zip [0..] ( Map.toList sculptures ) ) $ \( i , (objID, obj) ) -> do

      let model = transformationFromPose $ shiftBy pedestalOffset  (obj ^. scpPose)  
      drawShape' model projection viewMat pedestalShape







  {-

    Render the Paintings

  -}
  paintings <- use wldPaintings

  forM_ ( zip [0..] ( Map.toList paintings ) ) $ \( i , (objID, obj) ) -> do

    let shape = (paintingShapes !! i)
    useProgram (sProgram shape)

    let Uniforms{..} = sUniforms shape

    uniformV3 uDimensions (V3 (paintingSize * 1.618) paintingSize 0)

    withVAO (sVAO shape) $ do

      let model = transformationFromPose $ shiftBy paintingOffset (obj ^. pntPose)  
      drawShape' model projection viewMat shape






  {-

    Render the Sculptures

    Draw them last because they are going to have their backsides shown

  -}

  glDisable GL_CULL_FACE
  
  sculptures <- use wldSculptures

  forM_ ( zip [0..] ( Map.toList sculptures ) ) $ \( i , (objID, obj) ) -> do

    let shape = (sculptureShapes !! i)
    useProgram (sProgram shape)

    let Uniforms{..} = sUniforms shape

    uniformV3 uDimensions (V3 (sculptureSize) (sculptureSize) (sculptureSize))

    withVAO (sVAO shape) $ do

      let model = transformationFromPose $ shiftBy sculptureOffset (obj ^. scpPose)  
      drawShape' model projection viewMat shape





{-

  Helper functions for drawing


-}

drawShape' ::(MonadIO m, MonadState World m) => M44 GLfloat -> M44 GLfloat -> M44 GLfloat ->  Shape Uniforms -> m ()
drawShape' model projection view shape = do 

  let Uniforms{..} = sUniforms shape

  light <- use wldLight
  time  <- use wldTime


  -- Recalculating for each object. doesn't make sense!
  uniformV3 uEye (fromMaybe view (inv44 view) ^. translation)
  uniformV3 uLight ((light ^. posPosition)- (V3 0 0.3 0))
  uniformF  uTime time

  uniformM44 uViewProjection      (projection !*! view)
  uniformM44 uModelViewProjection (projection !*! view !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model
  uniformM44 uNormalMatrix        (transpose . safeInv44 $ view !*! model )

  let vc = geoVertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr




--getPaintingPose :: Int -> Pose
getPaintingPose i = pose

  where fI = fromIntegral i 

        id = mod' (fromIntegral i) 3
        side = ((fromIntegral i ) - id ) / 3 
        angle = (-0.5 + (0.5 * side)) * 3.14159 * 2

        pose = Pose{ _posPosition    = V3 ((id-1) * 2 * paintingSize) 0 ((side - 0.5) * roomDepth)
                   , _posOrientation = axisAngle (V3 0 1 0) angle
                   }
--getSculpturePose :: Int -> Pose
getSculpturePose i = pose

  where fI = fromIntegral i 

        x = mod' (fromIntegral i) 4  
        z = (fromIntegral i ) - x 
        pose = Pose{ _posPosition    = V3 (((x / 4) - 1.5/4 ) * roomWidth * 0.8) 0 ((( z/4) - 1.5/4 ) * roomDepth * 0.4)
                   , _posOrientation = axisAngle (V3 0 1 0) 0
                   }







 
