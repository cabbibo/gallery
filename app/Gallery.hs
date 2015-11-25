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
import Control.Monad.Reader
import Control.Lens.Extra
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

import Graphics.GL.Freetype
import TinyRick

import Halive.Utils

import Types
import Shapes


{-

  Main:

  Gets called at beginning of program. 
  Initializing everything here, and than calling our main game loop.

-}

enableDevices :: [VRPalDevices]
enableDevices = [UseOpenVR]
-- enableDevices = [UseOpenVR, UseHydra]
-- enableDevices = []



newWorld :: Font -> IO World
newWorld font = do
  
  sculptures  <- buildSculptures font
  paintings   <- buildPaintings font
  chunks      <- buildChunks font

  return World 

        { _wldSculptures      = sculptures
        , _wldPaintings       = paintings
        , _wldChunks          = chunks
        , _wldFocusedObjectID = 0

        , _wldPlayer  = Pose {_posOrientation = axisAngle (V3 0 1 0) 3.14 , _posPosition = V3 0 0 0}
        , _wldRoom    = Room { _romPose = newPose {_posPosition = V3 0 0 0} }
        , _wldTime    = 0
        , _wldLight   = newPose {_posPosition = V3 0 (roomHeight) 0}

        }

main :: IO ()
main = do

  vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Gallery" enableDevices


  {-
    Setting up our resources to render the text
  -}
  glyphProg <- createShaderProgram "app/shaders/world/glyph.vert" "app/shaders/world/glyph.frag"
  font      <- createFont "app/fonts/SourceCodePro-Regular.ttf" 50 glyphProg
  
  {-

    Setting up some gl information

  -}
  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE

  shapes <- galleryShapes

  {-

    Building our default world.

    Because of the "!" s in the type declaration
    we need to declare all parts of the world, 
    or it be break in right away

  -}

  world <- reacquire 1 (newWorld font)


  {-

    Main Game Loop!

  -}
  void . flip runStateT world . whileWindow gpWindow $ do
    
    persistState 1

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
    immutably $ renderWith vrPal viewMat
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
  let chunkShapes        = shapes ^. shpChunks


  {-

    Render the Room

  -}

  glEnable GL_CULL_FACE
  glCullFace GL_FRONT
  glClearColor 0 0 0.1 1
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

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


    -- And their code
  -- glDisable GL_DEPTH_TEST
  glDisable GL_CULL_FACE
  glEnable  GL_BLEND
  forM_ paintings $ \obj -> do
    let buffer = obj ^. pntBuffer
        font   = bufFont buffer
        pose   = id
               . shiftBy (V3 (1.618 * paintingSize / 2) (paintingHeight + paintingSize/2) 0.01)
               $ newPose { _posPosition = obj ^. pntPose . posPosition }
        model44 = transformationFromPose pose
        mvp = projection !*! viewMat !*! model44 !*! scaleMatrix 0.02
    renderText font (bufText buffer) (bufSelection buffer) mvp

  -- glEnable GL_DEPTH_TEST
  glEnable  GL_CULL_FACE
  glDisable GL_BLEND


  {-

    Render the Pedestals

  -}
  sculptures <- use wldSculptures

  useProgram (sProgram pedestalShape)

  withVAO (sVAO pedestalShape) $ do
 
 

    forM_ ( zip [0..] ( Map.toList sculptures ) ) $ \( i , (objID, obj) ) -> do

      let model = transformationFromPose $ shiftBy pedestalOffset  (obj ^. scpPose)  
      drawShape' model projection viewMat pedestalShape


  -- And their code
  -- glDisable GL_DEPTH_TEST
  glDisable GL_CULL_FACE
  glEnable  GL_BLEND
  forM_ sculptures $ \obj -> do
    let buffer = obj ^. scpBuffer
        font   = bufFont buffer
        pose   = id
               -- . rotateBy (axisAngle (V3 1 0 0) (-pi/4 - 0.4))
               . shiftBy (V3 (-sculptureSize/2) ( sculptureHeight+sculptureSize * 2) sculptureSize/1.5)
               $ newPose { _posPosition = obj ^. scpPose . posPosition }
        model44 = transformationFromPose pose
        mvp = projection !*! viewMat !*! model44 !*! scaleMatrix 0.02
    renderText font (bufText buffer) (bufSelection buffer) mvp

  -- glEnable GL_DEPTH_TEST
  glEnable  GL_CULL_FACE
  glDisable GL_BLEND





  {-

    Render the Chunks

  -}
  chunks <- use wldChunks

  forM_ ( zip chunkShapes ( Map.toList chunks ) ) $ \( shape , (objID, obj) ) -> do

    --let shape = (chunkShapes !! i)
    useProgram (sProgram shape)

    let Uniforms{..} = sUniforms shape

    uniformV3 uDimensions (V3 chunkSize chunkSize 0)

    withVAO (sVAO shape) $ do

      let model = transformationFromPose $ shiftBy paintingOffset (obj ^. cnkPose)  
      drawShape' model projection viewMat shape



  -- And their code
  -- glDisable GL_DEPTH_TEST
  glDisable GL_CULL_FACE
  glEnable  GL_BLEND
  forM_ sculptures $ \obj -> do
    let buffer = obj ^. scpBuffer
        font   = bufFont buffer
        pose   = id
               -- . rotateBy (axisAngle (V3 1 0 0) (-pi/4 - 0.4))
               . shiftBy (V3 (-sculptureSize/2) ( sculptureHeight+sculptureSize) sculptureSize/1.5)
               $ newPose { _posPosition = obj ^. scpPose . posPosition }
        model44 = transformationFromPose pose
        mvp = projection !*! viewMat !*! model44 !*! scaleMatrix 0.02
    renderText font (bufText buffer) (bufSelection buffer) mvp

  -- glEnable GL_DEPTH_TEST
  glEnable  GL_CULL_FACE
  glDisable GL_BLEND



  {-

    Render the Paintings

  -}
  paintings <- use wldPaintings

  forM_ ( zip paintingShapes ( Map.toList paintings ) ) $ \( shape , (objID, obj) ) -> do

    --let shape = (paintingShapes !! i)
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

  forM_ ( zip sculptureShapes ( Map.toList sculptures ) ) $ \( shape , (objID, obj) ) -> do

    ---let shape = (sculptureShapes !! i)
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



buildSculptures :: (Enum k, Num k, Ord k, Integral k) => Font -> IO (Map.Map k Sculpture)
buildSculptures font = do

  sculptureGeo <- cubeGeometry (V3 sculptureSize sculptureSize sculptureSize) (V3 1 1 1)

  fmap Map.fromList $ forM (zip [0..] sculptureShaders) $ \(i, shaderName) -> do
    let shaderPath = "app/shaders/sculptures/" ++ shaderName ++ ".frag"
    shaderComp <- shaderRecompiler "app/shaders/template/raytrace.vert" shaderPath (makeShape sculptureGeo)
    
    buffer <- bufferFromFile font shaderPath
    updateIndicesAndOffsets buffer
    let sculpture = Sculpture
                  { _scpPose     = getSculpturePose (fI i)
                  , _scpGetShape = shaderComp 
                  , _scpBuffer   = buffer
                  , _scpScroll   = 0
                  }

    return (i, sculpture)


buildPaintings :: (Enum k, Num k, Ord k, Integral k) => Font -> IO (Map.Map k Painting)
buildPaintings font = do
  
  paintingGeo <- planeGeometry (V2 (1.618 * paintingSize) paintingSize ) (V3 0 0 (-1)) (V3 0 1 0) (V2 1 1)

  -- paintingShaders defined in 'Types.hs'
  fmap Map.fromList $ forM (zip [0..] paintingShaders) $ \(i, shaderName) -> do
    let shaderPath = "app/shaders/paintings/" ++ shaderName ++ ".frag"
    shaderComp <- shaderRecompiler "app/shaders/template/raytrace.vert" shaderPath (makeShape paintingGeo)
    
    buffer <- bufferFromFile font shaderPath
    updateIndicesAndOffsets buffer
    let painting = Painting
                  { _pntPose     = getPaintingPose (fI i)
                  , _pntGetShape = shaderComp 
                  , _pntBuffer   = buffer
                  , _pntScroll   = 0
                  }

    return (i, painting)


buildChunks :: (Enum k, Num k, Ord k, Integral k) => Font -> IO (Map.Map k Chunk)
buildChunks font = do
  
  chunkGeo <- planeGeometry (V2 chunkSize chunkSize) (V3 0 0 (-1)) (V3 0 1 0) (V2 1 1)

  -- chunkShaders defined in 'Types.hs'
  fmap Map.fromList $ forM (zip [0..] chunkShaders) $ \(i, shaderName) -> do
    let shaderPath = "app/shaders/chunkRender/" ++ shaderName ++ ".frag"
    shaderComp <- shaderRecompiler "app/shaders/template/raytrace.vert" shaderPath (makeShape chunkGeo)
    
    buffer <- bufferFromFile font shaderPath
    updateIndicesAndOffsets buffer
    let chunk = Chunk
                  { _cnkPose     = getChunkPose (fI i)
                  , _cnkGetShape = shaderComp 
                  , _cnkBuffer   = buffer
                  , _cnkScroll   = 0
                  }

    return (i, chunk)


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

getChunkPose :: Int -> Pose GLfloat
getChunkPose i = pose

  where fI = fromIntegral i 

        id = mod' fI 15
        side = ((fromIntegral i ) - id ) / 15

        y = (mod' id 3)
        x = ((id - y ) / 3)

        z = (0.5 - side )* roomWidth
        pose = Pose{ _posPosition    = V3 z ((y-1) * (chunkSize + 0.1)) ((x-2.0) * 0.1 * roomWidth)
                   , _posOrientation = axisAngle (V3 0 1 0) ((1.0 * side + 0.5) * 3.14159)
                   }







 
