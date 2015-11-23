{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Shapes where

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

galleryShapes :: IO (Shapes Uniforms)
galleryShapes = do 

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
  chunkGeo       <- planeGeometry (V2 chunkSize chunkSize) (V3 0 0 (-1)) (V3 0 1 0) (V2 1 1)

  let vs = "app/shaders/template/raytrace.vert"


  paintingShapes  <- makeShapes paintingGeo   vs "app/shaders/paintings/"    paintingShaders
  sculptureShapes <- makeShapes sculptureGeo  vs "app/shaders/sculptures/"   sculptureShaders
  chunkShapes     <- makeShapes chunkGeo      vs "app/shaders/chunkRender/"  chunkShaders

  {-

  calcIntersection
      
  refractCol      
       
  calcLookAt      
  reflectRay      

  -}




  let shapes = Shapes{ _shpRoom        = roomShape
                     , _shpFrame       = frameShape
                     , _shpLight       = lightShape
                     , _shpPedestal    = pedestalShape
                     , _shpSculptures  = sculptureShapes
                     , _shpPaintings   = paintingShapes
                     , _shpChunks      = chunkShapes
                     }

  return shapes


makeShapes geo vs path shaders = do

  shapes <- forM (zip [0..] shaders) $ \(i, shaderName) -> do

    let shaderPath = path ++ shaderName ++ ".frag"

    program <- createShaderProgram vs shaderPath
    shape   <- makeShape geo program

    return shape

  return shapes
