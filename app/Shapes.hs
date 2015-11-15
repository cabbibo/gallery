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

  tRaytraceProg     <- createShaderProgram vs "app/shaders/template/raytrace.frag"
  tFogProg          <- createShaderProgram vs "app/shaders/template/fogStep.frag"


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


  -- all the chunks to render!
  sdSphere        <- createShaderProgram vs "app/shaders/chunkRender/sdSphere.frag"
  sdBox           <- createShaderProgram vs "app/shaders/chunkRender/sdBox.frag"
  sdCappedCylinder<- createShaderProgram vs "app/shaders/chunkRender/sdCappedCylinder.frag"
  sdHexPrism      <- createShaderProgram vs "app/shaders/chunkRender/sdHexPrism.frag"
  sdPlane         <- createShaderProgram vs "app/shaders/chunkRender/sdPlane.frag"
  sdCappedCone    <- createShaderProgram vs "app/shaders/chunkRender/sdCappedCone.frag"
  sdTorus         <- createShaderProgram vs "app/shaders/chunkRender/sdTorus.frag"
  smoothU         <- createShaderProgram vs "app/shaders/chunkRender/smoothU.frag"
  opU             <- createShaderProgram vs "app/shaders/chunkRender/opU.frag"
  opS             <- createShaderProgram vs "app/shaders/chunkRender/opS.frag"
  xrotate         <- createShaderProgram vs "app/shaders/chunkRender/xrotate.frag"
  yrotate         <- createShaderProgram vs "app/shaders/chunkRender/yrotate.frag"
  zrotate         <- createShaderProgram vs "app/shaders/chunkRender/zrotate.frag"
  opRep           <- createShaderProgram vs "app/shaders/chunkRender/opRep.frag"
  opCheapBend     <- createShaderProgram vs "app/shaders/chunkRender/opCheapBend.frag"
  disform         <- createShaderProgram vs "app/shaders/chunkRender/disform.frag"


  {-

  calcSpec
  calcLamb
  calcFersnel
  procNoise
  fNoise
  refract col

  hsv

  reflect

  calcIntersection
  calcNormal
  calcAO

  disform
  calcLookAt

  mixColors

  fogColor ( stepwise calc intersection )



  -}




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


  c1  <- makeShape chunkGeo sdSphere
  c2  <- makeShape chunkGeo sdBox
  c3  <- makeShape chunkGeo sdHexPrism
 
  c4  <- makeShape chunkGeo sdCappedCylinder
  c5  <- makeShape chunkGeo sdCappedCone
  c6  <- makeShape chunkGeo sdTorus

  c7  <- makeShape chunkGeo opU
  c8  <- makeShape chunkGeo opS
  c9  <- makeShape chunkGeo smoothU

  c10  <- makeShape chunkGeo xrotate
  c11  <- makeShape chunkGeo yrotate
  c12  <- makeShape chunkGeo zrotate


  c13  <- makeShape chunkGeo opRep
  c14  <- makeShape chunkGeo opCheapBend
  c15  <- makeShape chunkGeo disform




  let shapes = Shapes{ _shpRoom        = roomShape
                     , _shpFrame       = frameShape
                     , _shpLight       = lightShape
                     , _shpPedestal    = pedestalShape
                     , _shpSculptures  = [s1 , s2 , s3 , s4 , s5 , s6, s7, s8]
                     , _shpPaintings   = [p1 , p2 , p4 , p3 , p6 , p5]
                     , _shpChunks      = [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c1,c1,c1,c1]
                     }

  return shapes
