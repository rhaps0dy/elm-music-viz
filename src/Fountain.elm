module Fountain (Model, init, Action, update, entity) where

import WebGL exposing (webgl, trianglesEntity, pointsEntity, Shader)
import Time exposing (Time)
import Time
import Math.Vector2 exposing (vec2, Vec2)
import Math.Vector2 as V2
import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import Math.Vector4 exposing (vec4, Vec4)
import Math.Vector4 as V4
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as M4
import Math
import Color
import Random exposing (..)

type alias GLColor = Vec4

fromColor : Color.Color -> GLColor
fromColor c =
    let (Color.RGBA r g b a) = toRGBA c
        r' = toFloat r / 255
        g' = toFloat g / 255
        b' = toFloat b / 255
    in  vec4 r' g' b' a

type alias Particle = { t0 : Time -- ^ Time the particle was born
                      , lifespan : Time -- ^ Time the particle will live
                      , dir0 : Vec3 -- ^ Initial direction of the particle
                      , pos0 : Vec3 -- ^ Initial position of the particle
                      , col : GLColor -- ^ Color of the particle
                      }

type alias Model = { pos : Vec3 -- ^ Position of the particle source
                      , avgDir : Vec3 -- ^ Average direction of the particles
                      , col : GLColor -- ^ Color of the particles
                      , avgLifespan : Time -- ^ Average lifespan
                      , parts : List Particle -- ^ The particles this fountain has emitted
                      , seed : Random.Seed -- ^ Current random seed for generating more particles
                      , lastParticleT : Time -- ^ Time the last particle was generated in
                      , partGenInterval : Time -- ^ Time between generating particles
                      }

-- | Here be shaders
vertShad : Shader { t0: Time
                  , lifespan: Time
                  , dir0: Vec3
                  , pos0 : Vec3
                  , col : GLColor
                  }
                  { camera: Mat4
                  , time: Time
                  } { f_col : GLColor }
vertShad = [glsl|
precision mediump float;

attribute float t0;
attribute float lifespan;
attribute vec3 dir0;
attribute vec3 pos0;
attribute vec4 col;
uniform mat4 camera;
uniform float time;
varying vec4 f_col;

void main() {
    float dt = t - t0;
    vec3 pos = pos0 + dir0 * dt;
    pos.y -= (9.81/2) * dt * dt;
    gl_Position = camera * vec4(pos, 1);
    gl_PointSize = 2.0;
    f_col = col;
}
|]

fragShad : Shader {}
                  { camera: Mat4
                  , time: Time
                  } { f_col : GLColor }
fragShad = [glsl|
precision mediump float;

uniform mat4 camera;
uniform float time;
varying vec4 f_col;

void main() {
     gl_FragColor = f_col;
}
|]