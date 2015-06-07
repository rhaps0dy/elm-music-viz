module Fountain (Model, init, Action, update, entity) where
import WebGL exposing (webgl, trianglesEntity, pointsEntity, Shader, Entity)
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
import Color
import Random
import List exposing (filter, map)
import Debug

type alias GLColor = Vec4

type alias Vertex = { col: GLColor
                    , pos: Vec3
                    }


type alias Particle = { t0 : Time -- ^ Time the particle was born
                      , lifespan : Time -- ^ Time the particle will live
                      , dir0 : Vec3 -- ^ Initial direction of the particle
                      }

type alias Model = { pos : Vec3 -- ^ Position of the particle source
                   , avgDir : Vec3 -- ^ Average direction of the particles
                   , col : GLColor -- ^ Color of the particles
                   , avgLifespan : Time -- ^ Average lifespan
                   , parts : List Particle -- ^ The particles this fountain has emitted
                   , seed : Random.Seed -- ^ Current random seed for generating more particles
                   , lastParticleT : Time -- ^ Time the last particle was generated in
                   , partGenInterval : Time -- ^ Time between generating particles
                   , time : Time -- ^ Current time
                   }

type alias Action = Time

-- | Converts Elm color into WebGL color
fromColor : Color.Color -> GLColor
fromColor c =
    let {red, green, blue, alpha} = Color.toRgb c
        r = toFloat red / 255
        g = toFloat green / 255
        b = toFloat blue / 255
    in  vec4 r g b alpha

-- | Generate a random vec3 from (-1, -1, -1) to (1, 1, 1)
randVec3 : Random.Generator Vec3
randVec3 = Random.customGenerator <| \s ->
    let comp = Random.float -1 1
        (x, s') = Random.generate comp s
        (y, s'') = Random.generate comp s'
        (z, s''') = Random.generate comp s''
    in  (vec3 x y z, s''')

-- | Generate a particle for the fountain, slightly random.
particleGen : Model -> Time -> Random.Generator Particle
particleGen {pos, avgDir, col, avgLifespan} t0 = Random.customGenerator <| \s ->
    let (v, s') = Random.generate randVec3 s
        varLifespan = avgLifespan * 0.1
        (lifespan', s'') = Random.generate (Random.float -varLifespan varLifespan) s'
        dir0 = V3.add avgDir (V3.scale 0.2 v)
    in  ({t0=t0, lifespan=lifespan' + avgLifespan, dir0=dir0}, s'')

calcParticle : Model -- ^ The fountain this particle belongs to
            -> Particle -- ^ Particle to be updated
            -> Vertex -- ^ Resulting vertex to draw
calcParticle {pos, col, time} {t0, lifespan, dir0} =
    let t : Time
        t = time - t0
        p = V3.add pos <| V3.scale t dir0
        p' = V3.setY (V3.getY p - (9.81/2) * t * t) p
    in  {col = col, pos = p'}


-- | Step the fountain's time
update : Action -> Model -> Model
update dt f =
    let t = f.time + dt
        pgen = particleGen f t
        (p, s) = Random.generate pgen f.seed
        ps = filter (\{t0, lifespan} -> t0 + lifespan > t) f.parts
        (lastT, ps') = if t - f.lastParticleT > f.partGenInterval
                       then (t, p :: ps)
                       else (f.lastParticleT, ps)
    in  {f | parts <- ps', seed <- s, lastParticleT <- lastT, time <- t}

-- | Create a fountain
init : Vec3 -- ^ Position
    -> Vec3 -- ^ Average direction
    -> Color.Color -- ^ Fountain color
    -> Time -- ^ Average lifespan of particles
    -> Time -- ^ Time between generating particles
    -> Model
init pos dir col lifespan partdt =
    { pos = pos
    , avgDir = dir
    , col = fromColor col
    , avgLifespan = lifespan
    , parts = []
    , seed = Random.initialSeed 0
    , lastParticleT = 0.0
    , partGenInterval = partdt
    , time = 0.0
    }

-- | Render a fountain to an entity
entity : Mat4 -> Model -> Entity
entity cam f =
    pointsEntity vertShad fragShad (map (calcParticle f) f.parts) {camera = cam}

-- | Shader that calculates the rendering of all of a font's particles
vertShad : Shader {pos : Vec3, col : GLColor} {camera : Mat4} {f_col : GLColor}
vertShad  = [glsl|
precision mediump float;
uniform mat4 camera;
attribute vec3 pos;
attribute vec4 col;
varying vec4 f_col;

void main() {
    gl_Position = camera * vec4(pos, 1);
    gl_PointSize = 10.0;
    f_col = col;
}
|]

fragShad : Shader {} {camera : Mat4} {f_col : GLColor}
fragShad  = [glsl|
precision mediump float;
uniform mat4 camera;
varying vec4 f_col;

void main() {
    gl_FragColor = f_col;
}
|]