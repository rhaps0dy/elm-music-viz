import Graphics.Element exposing (..)
import WebGL exposing (webgl, trianglesEntity, pointsEntity, Shader)
import Math.Vector2 exposing (vec2, Vec2)
import Math.Vector2 as V2
import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import Math.Vector4 exposing (vec4, Vec4)
import Math.Vector4 as V4
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as M4
import Maybe exposing (Maybe)
import Signal
import Signal exposing ((<~), (~), Signal)
import Window
import Keyboard
import Time exposing (Time)
import Time
import Random exposing (..)
import List exposing (filter, map)
import Debug

type alias Color = Vec4

type alias Vertex = { col: Color
                    , pos: Vec3
                    }

type alias Particle = { t0 : Time -- ^ Time the particle was born
                      , lifespan : Time -- ^ Time the particle will live
                      , dir0 : Vec3 -- ^ Initial direction of the particle
                      }

type alias Fountain = { pos : Vec3 -- ^ Position of the particle source
                      , avgDir : Vec3 -- ^ Average direction of the particles
                      , col : Color -- ^ Color of the particles
                      , avgLifespan : Time -- ^ Average lifespan
                      , parts : List Particle -- ^ The particles this fountain has emitted
                      , seed : Random.Seed -- ^ Current random seed for generating more particles
                      }

gravity : Float
gravity = -9.81

-- | Calculate the position of a single particle
calcParticle : Fountain -- ^ The fountain this particle belongs to
            -> Time -- ^ Current time to be calculated
            -> Particle -- ^ Particle to be updated
            -> Vertex -- ^ Resulting vertex to draw
calcParticle {pos, col} t {t0, lifespan, dir0} =
    let t' = t - t0
        t'2 = t' * t'
        x = V3.getX pos + (V3.getX dir0) * t'
        y = V3.getY pos + (V3.getY dir0) * t' + (gravity * t'2)/2
        z = V3.getZ pos + (V3.getZ dir0) * t'
    in  {col = col, pos = vec3 x y z}

-- | Generate a random vec3 from (-1, -1, -1) to (1, 1, 1)
randVec3 : Generator Vec3
randVec3 = customGenerator <| \s ->
    let comp = float -1 1
        (x, s') = generate comp s
        (y, s'') = generate comp s'
        (z, s''') = generate comp s''
    in  (vec3 x y z, s''')

-- | Generate a particle for the fountain, slightly random.
particleGen : Fountain -> Time -> Generator Particle
particleGen {pos, avgDir, col, avgLifespan} t0 = customGenerator <| \s ->
    let (v, s') = generate randVec3 s
        varLifespan = avgLifespan * 0.1
        (lifespan', s'') = generate (float -varLifespan varLifespan) s'
        dir0 = V3.add avgDir (V3.scale 0.2 v)
    in  ({t0=t0, lifespan=lifespan' + avgLifespan, dir0=dir0}, s'')


-- | Step the fountain's time
updateFountain : Time -> Fountain -> Fountain
updateFountain t f =
    let pgen = particleGen f t
        (p, s) = generate pgen f.seed
        ps = filter (\{t0, lifespan} -> t0 + lifespan > t) f.parts
    in  {f | parts <- p :: ps, seed <- s}

dtSignal : Signal Time
dtSignal = Time.inSeconds <~ Time.fps 60

clockSignal : Signal Time
clockSignal = Time.inSeconds <~ Time.every (Time.second / 60)

fountain : Signal Fountain
fountain = Signal.foldp updateFountain
             { pos = vec3 1 0 0
             , avgDir = vec3 -1 1 0
             , col = vec4 1 0 0 1
             , avgLifespan = 2
             , parts = []
             , seed = Random.initialSeed 0
             } clock

cameraLookAt : Vec3 -- ^ Position of the camera
            -> Vec3 -- ^ Goal point the camera will look at
            -> (Int, Int) -- ^ Window size
            -> Mat4
cameraLookAt pos targ (w, h) =
    let aspectRatio = toFloat h / toFloat w
        perspective = M4.makePerspective 60 aspectRatio 1.0 100000.0
        lookAt = M4.makeLookAt pos targ V3.j
    in  M4.mul perspective lookAt
  
cameraPositionSignal : Signal {x:Int, y:Int}
cameraPositionSignal = Signal.foldp addDir {x = 0, y = 0} (Signal.sampleOn deltaT Keyboard.arrows)

addDir : {x: Int, y:Int} -> {x: Int, y:Int} -> {x:Int, y:Int}
addDir a b = {x=a.x+b.x, y=a.y+b.y}

particles : Signal (List Vertex)
particles = map <~ (calcParticle <~ fountain ~ clock) ~ (.parts <~ fountain)

main : Signal Element
main = scene <~ Window.dimensions ~ cameraPositionSignal ~ particles

scene : (Int, Int) -> {x:Int, y:Int} -> List Vertex -> Element
scene dimensions {x,y} vertices =
    let cam = cameraLookAt (vec3 (0.2*toFloat x) (0.2*toFloat y) 4) (vec3 0 0 0) dimensions
        e = pointsEntity vertShad fragShad vertices {camera = cam}
    in  webgl dimensions [e]

vertShad : Shader { pos : Vec3, col : Color} { camera: Mat4 } { f_col : Color }
vertShad = [glsl|

precision mediump float;

attribute vec3 pos;
attribute vec4 col;
uniform mat4 camera;
varying vec4 f_col;

void main() {
    gl_Position = camera * vec4(pos, 1);
    gl_PointSize = 40.0 / gl_Position.z;
    f_col = col;
}

|]

fragShad : Shader {} { camera: Mat4 } { f_col : Color }
fragShad = [glsl|

precision mediump float;
varying vec4 f_col;
uniform mat4 camera;

void main() {
     gl_FragColor = f_col;
}
|]