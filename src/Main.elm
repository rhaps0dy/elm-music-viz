-- | Nota: dir-li font magica de montjuic
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
import Math
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
import Color

type alias GLColor = Vec4

fromColor : Color.Color -> GLColor
fromColor c =
    let (Color.RGBA r g b a) = toRGBA c
        r' = toFloat r / 255
        g' = toFloat g / 255
        b' = toFloat b / 255
    in  vec4 r' g' b' a

type alias Vertex = { col: GLColor
                    , pos: Vec3
                    }

type alias Particle = { t0 : Time -- ^ Time the particle was born
                      , lifespan : Time -- ^ Time the particle will live
                      , dir0 : Vec3 -- ^ Initial direction of the particle
                      }

type alias Fountain = { pos : Vec3 -- ^ Position of the particle source
                      , avgDir : Vec3 -- ^ Average direction of the particles
                      , col : GLColor -- ^ Color of the particles
                      , avgLifespan : Time -- ^ Average lifespan
                      , parts : List Particle -- ^ The particles this fountain has emitted
                      , seed : Random.Seed -- ^ Current random seed for generating more particles
                      , lastParticleT : Time -- ^ Time the last particle was generated in
                      , partGenInterval : Time -- ^ Time between generating particles
                      }

type alias State = { camPos : Vec3
                   , fountains : [Fountain]
                   }

gravity : Float
gravity = -9.81

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
        (lastT, ps') = if t - f.lastParticleT > f.partGenInterval then (t, p :: ps) else (f.lastParticleT, ps)
    in  {f | parts <- ps', seed <- s, lastParticleT <- lastT}

-- | Render a fountain to an entity
renderFountain : Mat4 -> Time -> Fountain -> Entity
renderFountain cam t f = pointsEntity vertShad fragShad (map (calcParticle f t) f.parts) {camera = cam}

fountains : [Fountain]
fountains =
    let colors = map fromColor [Color.lightRed, Color.green, Color.yellow,
                                Color.lightPurple, Color.lightGray,
                                Color.lightBrown, Color.lightBlue, Color.orange]
        ncolors = length colors
        color n = colors !! (n % ncolors)
        fountain n =
            let angle = degrees (360/12 * n)
                pos = vec3 (cos angle) 0 (sin angle)
                avgDir = vec3 (- cos angle) 2 (- sin angle)
                          |> scale 0.5
                col = color n
            in  { pos = pos
                , avgDir = avgDir
                , col = col
                , avgLifespan = 2
                , parts = []
                , seed = Random.initialSeed 0
                } 
    in  map fountain [1..12]

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

main : Signal Element
main = scene <~ Window.dimensions ~ (Time.inSeconds <~ Time.fps 60)

scene : (Int, Int) -> dt -> 
scene dimensions dt =
    let cam = cameraLookAt (vec3 (0.2*toFloat x) (0.2*toFloat y) 4) (vec3 0 0 0) dimensions
    in  webgl dimensions [e]


-- | Here be shaders
vertShad : Shader { pos : Vec3, col : GLColor} { camera: Mat4 } { f_col : GLColor }
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

fragShad : Shader {} { camera: Mat4 } { f_col : GLColor }
fragShad = [glsl|

precision mediump float;
varying vec4 f_col;
uniform mat4 camera;

void main() {
     gl_FragColor = f_col;
}
|]