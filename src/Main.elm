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
import Time exposing (Time)
import Time
import Random exposing (..)
import List exposing (drop, head, length, map)
import Mouse
import Color
import Fountain
import Debug

type alias Model = { fountains : List Fountain.Model
                   , camY : Float
                   , camAngle : Float
                   , camZ : Float
                   }

type alias Input = { x: Int
                   , y: Int
                   , prevX: Int
                   , prevY: Int
                   , pressed: Bool
                   , dt: Time
                   }

input : Signal Input
input =
    let dt = Time.inSeconds <~ Time.fps 30
        updateMouse x (xolder, xold) = (xold, x)
        msign = Signal.foldp updateMouse (0, 0)
        xsign = msign Mouse.x
        ysign = msign Mouse.y
        x = snd <~ xsign
        y = snd <~ ysign
        prevX = fst <~ xsign
        prevY = fst <~ ysign
    in Signal.sampleOn dt <| Input <~ x ~ y ~ prevX ~ prevY ~ Mouse.isDown ~ dt

(!!) : List a -> Int -> a
(!!) l i =
    let (Just elem) = drop i l |> head
    -- Exception if pattern matching fails
    in  elem

colors : List Color.Color
colors = [Color.lightRed, Color.green, Color.yellow, Color.lightPurple,
          Color.lightGray, Color.lightBrown, Color.lightBlue, Color.orange]

init : Int -> Model
init nfountains =
    let ncolors = length colors
        color n = colors !! (n % ncolors)
        fountain n =
            let angle = degrees (360.0/toFloat nfountains * toFloat n)
                pos = vec3 (cos angle) 0 (sin angle) |>
                          V3.scale 5
                avgDir = vec3 -(cos angle) 1 -(sin angle) |>
                          V3.scale 3.4
                col = color n
            in  Fountain.init pos avgDir col 2 (1/40)
    in  { fountains = map fountain [1..nfountains]
        , camY = 7
        , camAngle = 0
        , camZ = 7
        }

cameraLookAt : Vec3 -- ^ Position of the camera
            -> Vec3 -- ^ Goal point the camera will look at
            -> (Int, Int) -- ^ Window size
            -> Mat4
cameraLookAt pos targ (w, h) =
    let aspectRatio = toFloat w / toFloat h
        perspective = M4.makePerspective 60 aspectRatio 1.0 100000.0
        lookAt = M4.makeLookAt pos targ V3.j
    in  M4.mul perspective lookAt
  
update : Input -> Model -> Model
update inp m =
    let fountains = map (Fountain.update inp.dt) m.fountains
        camY = if inp.pressed then m.camY + toFloat (inp.y - inp.prevY) / 100.0 else m.camY
        camAngle = if inp.pressed then m.camAngle + degrees (toFloat (inp.x - inp.prevX)) else m.camAngle
    in { fountains = fountains
       , camY = camY
       , camAngle = camAngle
       , camZ = m.camZ
       }

state : Signal Model
state = Signal.foldp update (init 20) input

main : Signal Element
main = view <~ Window.dimensions ~ state

view : (Int, Int) -> Model -> Element
view (w, h) {fountains, camY, camAngle, camZ} =
    let x = camZ * cos camAngle
        z = camZ * sin camAngle
        cam = cameraLookAt (vec3 x camY z) (vec3 0 0 0) (w, h)
        fountainview = Fountain.entity cam
        scene = webgl (w, h) <| map fountainview fountains
    in  layers [color Color.black (spacer w h), scene]