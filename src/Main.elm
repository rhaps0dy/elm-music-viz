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
import List exposing (drop, head, length, map)
import Debug
import Color
import Fountain
import Debug

type alias Model = {fountains : List Fountain.Model}

(!!) : List a -> Int -> a
(!!) l i =
    let (Just elem) = drop i l |> head
    -- Exception if pattern matching fails
    in  elem

init : Model
init =
    let colors = [Color.lightRed, Color.green, Color.yellow,
                  Color.lightPurple, Color.lightGray,
                  Color.lightBrown, Color.lightBlue, Color.orange]
        ncolors = length colors
        color n = colors !! (n % ncolors)
        fountain n =
            let angle = degrees (360.0/12.0 * toFloat n)
                pos = vec3 (cos angle) 0 (sin angle)
                avgDir = vec3 -(cos angle) 2 -(sin angle) |>
                            V3.scale 0.5
                col = color n
            in  Fountain.init pos avgDir col 2 (1/40)
    in  {fountains = map fountain [1..12]}

cameraLookAt : Vec3 -- ^ Position of the camera
            -> Vec3 -- ^ Goal point the camera will look at
            -> (Int, Int) -- ^ Window size
            -> Mat4
cameraLookAt pos targ (w, h) =
    let aspectRatio = toFloat h / toFloat w
        perspective = M4.makePerspective 60 aspectRatio 1.0 100000.0
        lookAt = M4.makeLookAt pos targ V3.j
    in  M4.mul perspective lookAt
  
-- cameraPositionSignal : Signal {x:Int, y:Int}
-- cameraPositionSignal = Signal.foldp addDir {x = 0, y = 0} (Signal.sampleOn deltaT Keyboard.arrows)

-- addDir : {x: Int, y:Int} -> {x: Int, y:Int} -> {x:Int, y:Int}
-- addDir a b = {x=a.x+b.x, y=a.y+b.y}

update : Time -> Model -> Model
update t m = {fountains = map (Fountain.update t) m.fountains}

state : Signal Model
state = Signal.foldp update init (Time.inSeconds <~ Time.fps 60)

main : Signal Element
main = view <~ Window.dimensions ~ state

camera : (Int, Int) -> Mat4
camera = cameraLookAt (vec3 10 10 10) (vec3 0 0 0)

view : (Int, Int) -> Model -> Element
view dimensions {fountains} =
    let cam = camera dimensions
        fountainview = Fountain.entity cam
    in  webgl dimensions <| map fountainview fountains