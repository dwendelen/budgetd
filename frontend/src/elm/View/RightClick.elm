module View.RightClick exposing (onRightClick)

import Html exposing (Html)
import Html.Events exposing (onWithOptions)
import Json.Decode exposing (succeed)


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    onWithOptions
        "contextmenu"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.Decode.succeed msg)
