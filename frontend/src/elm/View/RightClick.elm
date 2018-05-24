{-
   Copyright 2018 Daan Wendelen

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}


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
