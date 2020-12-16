port module PhotoGallery exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes as Attr exposing(class, classList, id, name, src, title, type_)
import Html.Events exposing(on, onClick)
import Browser
import Array exposing(Array)
import Json.Encode
import Random
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Task exposing (succeed)


port setFilters : FilterOptions -> Cmd msg

port activityChanges : (String -> msg) -> Sub msg

type alias FilterOptions =
    { url : String 
    , filters : List { name : String, amount : Float }
    }

type alias Photo =
    { url : String
    , size : Int
    , title : String
    }

photoDecoder : Decoder Photo
photoDecoder =
    Json.Decode.succeed Photo
        |> Json.Decode.Pipeline.required "url" string
        |> Json.Decode.Pipeline.required "size" int
        |> Json.Decode.Pipeline.optional "title" string "(untitled)"

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String

type alias Model =
    { status : Status
    , activity : String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }

type ThumbnailSize 
    = Small
    | Medium
    | Large

type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotActivity String
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]

viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Json.Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]
        

viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
        [ button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!"]
        , div [ class "activity" ] [ text model.activity ]
        , div [ class "filters" ]
            [ viewFilter SlidHue "Hue" model.hue
            , viewFilter SlidRipple "Ripple" model.ripple
            , viewFilter SlidNoise "Noise" model.noise
            ]
        , h3 [] [ text "Thumbnail Size: " ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [Small, Medium, Large ])
        , div [id "thumbnails", class (sizeToString model.chosenSize) ] 
            (List.map (viewThumbnail selectedUrl) photos)
        , canvas [ id "main-canvas", class "large" ] []
        ]

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img 
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url) 
        ]
        []

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Large ->
            "large"
        Medium ->
            "med"
        Small ->
            "small"

initialModel : Model
initialModel = 
    { status = Loading
    , activity = ""
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    }

initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder)
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPhotos (Ok photos) ->
            case photos of
                (first :: rest) ->
                    applyFilters { model | status = Loaded photos first.url }
                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server Error!" }, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        ClickedPhoto url ->
            applyFilters { model | status = selectUrl  url model.status }

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( model, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )

applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]
                url =
                    urlPrefix ++ "large/" ++ selectedUrl 
            in
                ( model, setFilters { url = url, filters = filters } )
        
        Errored errorMessage ->
            ( model, Cmd.none )
        Loading ->
            ( model, Cmd.none )

selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status
        
        Errored errorMessage ->
            status


main : Program Float Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : Float -> (Model, Cmd Msg)
init flags =
    let
        activity =
            "Installing Pasta v" ++ String.fromFloat flags    
    in
        ( { initialModel | activity = activity }, initialCmd )

subscriptions : Model -> Sub Msg
subscriptions model =
    activityChanges GotActivity

rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children

onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"

photoFromUrl : String -> Photo
photoFromUrl url = Photo url 0 ""