module Qnject.Connection exposing (..)
{-| Describe me please...
-}

import Dict exposing (Dict)
import Error exposing (Error)
import Http
import Json.Decode
import Task
import Qnject.Qobject exposing (Address, QApp, QMainMenus, QMenu, QObjectDetails, QObjectSummary, decodeQApp, decodeQMenus, decodeQObjectDetails)




-- MODEL ------------------------


type alias Model =
    { url: String
    , app: Maybe QApp
    , objects: Dict Address QObjectDetails
    , mainMenu: Maybe QMainMenus
    , requestError: Maybe Http.Error
    }


default : Model
default =
    { url = "http://localhost:8000"
    , app = Nothing
    , objects = Dict.empty
    , mainMenu = Nothing
    , requestError = Nothing
    }


objectAt : Address -> Model -> Maybe QObjectDetails
objectAt addr model =
    Dict.get addr model.objects


getError : Model -> Maybe Http.Error
getError model =
    model.requestError

hasError : Model -> Bool
hasError model =
    case model.requestError of
        Nothing -> False
        Just _ -> True


-- MSG ------------------------

type Msg
    = ReloadAll
    | OnReloadApp (Result Http.Error QApp)

    | AddObject Address
    | OnReloadObject Address (Result Http.Error QObjectDetails)

    | ReloadMainMenu
    | OnReloadMainMenu (Result Http.Error QMainMenus)



url : String -> Model -> String
url path {url} =
    url ++ "/api" ++ path

get : Json.Decode.Decoder v -> String -> (Result Http.Error v -> msg) -> Model -> Cmd msg
get decoder path msg model =
    Http.send msg <| Http.get (url path model) decoder

-- HTTP ------------------------

type alias HttpFn v = (Result Http.Error v -> Msg) -> Model -> Cmd Msg
{-| Gets all widgets from the qnject server that
-}
allWidgets : (Result Http.Error QApp -> Msg) -> Model -> Cmd Msg
allWidgets = get decodeQApp "/qwidgets"

singleObject : Address -> HttpFn QObjectDetails
singleObject addr = get decodeQObjectDetails ("/qwidgets/by-address/" ++ addr)

-- UPDATE ------------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReloadAll ->
            ( model, Cmd.batch
                [ allWidgets OnReloadApp model
                , get decodeQMenus "/qwidgets/main-menu" OnReloadMainMenu model
                ])

        OnReloadApp (Ok app) ->
            ({ model | app = Just app, requestError = Nothing }, Cmd.none)

        OnReloadApp (Err err) ->
            ({ model | app = Nothing, requestError = Just err }, Cmd.none)


        AddObject a ->
            (model, singleObject a (OnReloadObject a) model)

        OnReloadObject a (Ok obj) ->
            { model | objects = Dict.insert a obj model.objects} ! []

        OnReloadObject a (Err err) ->
            ({ model | objects = Dict.remove a model.objects, requestError = Just err }, Cmd.none)


        ReloadMainMenu ->
            (model, get decodeQMenus "/qwidgets/main-menu" OnReloadMainMenu model)

        OnReloadMainMenu (Ok app) ->
            ({ model | mainMenu = Just app, requestError = Nothing }, Cmd.none)

        OnReloadMainMenu (Err err) ->
            ({ model | mainMenu = Nothing, requestError = Just err }, Cmd.none)
