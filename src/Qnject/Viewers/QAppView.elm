module Qnject.Viewers.QAppView exposing
    ( Model
    , initialModel
    
    , Msg(..)
    
    , subscriptions
    , update
    , view
    , css
    )
{-| Describe me please...
-}

import Effects exposing (Effects)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, for, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Qnject.Qobject exposing (Address, QApp, QObjectSummary)
import Qnject.Connection as Connection
import Qnject.ViewerEffects exposing (ViewerEffect(OpenObjectView))
import Regex




-- MODEL


type alias Model =
    { searchString: Maybe String
    }


initialModel : Model
initialModel =
    { searchString = Nothing
    }


type alias Context =
    { model: Model
    , connection: Connection.Model
    }

contextFrom : Model -> Connection.Model -> Context
contextFrom = Context


-- MATCHING --------------------------------------------------------------------

objectsMatching : Model -> List QObjectSummary -> List QObjectSummary
objectsMatching { searchString } objs =
    case searchString of
        Nothing -> objs
        Just s ->
            let rx = Regex.regex s |> Regex.caseInsensitive
                matches ss = Regex.contains rx ss
                flt ss =
                    matches ss.objectName ||
                    matches ss.className ||
                    matches ss.address

            in List.filter flt objs


matchingSearch : Context -> List QObjectSummary
matchingSearch { model, connection } =
    connection.app
        |> Maybe.map .widgets
        |> Maybe.withDefault []
        |> objectsMatching model

-- MSG


type Msg
    = OpenAddress Address
    | SetSearchString String



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg, Effects ViewerEffect)
update msg model =
    case msg of
        OpenAddress addr -> (model, Cmd.none, Effects.from <| OpenObjectView addr)

        SetSearchString str ->
            let newS = if String.isEmpty str then Nothing else Just str
            in ({model | searchString = newS}, Cmd.none, Effects.none)



-- VIEW


view : Context -> Html Msg
view {model, connection} =
    case connection.app of
        Nothing -> text "No app"
        Just app ->
            div [ class "QAppView-view" ]
                [ headerView model app
                , objectsView model (objectsMatching model app.widgets)
                ]



-- VIEW: headerView



{-| header view
-}
headerView : Model -> QApp -> Html Msg
headerView model app =
    div [ class "header-view" ]
        [ searchBox model
        , span [ class "app-name" ] [ text app.appName ]
        , text " "
        , span [ class "address" ] [ text app.address ]
        , text " "
        , span [ class "app-object-count" ]
            [ text " with "
            , Html.b [ class "object-count" ]
                [ text <| toString <| List.length <| app.widgets ]
            , text " widget objects"
            ]
        ]

{-| CSS parts for headerView
-}
headerViewCss : String
headerViewCss = """
.header-view {  }
.header-view .app-name { display:inline-block; }
.header-view .app-name:before { content: "Application: " }
"""


-- VIEW: searchBox



{-| search box
-}
searchBox : Model -> Html Msg
searchBox model =
    let searchValue = model.searchString |> Maybe.withDefault ""
    in div [ class "search-box" ]
        [ Html.label [ class "search-label" ] [ text "Search:" ]
        , Html.input
            [ type_ "text"
            , value searchValue
            , onInput SetSearchString] []
        ]

{-| CSS parts for searchBox
-}
searchBoxCss : String
searchBoxCss = """
.search-box {  }
"""






-- VIEW: objectsView



{-| generic object list widget
-}
objectsView : Model -> List QObjectSummary -> Html Msg
objectsView model objs =
    div [ class "objects-view" ]
        [ Html.table [ class "qobject-table table" ]
            [ Html.thead []
                [ 
                ]

            , Html.Keyed.node "tbody" [] <| List.map (qobjectTableRow model) objs
            ]
        ]

{-| CSS parts for objectsView
-}
objectsViewCss : String
objectsViewCss = """
.objects-view {  }
.qobject-table { width:100%;  }
"""



-- VIEW: qobjectTableRow



{-| qobject table row
-}
qobjectTableRow : Model -> QObjectSummary -> (String, Html Msg)
qobjectTableRow  model obj =
    (obj.address,
    Html.tr
        [ class "qobject-table-row"
        , class <| "qobject-kind-" ++ toString obj.objectKind
        ]
        [ Html.td [ class "address", onClick <| OpenAddress obj.address ] [ text obj.address ]
        , Html.td [ class "object-name" ] [ text obj.objectName ]
        , Html.td [ class "class-name"] [ text obj.className ]
        , Html.td [ class "super-class" ] [ text obj.superClass ]
        ])

{-| CSS parts for qobjectTableRow
-}
qobjectTableRowCss : String
qobjectTableRowCss = """
.qobject-table-row {  }

"""








-- CSS

css : String
css = """
.QAppView-view {}
""" ++ headerViewCss ++ objectsViewCss ++ qobjectTableRowCss