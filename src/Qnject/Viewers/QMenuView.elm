module Qnject.Viewers.QMenuView
    exposing
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

import Dict
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, src, type_, value)
import Html.Keyed
import Qnject.Connection as Connection
import Qnject.Qobject exposing (Address, ObjectMethod, QAction, QMenu(..), QObjectDetails, QObjectSummary)


-- MODEL


type alias Model =
    {
    }


initialModel : Model
initialModel =
    {
    }


type alias Context =
    { model : Model
    , connection : Connection.Model
    }



-- MSG


type Msg
    = Noop



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []



-- VIEW


view : Context -> Html Msg
view ctx =
    let
        a = 1
--        obj =
--            Connection.objectAt ctx.model.address ctx.connection
    in
        div [ class "qobject-view" ]
            [ case ctx.connection.mainMenu of
                Nothing -> text "No menu loaded"
                Just menus -> div [] <| List.map menuView menus
            ]





-- CSS


css : String
css =
    """
.qobject-view {}
""" ++ menuViewCss
--++ screenshotViewCss ++ baseDataViewCss



-- VIEW: menuView



{-| menu view
-}
menuView : QMenu -> Html Msg
menuView menu =
    case menu of
        QMenu menu ->
            div [ class "menu-view" ]
                [ Html.h5 [] [ text "Menu @ ", span [ class "address" ] [ text <|menu.object.address ] ]

                , menu.actions
                    |> List.map actionView
                    |> List.map (\el -> Html.li [ class "menu-li" ] [el])
                    |> Html.ul [ class "actions" ]

                , menu.childMenus
                    |> List.map menuView
                    |> List.map (\el -> Html.li [ class "menu-li" ] [el])
                    |> Html.ul [ class "child-menus" ]
--                , Html.div [] <| List.map menuView menu.childMenus
                ]

{-| CSS parts for menuView
-}
menuViewCss : String
menuViewCss = """
.menu-view {  }
""" ++ actionViewCss



-- VIEW: actionView



{-| action view
-}
actionView : QAction -> Html Msg
actionView act =
    let
        action = act.action
        tag acc label = if acc action then span [ class "tag" ] [ text label ] else text ""

    in div [ class "action-view" ]
        [ Html.div [ class "action-name" ]
            [ Html.b [] [ text <| action.text ]
            , Html.small [] [ text action.toolTip]
            ]
        , tag .isCheckable "Checkable"
        , tag .isChecked "isChecked"
        , tag .isEnabled "isEnabled"
        , tag .isVisible "isVisible"
        ]

{-| CSS parts for actionView
-}
actionViewCss : String
actionViewCss = """
.action-view {  }
.action-view .action-name { display:inline-block; padding:0.2em 1em ; border-radius: 0.3em; border: 0.1em solid; }
.action-view .action-name b { display:block; }
.action-view .tag { display:inline-block; font-size:0.8em; padding:0 1em; border-radius: 0.3em; border: 0.1em solid; }
"""






---- VIEW: propertiesView
--
--
--{-| properties view
---}
--propertiesView : QObjectDetails -> Html Msg
--propertiesView obj =
--    let
--        row prop =
--            [ ( "name", text prop.name )
--            , ( "kind", text prop.kind )
--            , ( "value", Html.input [ type_ "text", value prop.value ] [] )
--            ]
--    in
--        tableView row "properties" [ "name", "kind", "value" ] obj.properties
--
--
--
----    div [ class "properties-view" ]
----        [
----        ]
--

{-| CSS parts for propertiesView
-}
propertiesViewCss : String
propertiesViewCss =
    """
.properties-view {  }
"""
