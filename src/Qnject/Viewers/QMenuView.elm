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
import Effects exposing (Effects)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, src, type_, value)
import Html.Events exposing (onClick)
import Html.Keyed
import Http
import Qnject.Connection as Connection
import Qnject.Qobject exposing (Address, ObjectMethod, QAction, QMenu(..), QObjectDetails, QObjectSummary)
import Qnject.ViewerEffects exposing (ViewerEffect(..))



-- MODEL


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


type alias Context =
    { model : Model
    , connection : Connection.Model
    }



-- MSG


type Msg
    = RequestTriggerAction Address
--    | OnActionTriggered (Result Http.Error String)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Effects ViewerEffect )
update msg model =
    case msg of
        RequestTriggerAction addr ->
            (model, Cmd.none, Effects.from <| TriggerAction addr)



-- VIEW


view : Context -> Html Msg
view ctx =
    let
        a =
            1

        --        obj =
        --            Connection.objectAt ctx.model.address ctx.connection
    in
        div [ class "qobject-view" ]
            [ case ctx.connection.mainMenu of
                Nothing ->
                    text "No menu loaded"

                --                Just menus -> div [] <| List.map menuView menus
                Just menus ->
                    div [] <| List.map (menuItemView ctx.model) menus
            ]



-- CSS


css : String
css =
    """
.qobject-view {}
"""
        ++ menuViewCss
        ++ menuItemViewCss



-- VIEW: menuView


{-| menu view
-}
menuView : QMenu -> Html Msg
menuView menu =
    case menu of
        QMenu menu ->
            div [ class "menu-view" ]
                [ Html.h5 [] [ text "Menu @ ", span [ class "address" ] [ text <| menu.object.address ] ]
                , menu.actions
                    |> List.map actionView
                    |> List.map (\el -> Html.li [ class "menu-li" ] [ el ])
                    |> Html.ul [ class "actions" ]
                , menu.childMenus
                    |> List.map menuView
                    |> List.map (\el -> Html.li [ class "menu-li" ] [ el ])
                    |> Html.ul [ class "child-menus" ]
                ]


{-| CSS parts for menuView
-}
menuViewCss : String
menuViewCss =
    """
.menu-view {  }
""" ++ actionViewCss



-- VIEW: actionView


{-| action view
-}
actionView : QAction -> Html Msg
actionView act =
    let
        action =
            act.action

        tag acc label =
            if acc action then
                span [ class "tag" ] [ text label ]
            else
                text ""
    in
        div [ class "action-view" ]
            [ Html.div [ class "action-name" ]
                [ Html.b [] [ text <| action.text ]
                , Html.small [] [ text action.toolTip ]
                ]
            , tag .isCheckable "Checkable"
            , tag .isChecked "isChecked"
            , tag .isEnabled "isEnabled"
            , tag .isVisible "isVisible"
            ]


{-| CSS parts for actionView
-}
actionViewCss : String
actionViewCss =
    """
.action-view {  }
.action-view .action-name { display:inline-block; padding:0.2em 1em ; border-radius: 0.3em; border: 0.1em solid; }
.action-view .action-name b { display:block; }
.action-view .tag { display:inline-block; font-size:0.8em; padding:0 1em; border-radius: 0.3em; border: 0.1em solid; }
"""



-- VIEW: menuItemView


{-| menu item view
-}
menuItemView : Model -> QMenu -> Html Msg
menuItemView model menu =
    let
        data =
            case menu of
                QMenu menu ->
                    menu
    in
        Html.table
            [ class "table menu-table" ]
            [ Html.tbody [] <| List.map (menuActionView model) data.actions
            , Html.tbody []
                [ Html.tr
                    [ class "menu-item-children" ]
                    [ Html.td
                        [ Html.Attributes.colspan 4
                        , class "menu-items-children-wrapper"
                        ]
                      <|
                        List.map (menuItemView model) data.childMenus
                    ]
                ]
            ]


{-| CSS parts for menuItemView
-}
menuItemViewCss : String
menuItemViewCss =
    """
.menu-item-view {  }
.menu-table { width:100%;  }
.menu-items-children-wrapper > table { margin-left: 2em; border-top: 0.2em solid; border-left: 0.5em solid; }
""" ++ menuActionViewCss



-- VIEW: menuActionView


{-| menu action view
-}
menuActionView : Model -> QAction -> Html Msg
menuActionView model { action, object } =
    Html.tr
        [ class "menu-action-view" ]
        [ Html.td [ class "menu-entry" ]
            [ span
                [ class "menu-title"
                , onClick <| RequestTriggerAction object.address
                ]
                [ text <| action.text ]
            , span [ class "menu-tooltip" ] [ text <| action.toolTip ]
            , span [ class <| "is-checked is-checked" ++ toString action.isChecked ] []
            ]
        ]


{-| CSS parts for menuActionView
-}
menuActionViewCss : String
menuActionViewCss =
    """
.menu-action-view {  }
.menu-entry { cursor: pointer; }
.menu-title:hover { text-decoration:underline; }
.menu-action-view .menu-title { font-weight:bold; }
.menu-action-view .menu-tooltip { font-size: 0.8em; font-style: italic; display:block;}
.menu-action-view {  }
"""
