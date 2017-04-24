module Qnject.Qobject exposing (..)

{-| Describe me please...
-}

import Json.Decode as Decode exposing (field, string)


type alias ClassName =
    String


type alias ObjectName =
    String


type alias Address =
    String


type QObjectKind
    = KindQWidget
    | KindQObject


stringField name =
    field name string


enumField name decoder =
    field name string |> Decode.andThen decoder


{-| Represents a QObject gotten from the internal webserver
-}
type alias QObjectSummary =
    { objectName : ObjectName
    , address : Address
    , parentName : ObjectName
    , objectKind : QObjectKind
    , className : ClassName
    , superClass : ClassName
    }


decodeQObjectKind : String -> Decode.Decoder QObjectKind
decodeQObjectKind kindStr =
    case kindStr of
        "widget" ->
            Decode.succeed KindQWidget

        _ ->
            Decode.succeed KindQObject


decodeQObjectSummary : Decode.Decoder QObjectSummary
decodeQObjectSummary =
    Decode.map6 QObjectSummary
        (stringField "objectName")
        (stringField "address")
        (stringField "parentName")
        (enumField "objectKind" decodeQObjectKind)
        (stringField "className")
        (stringField "superClass")



-- DETAILS ------------------------


type alias Property =
    { name : String
    , kind : String
    , value : String
    }


type QAccessKind
    = PublicAccess
    | PrivateAccess
    | ProtectedAccess


type QMethodKind
    = Method
    | Constructor
    | Signal
    | Slot


decodeAccess str =
    case str of
        "public" ->
            Decode.succeed PublicAccess

        "private" ->
            Decode.succeed PrivateAccess

        "protected" ->
            Decode.succeed ProtectedAccess

        _ ->
            Decode.fail <| "Unknown access type: " ++ str


type alias ObjectMethod =
    { name : String
    , kind : String
    , access : QAccessKind
    , signature : String
    }


type alias QObjectDetails =
    { methods : List ObjectMethod
    , properties : List Property
    , summary : QObjectSummary
    }


decodeMethod : Decode.Decoder ObjectMethod
decodeMethod =
    Decode.map4 ObjectMethod
        (field "name" string)
        (field "type" string)
        (field "access" string
            |> Decode.andThen decodeAccess
        )
        (field "signature" string)


decodeProperty : Decode.Decoder Property
decodeProperty =
    Decode.map3 Property
        (field "name" string)
        (field "source" string)
        (field "value" string)


decodeQObjectDetails : Decode.Decoder QObjectDetails
decodeQObjectDetails =
    Decode.map3 QObjectDetails
        (field "methods" <| Decode.list decodeMethod)
        (field "properties" <| Decode.list decodeProperty)
        (field "object" decodeQObjectSummary)



-- MENUS -----------------------------------------------------------------------


type alias QAction=
    { action: QActionBase
    , object: QObjectSummary
    }

type alias QActionBase =
    { isCheckable : Bool
    , isChecked : Bool
    , isEnabled : Bool
    , isVisible : Bool
    , text : String
    , toolTip : String
    , whatsThis : String
    }


decodeQActionBase : Decode.Decoder QActionBase
decodeQActionBase =
    Decode.map7 QActionBase
        (field "isCheckable" Decode.bool)
        (field "isChecked" Decode.bool)
        (field "isEnabled" Decode.bool)
        (field "isVisible" Decode.bool)
        (stringField "text")
        (stringField "toolTip")
        (stringField "whatsThis")

decodeQAction : Decode.Decoder QAction
decodeQAction =
    Decode.map2 QAction
        (field "action" decodeQActionBase)
        (field "object" decodeQObjectSummary)

-- MENUS -----------------------------------------------------------------------

type alias QMainMenus = List QMenu

type QMenu
    = QMenu QMenuData


type alias QMenuData =
    { object : QObjectSummary
    , actions : List QAction
    , childMenus : List QMenu
    }

decodeQMenu : Decode.Decoder QMenu
decodeQMenu =
    Decode.map3 QMenuData
        (field "object" decodeQObjectSummary)
        (field "actions" (Decode.list decodeQAction))
        (field "childMenus" (Decode.list (Decode.lazy (\_ -> decodeQMenu))))
        |> Decode.andThen (Decode.succeed << QMenu )


decodeQMenus : Decode.Decoder (List QMenu)
decodeQMenus = Decode.list decodeQMenu

-- QAPP ------------------------


type alias QApp =
    { appName : String
    , address : Address
    , widgets : List QObjectSummary
    }


decodeQApp : Decode.Decoder QApp
decodeQApp =
    Decode.map3 QApp
        (Decode.field "appName" Decode.string)
        (Decode.field "qApp" Decode.string)
        (Decode.field "widgets" (Decode.list decodeQObjectSummary))
