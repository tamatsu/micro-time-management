port module Main exposing (main)
import Browser
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes as A

import Json.Decode as D
import Task
import Time
import Dict exposing (Dict)

import Json.Encode as E
port store : E.Value -> Cmd msg




main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }
  
-- MODEL

type alias Model =
  { items: List Item
  , newText: String
  , time: Time.Posix
  , zone: Time.Zone
  , currentProject: Maybe String
  }

init : E.Value -> ( Model, Cmd Msg )
init value = 
  let
    cmd = Task.perform AdjustTimeZone Time.here
  in
    case D.decodeValue ( D.list itemDecoder ) value of
      Err error ->
        ( Model [] "" ( Time.millisToPosix 0 ) Time.utc Nothing, cmd )
      Ok items ->
        ( Model items "" ( Time.millisToPosix 0 ) Time.utc Nothing, cmd )

encodeModel : Model -> E.Value
encodeModel model =
  E.list encodeItem model.items

        
type alias Item =
  { content : String
  , time : Time.Posix
  , project : Maybe String
  }
  
encodeItem : Item -> E.Value
encodeItem item =
  E.object
    [ ( "content", E.string item.content )
    , ( "time", E.int ( Time.posixToMillis item.time ) )
    , ( "project"
        , case item.project of
            Just string -> E.string string
            Nothing -> E.null
      )
    ]

itemDecoder : D.Decoder Item
itemDecoder =
  D.map3 Item 
    ( D.field "content" D.string )
    ( D.field "time" D.int
        |> D.map Time.millisToPosix
    )
    ( D.field "project" ( D.nullable D.string ) )
            
  
-- UPDATE

type Msg 
  = Push String
  | NewText String
  | NewProject String
  | Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | SetProject ( Maybe String )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Push comment ->
      let
        newmodel = 
          { model
            | items = ( Item comment model.time model.currentProject ) :: model.items 
            , newText = ""
          }
      in
        ( newmodel, store ( encodeModel newmodel ) )
    NewText newText ->
      ( { model | newText = newText }, Cmd.none )
      
    NewProject currentProject ->
      let
        p : String -> Maybe String
        p s =
          if s == "" then Nothing
          else Just s
      in
        ( { model | currentProject = p currentProject }, Cmd.none )
      
    Tick newTime ->
      ( { model | time = newTime }, Cmd.none )
      
    AdjustTimeZone zone ->
      ( { model | zone = zone }, Cmd.none )
      
    SetProject project ->
      ( { model | currentProject = project }, Cmd.none )
    
      
      
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model
  = Time.every 1000 Tick


-- VIEW

view : Model -> Html Msg
view model =
  div [ A.attribute "class" "container" ]
    [ div [ A.attribute "class" "first" ]
        [ div [ A.attribute "class" "control" ]
            [ textarea
                [ A.placeholder "new comment"
                , A.value model.newText
                , onInput NewText 
                , A.attribute "class" "new-comment"
                ] []
            , button [ onClick ( Push model.newText ) ] [ text "+" ]
            , textarea
                [ A.placeholder "current project"
                , A.value ( Maybe.withDefault "" model.currentProject )
                , A.style "background-color" ( projectColor model.currentProject )
                , onInput NewProject
                ] []
            ]
        , div []
            ( viewItems model )
        ]
      , div [ A.attribute "class" "second" ]
          -- ( graphView model )
          ( summaryView model )
      , div []
          ( graphView model )
      , div 
          [ A.attribute "class" "shadow px-2 fixed top-0 right-0" 
          , A.style "background-color" ( projectColor model.currentProject )
          ]
          ( groupedView model )
    ]
    

summaryView : Model -> List ( Html Msg )
summaryView model =
  let
    dict = groupbyProject ( forTheDay model model.items )
    list = Dict.toList dict
    foldDuration : ( List Item ) -> Int -> Int
    foldDuration items n = duration items + n
    sum = List.foldl foldDuration 0 ( List.map Tuple.second list )
    toHtml : ( String, List Item ) -> Html Msg
    toHtml ( project, items ) =
      div
        [ A.attribute "class" "flex justify-between border-b"
        , A.style "background-color" ( projectColor ( Just project ) )
        ]
        [ div [ A.attribute "class" "break-all"] [ text project ]
        , div [ A.attribute "class" "text-pink-800"] [ text ( String.fromInt ( duration items // ( 1000 * 60 ) ) )
                 , text "min."
                 ]
        ]
    total = 
      [ div [ A.attribute "class" "flex justify-between"]
          [ div []
              [ text "Total" ]
          , div []
              [ text ( String.fromInt ( sum // ( 1000 * 60 ) ) )
              , text " min." 
              ]
          ]
      ]
  in
    List.append ( List.map toHtml list ) total

            

type Element
  = Content Item
  | Separator Time.Posix

millisOf : Element -> Int
millisOf element =
  case element of
    Content item ->
      item.time |> Time.posixToMillis
    Separator time ->
      time |> Time.posixToMillis

viewElement : Model -> Element -> Html Msg
viewElement model element =
  let
    toString : ( Time.Zone -> Time.Posix -> Int ) -> Time.Posix -> String
    toString f time =
      f model.zone time
        |> String.fromInt
        |> String.pad 2 '0'
        
    secondsAgo : Time.Posix -> Int
    secondsAgo time = 
      ( ( Time.posixToMillis time ) - ( Time.posixToMillis model.time ) ) // 1000
  in
    case element of
      Content item ->
        viewItem model item
      Separator time ->
        p [ A.style "color" "#888"
          , A.attribute "class" "separator"
          ] 
          [ small
            []
            [ text " "
            , text (String.fromInt ( ( secondsAgo time ) // 60 ) )
            , text " min. "
            , text " "
            , text ( toString Time.toHour time )
            , text ":"
            , text ( toString Time.toMinute time )
            , text " "
            ]
          ]

projectColor : Maybe String -> String
projectColor p =
  case p of
    Just project ->
      "hsl(" ++ ( String.fromInt ( hash project * 360 |> ceiling ) ) ++ ", 90%, 90%"
    Nothing ->
      "white"

projectFgColor : Maybe String -> String
projectFgColor p =
  case p of
    Just project ->
      "hsl(" ++ ( String.fromInt ( hash project * 360 |> ceiling ) ) ++ ", 80%, 40%"
    Nothing ->
      "hsl(0, 0%, 40%)"

viewItem : Model -> Item -> Html Msg
viewItem model item =
  let
    hour = Time.toHour model.zone item.time
    minute = Time.toMinute model.zone item.time
    second = Time.toSecond model.zone item.time
    timeString
      = ( String.pad 2 '0' ( String.fromInt hour ) )
      ++ ":" ++ ( String.pad 2 '0' ( String.fromInt minute ) )
    

  in
    p [ A.attribute "class" "content"
      , A.attribute "id" ( String.fromInt ( Time.posixToMillis item.time ) )
      , A.style "background-color" ( projectColor item.project )
      ]
      [ div
          [ A.attribute "class" "prompt-head"
          , A.style "color" ( projectFgColor item.project )
          , onClick ( SetProject item.project )

          ] 
          [ text "["
          , text ( Maybe.withDefault "" item.project )
          , text "] "
          , div [ A.attribute "class" "time" ] [ text timeString ]
          ]
        , text "> "
        , text item.content

      ]

viewItems : Model -> List ( Html Msg )
viewItems model =
  let
    ago : Time.Posix -> Int -> Time.Posix
    ago time second = ( Time.posixToMillis time ) - second * 1000 |> Time.millisToPosix

    separators =
      List.range 0 144
        |> List.map ( (*) 300 )
        |> List.map ( ago model.time )
        |> List.map Separator

  in
    model.items
      |> List.take 100
      |> List.map Content
      |> List.append separators
      |> List.sortBy millisOf
      |> List.reverse
      |> List.map ( viewElement model )

      
hash : String -> Float
hash s =
  ( s
      |> String.toList
      |> List.map Char.toCode
      |> List.foldr ( lcg 216091 ) 23209
      |> toFloat
  ) / 216091
    
lcg : Int -> Int -> Int -> Int
lcg m a b =
  modBy m ( a * b )
    
graphView : Model -> List ( Html Msg )
graphView model =
  model.items
    |> List.take 100
    |> List.reverse
    |> List.map ( itemPlot model )
    
itemPlot : Model -> Item -> Html Msg
itemPlot model item =
  let
    pos : String
    pos = 
      ( ( Time.posixToMillis model.time ) - ( Time.posixToMillis item.time ) ) // ( 60 * 1000 ) + 10
        |> String.fromInt

  in     
    a 
      [ A.attribute "href" ( "#" ++ ( String.fromInt ( Time.posixToMillis ( item.time ) ) ) ) ]
      [ div
          [ A.style "position" "fixed"
          , A.style "top" ( pos ++ "px" )
          , A.style "width" "32px"
          , A.style "height" "10px"
          , A.style "color" ( projectColor item.project )
          , A.style "background-color" ( projectColor item.project )
          --, A.style "font-size" "0.5rem"
          , A.attribute "title" ( Maybe.withDefault "" item.project )
          ]
          []
      ]

type alias ItemDict =
  Dict String ( List Item )

groupbyProject : List Item -> ItemDict
groupbyProject items =
  List.foldl appendItem Dict.empty items
  
appendItem : Item -> ItemDict -> ItemDict
appendItem item dict =
  if ( List.length ( Maybe.withDefault [] ( Dict.get ( Maybe.withDefault "" item.project ) dict ) ) == 0 ) then
    Dict.insert ( Maybe.withDefault "" item.project ) ( List.singleton item ) dict
  else
    Dict.update ( Maybe.withDefault "" item.project ) ( updateDict item ) dict

groupedView : Model -> List ( Html Msg )
groupedView model =
  let
    dict = groupbyProject model.items
    project = Maybe.withDefault "" model.currentProject
    durmin = 
      Dict.get project dict
        |> Maybe.withDefault []
        |> forTheDay model
        |> duration
        |> toFloat
        |> (*) ( 1 / ( 1000 * 60 ) )
        |> floor
        |> String.fromInt
  in
    [ div [ A.style "max-width" "100px" ]
        [ text project
        , text " duration(today): "
        , text durmin
        , text " min."
        ]
    ]


updateDict : Item -> ( Maybe ( List Item ) -> Maybe ( List Item ) )
updateDict item maybeItems =
  case maybeItems of
    Just items ->
      Just ( List.append ( List.singleton item ) items )
    Nothing ->
      Just ( List.singleton item )
  
duration : List Item -> Int
duration items =
  let
    millis = items |> List.map .time |> List.map Time.posixToMillis
    max = List.maximum millis |> Maybe.withDefault 0
    min = List.minimum millis |> Maybe.withDefault 0
  in
    max - min

forTheDay : Model -> List Item -> List Item
forTheDay model items =
  let
    today : Item -> Bool
    today item
      = Time.toYear model.zone model.time == Time.toYear model.zone item.time
      && Time.toMonth model.zone model.time == Time.toMonth model.zone item.time
      && Time.toDay model.zone model.time == Time.toDay model.zone item.time
  
  in
    List.filter today items