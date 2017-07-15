import ElmCss
import Html exposing (..)
import Html.Attributes exposing (class, target, href, defaultValue, type_, checked, placeholder, value)
import Html.Events exposing (..)
import Http 
import Auth
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import Regex exposing (regex, escape, HowMany(..))
import String exposing (..)
import Basics exposing (..)
import Tuple exposing (..)
import Json.Encode as Encode 
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = init
        , subscriptions = always Sub.none
        }

receta model =
    Encode.object
    [ ( "titulo", Encode.string model.titulo )
    , ( "ingredientes", Encode.list [(Encode.string "1 pera"), (Encode.string "1 cucharada de sal"), (Encode.string "1 cucharada de aceite de oliva") ])
    , ( "resumen", Encode.string model.resumen )
    , ( "porciones", Encode.int model.porciones )
    , ( "tiempo", Encode.int model.tiempo )
    , ( "instrucciones", Encode.string model.instrucciones )
    , ( "cocina", Encode.string model.cocina )
    ]


getListaIngredientes ingredientes =
    split "\n" ingredientes

type alias Model = 
    { titulo : String
    , ingredientes : String
    , resumen : String
    , porciones : Int
    , tiempo : Int
    , instrucciones : String
    , cocina : String
    , resultado : SearchResult
    , error : String
    }

type HealthLabels =
    Vegan
    | Vegetarian
    | DairyFree
    | LowSugar
    | LowFatAbs
    | SugarConscious
    | FatFree
    | GlutenFree
    | WheatFree

type DiabetLabels =
    Balanced
    | HighProtein
    | HighFiber
    | LowFat
    | LowCarb
    | LowSodium

type alias NutrienInfo = 
    { label : String
    , quantity : Float
    , unit : String
    }

type alias SearchResult = 
    { uri : String
    , yeld : Float
    , calories : Int
    , totalWeight : Float
--    , dietLabels : DiabetLabels
--    , healthLabels : HealthLabels
--    , totalNutrients : List (String, NutrienInfo)
    }

totalNutrientsDecoder : Decoder SearchResult
totalNutrientsDecoder =
    Json.Decode.at [ "totalNutrients" ] searchResultDecoder

responseDecoder : Decoder SearchResult
responseDecoder =
    Json.Decode.at [ ] searchResultDecoder

nutrienInfoDecoder : Decoder NutrienInfo
nutrienInfoDecoder =
    decode  NutrienInfo
        |> Json.Decode.Pipeline.required "label" Json.Decode.string
        |> Json.Decode.Pipeline.required "quantity" Json.Decode.float
        |> Json.Decode.Pipeline.required "unit" Json.Decode.string

searchResultDecoder : Decoder SearchResult
searchResultDecoder =
    decode  SearchResult
        |> Json.Decode.Pipeline.required "uri" Json.Decode.string
        |> Json.Decode.Pipeline.required "yield" Json.Decode.float
        |> Json.Decode.Pipeline.required "calories" Json.Decode.int
        |> Json.Decode.Pipeline.required "totalWeight" Json.Decode.float

type Msg =
    Send
    | InputTitulo String
    | InputIngredientes String
    | InputResumen String
    | IncrementarPorciones
    | DecrementarPorciones
    | DecrementarTiempo
    | IncrementarTiempo
    | InputInstrucciones String
    | InputCocina String
    | ResponseAPI (Result Http.Error SearchResult )

init : (Model, Cmd Msg )
init = 
    ( { titulo = ""
    , ingredientes = ""
    , resumen = "" 
    , porciones = 0
    , tiempo = 0 
    , instrucciones = "" 
    , cocina = ""
    , resultado = 
        { uri = ""
        , yeld = 0.0
        , calories = 0
        , totalWeight = 0.0
        }
    , error = ""
    }
    , Cmd.none
    )



sendRequestAPI : Model -> String -> Http.Request SearchResult
sendRequestAPI model urlAPI =
    Http.request
        { method = "POST"
        , expect = Http.expectJson responseDecoder
        , headers = [ ] --[ Http.header "Content-Type"  "application/json" ]
        , url = Debug.log "URLRequest: " urlAPI
        , body = Debug.log "recetaEncodeRequest : " Http.jsonBody (jsonMyEncoder model)
        , withCredentials = False
        , timeout = Nothing
        } 

jsonMyEncoder : Model -> Encode.Value
jsonMyEncoder model =
    Encode.object
        [ ( "title", Encode.string model.titulo )
        , ( "prep", Encode.string model.resumen )
        , ( "yield", Encode.string (toString model.porciones) )
        , ( "ingr", 
            Encode.object
                [ ( "ing1", Encode.string "1 apple" )
                , ( "ing2", Encode.string "1 onion" )
                , ( "ing3", Encode.string "1 potato" )
                , ( "ing4", Encode.string "1 tomatoe" )
                ]
        )
        ]
{-        [ ( "fields"
          , Encode.object
                [ ( "title", Encode.string model.titulo )
                , ( "prep", Encode.string model.resumen )
                , ( "author", Encode.string model.instrucciones )
                , ( "yield", Encode.string (toString model.porciones) )
                , ( "ingr", 
                    Encode.object
                       [ ( "ing1", Encode.string "1 apple" )
                       , ( "ing2", Encode.string "1 onion" )
                       , ( "ing3", Encode.string "1 potato" )
                       , ( "ing4", Encode.string "1 tomatoe" )
                       ]
                )
                ]
        )
        ]
        -}

getUrlAPI : Model -> String
getUrlAPI model = 
    Auth.url_api 
    ++ "?app_id=" ++ Auth.app_id
    ++ "&app_key=" ++ Auth.app_key
{-    ++ "&ingr=" ++ model.ingredientes -}

urlReplace : String -> String
urlReplace string =
    string
        |> Regex.replace All (regex " ") (always "%20")
        |> Regex.replace All (regex "\"") (always "")

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Send ->
            let
                urlAPI = urlReplace (getUrlAPI model)
            in
            ( model, Http.send ResponseAPI (sendRequestAPI model urlAPI ))
        ResponseAPI (Ok msg)->
            ({ model | resultado = Debug.log "Mensaje:" msg }, Cmd.none )
        ResponseAPI (Err error)->
            ( { model | error = Debug.log "error" (toString error) }, Cmd.none)
        InputTitulo titulo ->
            ( { model | titulo = Debug.log "titulo : " titulo }, Cmd.none )
        InputIngredientes ingredientes ->
            ( { model | ingredientes = Debug.log "ingredientes : " toString ingredientes }, Cmd.none )
        InputResumen resumen ->
            ( { model | resumen = Debug.log "resumen : " resumen }, Cmd.none )
        IncrementarPorciones ->
            ( { model | porciones = Debug.log "porciones : " model.porciones + 1 }, Cmd.none ) 
        DecrementarPorciones ->
            ( { model | porciones = Debug.log "porciones : " model.porciones - 1 }, Cmd.none )
        DecrementarTiempo ->
            ( { model | tiempo = Debug.log "tiempo : " model.tiempo - 1 }, Cmd.none )
        IncrementarTiempo ->
            ( { model | tiempo = Debug.log "tiempo : " model.tiempo + 1 }, Cmd.none )
        InputInstrucciones instrucciones ->
            ( { model | instrucciones = Debug.log "instrucciones : " instrucciones }, Cmd.none )
        InputCocina cocina ->
            ( { model | cocina = Debug.log "cocina : " cocina }, Cmd.none )

  
view : Model -> Html Msg
view model =
    div [ class "content"]
        [ Html.header []
            [ div [ class "head" ] 
                [ img [ src "img/images.jpg"] [] 
                , img [ src "img/images1.jpg"] []
                , h1 [] [ text "Nutriologo en Elm" ] 
                ] 
            ]
        , div [ class "content-body" ]
            [ div [ class "receta-form" ]
                [ h1 [] [ text "Receta"]
                , div [ class "row" ] 
                    [ label [ class"etiqueta" ] [ text "Titulo:" ]
                    , input [ class "text", onInput InputTitulo, placeholder "titulo..." ] []
                    ]
                , div [ class "row" ] 
                    [ label [ class"etiqueta" ] [ text "Ingredientes"]
                    , textarea [ class "text", placeholder "Ingredientes...", onInput InputIngredientes ] []
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Resumen"]
                    , textarea [ class "text", placeholder "resumen...", onInput InputResumen ] []
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Porciones"]
                    , button [ onClick DecrementarPorciones ] [ text "-" ]
                    , div [] [ text (toString model.porciones) ]
                    , button [ onClick IncrementarPorciones ] [ text "+" ]
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Tiempo"]
                    , button [ onClick DecrementarTiempo ] [ text "-" ]
                    , div [] [ text (toString model.tiempo) ]
                    , button [ onClick IncrementarTiempo ] [ text "+" ]
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Instrucciones:"]
                    , textarea [ class "text", placeholder "instrucciones...", onInput InputInstrucciones ] []
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Tipo de cocina: "]
                    , input [ class "text", onInput InputCocina, placeholder "cocina..." ] []
                    ]
                , div [ class "row" ] 
                    [ button [ class "btn", onClick Send ] [ text "Enviar" ]
                    ]
                ]
            , div [ class "receta-analisis" ]
                [   h1 [ ] [ text "Resultados" ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text model.error ]
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Rendimiento :"]
                    , label [ ] [ text ( toString model.resultado.yeld ) ]
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Calorias :"]
                    , label [ ] [ text ( toString model.resultado.calories ) ]
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Peso Total :"]
                    , label [ ] [ text ( toString model.resultado.totalWeight ) ]
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Etiqueta de la dieta :"]
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Nutrientes :"]
                    ]
                , div [ class "row" ] 
                    [ label [ class "etiqueta" ] [ text "Etiqueta de salud :"]
                    ]
                ]
            ]
        ]

getJsonReceta : Model -> String
getJsonReceta model =
    "{titulo:" ++ model.titulo
    ++ ",instrucciones:" ++ model.instrucciones 
    ++ ",cocina:" ++ model.cocina  
    ++ ",ingredientes:" ++ model.ingredientes 
    ++ ",resumen:" ++ model.resumen 
    ++ ",porciones:" ++ toString model.porciones 
    ++ ",tiempo:" ++ toString model.tiempo 
    ++ "}"
