module Main exposing (Model, Msg(..), main)

import Browser
import DnDList
import FormFields exposing (FormField, FormFields, Mode(..), QuestionType(..), radioField, textField)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Random exposing (Seed)
import UUID exposing (UUID)



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Flags =
    { seed : Int }


emptyFlags : Flags
emptyFlags =
    { seed = 0 }


fromFlags : Decode.Value -> Flags
fromFlags =
    Decode.decodeValue fromJson
        >> Result.withDefault emptyFlags


fromJson : Decoder Flags
fromJson =
    Decode.succeed Flags
        |> Pipeline.required "seed" Decode.int


{-| Generate a radio field with a single option and unique IDs, and a new seed.
-}
generateRadioField : Seed -> String -> ( FormField, Seed )
generateRadioField seed title =
    let
        ( fieldId, optionSeed ) =
            Random.step UUID.generator seed

        ( optionId, newSeed ) =
            Random.step UUID.generator optionSeed
    in
    ( radioField fieldId optionId title, newSeed )


{-| Generate a text field with a unique ID, and a new seed.
-}
generateTextField : Seed -> String -> ( FormField, Seed )
generateTextField seed title =
    let
        ( fieldId, newSeed ) =
            Random.step UUID.generator seed
    in
    ( textField fieldId title, newSeed )


{-| Generate a bunch of form fields with unique IDs, and a new seed.
-}
generateInitialFields : Seed -> ( List FormField, Seed )
generateInitialFields initialSeed =
    let
        generateFields title ( list, seed ) =
            let
                ( field, newSeed ) =
                    generateTextField seed title
            in
            ( field :: list, newSeed )
    in
    List.foldr generateFields ( [], initialSeed ) [ "grape", "cherry", "pear" ]


{-| Generate initial form data with a seed passed in through flags, and a new seed.
-}
initFormFields : Flags -> ( FormFields FormField, Seed )
initFormFields flags =
    let
        ( radioField, generatedSeed ) =
            generateRadioField (Random.initialSeed flags.seed) "apple"

        ( formFieldsList, newSeed ) =
            generateInitialFields generatedSeed
    in
    ( FormFields.init radioField formFieldsList, newSeed )



-- SYSTEM


config : DnDList.Config ( Mode, FormField )
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Swap
    }


system : DnDList.System ( Mode, FormField ) Msg
system =
    DnDList.create config DndMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dnd



-- MODEL


type alias Model =
    { dnd : DnDList.Model
    , formFields : FormFields FormField
    , seed : Seed
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        ( formFields, seed ) =
            initFormFields (fromFlags flags)
    in
    ( { dnd = system.model
      , formFields = formFields
      , seed = seed
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = DndMsg DnDList.Msg
    | FocusOn FormField
    | Add
    | Duplicate
    | Remove
    | FocusOnOption ( UUID, String )
    | TextInput String
    | OptionInput String
    | AddOption
    | RemoveOption


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DndMsg msg_ ->
            let
                ( dnd, formFieldsList ) =
                    system.update msg_ model.dnd (FormFields.toList model.formFields)

                formFieldsZipper =
                    case formFieldsList of
                        ( _, x ) :: xs ->
                            FormFields.init x (List.map Tuple.second xs)

                        _ ->
                            model.formFields

                editableField =
                    List.filter (\( mode, _ ) -> mode == Editing) formFieldsList
                        |> List.map Tuple.second
                        |> List.head

                newFormFields =
                    case editableField of
                        Just field ->
                            FormFields.focus field formFieldsZipper

                        Nothing ->
                            formFieldsZipper
            in
            ( { model
                | dnd = dnd
                , formFields = newFormFields
              }
            , system.commands dnd
            )

        FocusOn field ->
            ( { model
                | formFields =
                    FormFields.focus field model.formFields
              }
            , Cmd.none
            )

        Add ->
            let
                ( fieldId, newSeed ) =
                    Random.step UUID.generator model.seed
            in
            ( { model
                | formFields =
                    FormFields.add fieldId model.formFields
                , seed = newSeed
              }
            , Cmd.none
            )

        Duplicate ->
            let
                ( fieldId, newSeed ) =
                    Random.step UUID.generator model.seed
            in
            ( { model
                | formFields =
                    FormFields.duplicate fieldId model.formFields
                , seed = newSeed
              }
            , Cmd.none
            )

        Remove ->
            ( { model
                | formFields =
                    FormFields.remove model.formFields
              }
            , Cmd.none
            )

        FocusOnOption option ->
            ( { model
                | formFields =
                    FormFields.focusOnOption option model.formFields
              }
            , Cmd.none
            )

        TextInput input ->
            ( { model
                | formFields =
                    FormFields.editTitle input model.formFields
              }
            , Cmd.none
            )

        OptionInput input ->
            ( { model
                | formFields =
                    FormFields.editOption input model.formFields
              }
            , Cmd.none
            )

        AddOption ->
            let
                ( optionId, newSeed ) =
                    Random.step UUID.generator model.seed
            in
            ( { model
                | formFields =
                    FormFields.addOption optionId model.formFields
                , seed = newSeed
              }
            , Cmd.none
            )

        RemoveOption ->
            ( { model
                | formFields =
                    FormFields.removeOption model.formFields
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Form"
    , body =
        [ main_ [ class "main" ]
            [ section []
                [ div [ class "items" ] <|
                    List.indexedMap (viewItem model.dnd) (FormFields.toList model.formFields)
                , viewItemGhost model.dnd (FormFields.toList model.formFields)
                ]
            ]
        ]
    }


viewItem : DnDList.Model -> Int -> ( Mode, FormField ) -> Html.Html Msg
viewItem dnd index ( mode, formField ) =
    let
        fieldId =
            UUID.toString formField.id

        attrs =
            [ id fieldId
            , classList
                [ ( "item", True )
                , ( "-editing", mode == Editing )
                ]
            , onClick (FocusOn formField)
            ]
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                div (attrs ++ system.dropEvents index fieldId)
                    [ div [ class "item__handle" ] [ text ":::" ]
                    , div [ class "item__content" ] [ viewContent ( mode, formField ) ]
                    ]

            else
                div (attrs ++ [ class "-dragged" ])
                    [ div [ class "item__handle" ] [ text ":::" ]
                    , div [ class "item__content" ] [ viewContent ( mode, formField ) ]
                    ]

        Nothing ->
            div attrs
                [ div (class "item__handle" :: system.dragEvents index fieldId) [ text ":::" ]
                , div [ class "item__content" ] [ viewContent ( mode, formField ) ]
                ]


viewItemGhost : DnDList.Model -> List ( Mode, FormField ) -> Html.Html Msg
viewItemGhost dnd formFieldList =
    let
        maybeDragItem =
            system.info dnd
                |> Maybe.andThen (\{ dragIndex } -> formFieldList |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just ( mode, formField ) ->
            let
                classes =
                    classList
                        [ ( "item", True )
                        , ( "-ghost", True )
                        , ( "-editing", mode == Editing )
                        ]
            in
            div (classes :: system.ghostStyles dnd)
                [ div [ class "item__handle" ] [ text ":::" ]
                , div [ class "item__content" ] [ viewContent ( mode, formField ) ]
                ]

        Nothing ->
            text ""


viewContent : ( Mode, FormField ) -> Html Msg
viewContent ( mode, formField ) =
    case mode of
        Viewing ->
            viewViewingMode formField

        Editing ->
            div []
                [ viewEditingMode formField
                , viewToolbar
                ]


viewViewingMode : FormField -> Html Msg
viewViewingMode formField =
    case formField.questionType of
        Text ->
            text formField.title

        Radio options ->
            div []
                [ text formField.title
                , viewOptionsList Viewing (FormFields.toList options)
                ]


viewToolbar : Html Msg
viewToolbar =
    div [ class "toolbar" ]
        [ button [ clickMsg Duplicate ] [ text "duplicate" ]
        , button [ clickMsg Add ] [ text "+ add" ]
        , button [ clickMsg Remove ] [ text "x" ]
        ]


viewEditingMode : FormField -> Html Msg
viewEditingMode formField =
    case formField.questionType of
        Text ->
            formFieldConfig
                { toMsg = TextInput
                , id = UUID.toString formField.id
                , label = "Title for text question"
                , value = formField.title
                }
                |> viewTextField

        Radio options ->
            div []
                [ formFieldConfig
                    { toMsg = TextInput
                    , id = UUID.toString formField.id
                    , label = "Title for single choice question"
                    , value = formField.title
                    }
                    |> viewTextField
                , viewOptionsList Editing (FormFields.toList options)
                , button [ clickMsg AddOption ] [ text "add option" ]
                ]


viewOptionsList : Mode -> List ( Mode, ( UUID, String ) ) -> Html Msg
viewOptionsList currentMode options =
    let
        viewRadioItem ( mode, ( id, opt ) ) =
            case currentMode of
                Viewing ->
                    text opt

                Editing ->
                    if mode == Editing then
                        div []
                            [ formFieldConfig
                                { toMsg = OptionInput
                                , id = UUID.toString id
                                , label = "Single choice option"
                                , value = opt
                                }
                                |> viewTextField
                            , button [ clickMsg RemoveOption ] [ text "x" ]
                            ]

                    else
                        text opt
    in
    ul [ class "radio-options" ] <|
        List.map
            (\( mode, opt ) ->
                li [ onClick (FocusOnOption opt) ]
                    [ viewRadioItem ( mode, opt ) ]
            )
            options



-- VIEW FORM FIELDS


viewTextField : FormFieldConfig Msg -> Html Msg
viewTextField cfg =
    div []
        [ label [ for cfg.id, class "visually-hidden" ] [ text cfg.label ]
        , input
            [ id cfg.id
            , type_ "text"
            , value cfg.value
            , onInput cfg.toMsg
            ]
            []
        , viewIf cfg.hasError <|
            \_ ->
                span [ class "form-error" ]
                    [ text cfg.errorLabel ]
        ]



-- FORM STUFF


type alias FormFieldConfig msg =
    { id : String
    , label : String
    , value : String
    , toMsg : String -> msg
    , hasError : Bool
    , errorLabel : String
    }


formFieldConfig :
    { id : String
    , label : String
    , value : String
    , toMsg : String -> Msg
    }
    -> FormFieldConfig Msg
formFieldConfig { id, label, value, toMsg } =
    { id = id
    , label = label
    , value = value
    , toMsg = toMsg
    , hasError = False
    , errorLabel = ""
    }


applyErrorLabel : a -> { b | errorLabel : a } -> { b | errorLabel : a }
applyErrorLabel errorLabel cfg =
    { cfg | errorLabel = errorLabel }


applyHasError : a -> { b | hasError : a } -> { b | hasError : a }
applyHasError hasError cfg =
    { cfg | hasError = hasError }



--- HELPERS


viewIf : Bool -> (() -> Html Msg) -> Html Msg
viewIf bool html =
    if bool then
        html ()

    else
        text ""


clickMsg : Msg -> Attribute Msg
clickMsg msg =
    stopPropagationOn "click" (Decode.map alwaysStopPropagation (Decode.succeed msg))


alwaysStopPropagation : Msg -> ( Msg, Bool )
alwaysStopPropagation msg =
    ( msg, True )
