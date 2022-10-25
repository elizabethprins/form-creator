module Main exposing (Model, Msg(..), main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import DnDList
import FormFields
    exposing
        ( FormField
        , FormFields
        , Mode(..)
        , MultipleChoiceType(..)
        , OpenType(..)
        , QuestionType(..)
        , radioField
        , textField
        )
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
    Sub.batch
        [ system.subscriptions model.dnd
        , clickOutsideFormFields model.formFields
        ]


clickOutsideFormFields : FormFields FormField -> Sub Msg
clickOutsideFormFields formFields =
    case formFields of
        FormFields.NoFocus _ _ ->
            Sub.none

        FormFields.HasFocus _ ->
            Sub.batch
                [ Browser.Events.onKeyUp (outside "formfields" RemoveFocus)
                , Browser.Events.onClick (outside "formfields" RemoveFocus)
                ]



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
    = NoOp
    | DndMsg DnDList.Msg
    | FocusOn FormField
    | RemoveFocus
    | Add
    | Duplicate
    | Remove
    | FocusOnOption ( UUID, String )
    | TextInput String
    | OptionInput String
    | AddOption
    | RemoveOption ( UUID, String )
    | SetOpenType OpenType
    | SetMultipleChoiceType MultipleChoiceType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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

        RemoveFocus ->
            ( { model
                | formFields =
                    FormFields.removeFocus model.formFields
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

        RemoveOption option ->
            ( { model
                | formFields =
                    FormFields.removeOption <| FormFields.focusOnOption option model.formFields
              }
            , Cmd.none
            )

        SetOpenType openType ->
            ( { model
                | formFields =
                    FormFields.setOpenType openType model.formFields
              }
            , Cmd.none
            )

        SetMultipleChoiceType multiType ->
            let
                ( optionId, newSeed ) =
                    Random.step UUID.generator model.seed
            in
            ( { model
                | formFields =
                    FormFields.setMultipleChoiceType multiType optionId model.formFields
                , seed = newSeed
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Form"
    , body =
        [ main_ [ class "main" ]
            [ section [ id "formfields" ]
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
                , viewQuestionTypeSelect formField.questionType
                , viewToolbar
                ]


viewViewingMode : FormField -> Html Msg
viewViewingMode formField =
    case formField.questionType of
        Open openType ->
            div []
                [ text formField.title
                , viewAnswerHint openType
                ]

        MultipleChoice multiType options ->
            div []
                [ text formField.title
                , viewOptionsList Viewing multiType (FormFields.toList options)
                ]


viewAnswerHint : OpenType -> Html Msg
viewAnswerHint openType =
    let
        hint =
            case openType of
                Short ->
                    "Short answer text"

                Long ->
                    "Long answer text"
    in
    div [ class "open-answer-hint" ] [ span [] [ text hint ] ]


viewToolbar : Html Msg
viewToolbar =
    div [ class "toolbar" ]
        [ button [ clickMsg Duplicate ] [ text "duplicate" ]
        , button [ clickMsg Add ] [ text "+ add" ]
        , button [ clickMsg Remove ] [ text "x" ]
        ]


viewEditingMode : FormField -> Html Msg
viewEditingMode formField =
    let
        label =
            toLabel formField.questionType
    in
    case formField.questionType of
        Open openType ->
            div []
                [ formFieldConfig
                    { toMsg = TextInput
                    , id = UUID.toString formField.id
                    , label = label
                    , value = formField.title
                    }
                    |> viewTextField
                , viewAnswerHint openType
                ]

        MultipleChoice multiType options ->
            div []
                [ formFieldConfig
                    { toMsg = TextInput
                    , id = UUID.toString formField.id
                    , label = label
                    , value = formField.title
                    }
                    |> viewTextField
                , viewOptionsList Editing multiType (FormFields.toList options)
                , button [ clickMsg AddOption ] [ text "+ add option" ]
                ]


viewOptionsList : Mode -> MultipleChoiceType -> List ( Mode, ( UUID, String ) ) -> Html Msg
viewOptionsList currentMode multiType options =
    let
        viewRadioItem ( _, ( id, opt ) ) =
            case currentMode of
                Viewing ->
                    text opt

                Editing ->
                    div []
                        [ formFieldConfig
                            { toMsg = OptionInput
                            , id = UUID.toString id
                            , label = toOptionLabel multiType
                            , value = opt
                            }
                            |> viewTextField
                        , button [ clickMsg (RemoveOption ( id, opt )) ] [ text "x" ]
                        ]
    in
    ul
        [ classList
            [ ( "multiple-choice-options", True )
            , ( "-radio", multiType == Radio )
            ]
        ]
        (List.map
            (\( mode, opt ) ->
                li [ onClick (FocusOnOption opt) ]
                    [ viewRadioItem ( mode, opt ) ]
            )
            options
        )


viewQuestionTypeSelect : QuestionType -> Html Msg
viewQuestionTypeSelect questionType =
    let
        options =
            Dict.fromList
                [ ( shortAnswerTitle, SetOpenType Short )
                , ( longAnswerTitle, SetOpenType Long )
                , ( radioAnswerTitle, SetMultipleChoiceType Radio )
                , ( checkboxAnswerTitle, SetMultipleChoiceType Checkbox )
                ]

        decoder =
            Decode.at [ "target", "value" ] Decode.string

        toMsg value =
            case Dict.get value options of
                Just msg ->
                    msg

                Nothing ->
                    NoOp
    in
    select
        [ class "question-type-select"
        , on "change" (Decode.map toMsg decoder)
        ]
        (List.map
            (\content ->
                let
                    isCurrent =
                        toQuestionTypeTitle questionType == content
                in
                option
                    [ selected isCurrent
                    , value content
                    ]
                    [ text content ]
            )
            (Dict.keys options)
        )



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


toQuestionTypeTitle : QuestionType -> String
toQuestionTypeTitle questionType =
    case questionType of
        Open Short ->
            shortAnswerTitle

        Open Long ->
            longAnswerTitle

        MultipleChoice Radio _ ->
            radioAnswerTitle

        MultipleChoice Checkbox _ ->
            checkboxAnswerTitle


toLabel : QuestionType -> String
toLabel questionType =
    case questionType of
        Open Short ->
            "Question with a short answer"

        Open Long ->
            "Question with a longer answer"

        MultipleChoice Radio _ ->
            "Multiple choice question with a single answer"

        MultipleChoice Checkbox _ ->
            "Multiple choice question with more than one answer"


toOptionLabel : MultipleChoiceType -> String
toOptionLabel multipleChoiceType =
    case multipleChoiceType of
        Radio ->
            "Option for multiple choice with a single answer"

        Checkbox ->
            "Option for multiple choice with more than one answer"


applyErrorLabel : a -> { b | errorLabel : a } -> { b | errorLabel : a }
applyErrorLabel errorLabel cfg =
    { cfg | errorLabel = errorLabel }


applyHasError : a -> { b | hasError : a } -> { b | hasError : a }
applyHasError hasError cfg =
    { cfg | hasError = hasError }



--- HELPERS


shortAnswerTitle : String
shortAnswerTitle =
    "Short answer"


longAnswerTitle : String
longAnswerTitle =
    "Paragraph"


radioAnswerTitle : String
radioAnswerTitle =
    "Multiple choice"


checkboxAnswerTitle : String
checkboxAnswerTitle =
    "Checkboxes"


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


outside : String -> Msg -> Decode.Decoder Msg
outside htmlId msg =
    Decode.field "target" (decodeIsOutside htmlId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed msg

                else
                    Decode.fail "is inside target"
            )


decodeIsOutside : String -> Decode.Decoder Bool
decodeIsOutside htmlId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if id == htmlId then
                        Decode.succeed False

                    else
                        Decode.fail "check parentNode"
                )
        , Decode.lazy
            (\_ -> Decode.field "parentNode" (decodeIsOutside htmlId))
        , Decode.succeed True
        ]
