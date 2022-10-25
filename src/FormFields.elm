module FormFields exposing
    ( FormField
    , FormFields(..)
    , Mode(..)
    , MultipleChoiceType(..)
    , OpenType(..)
    , QuestionType(..)
    , add
    , addOption
    , duplicate
    , editOption
    , editTitle
    , focus
    , focusOnOption
    , init
    , next
    , previous
    , radioField
    , remove
    , removeFocus
    , removeOption
    , setMultipleChoiceType
    , setOpenType
    , textField
    , toList
    )

import List.Zipper as Zipper exposing (Zipper)
import UUID exposing (UUID)


{-| A FormField can have focus (be editable) or not.

    NoFocus a (List a) -- a non empty list

    HasFocus (Zipper a) -- a List where one element has focus

-}
type FormFields a
    = NoFocus a (List a)
    | HasFocus (Zipper a)


type alias FormField =
    { id : UUID
    , title : String
    , questionType : QuestionType
    }


type QuestionType
    = Open OpenType
    | MultipleChoice MultipleChoiceType (FormFields ( UUID, String ))


type OpenType
    = Short
    | Long


type MultipleChoiceType
    = Radio
    | Checkbox


type Mode
    = Viewing
    | Editing


init : a -> List a -> FormFields a
init =
    NoFocus


focus : a -> FormFields a -> FormFields a
focus field formFields =
    case formFields of
        NoFocus x xs ->
            case Zipper.findFirst ((==) field) (Zipper.fromCons x xs) of
                Nothing ->
                    formFields

                Just zipper_ ->
                    HasFocus zipper_

        HasFocus zipper ->
            case Zipper.findFirst ((==) field) zipper of
                Nothing ->
                    formFields

                Just zipper_ ->
                    HasFocus zipper_


removeFocus : FormFields a -> FormFields a
removeFocus formFields =
    case formFields of
        NoFocus _ _ ->
            formFields

        HasFocus formFieldZipper ->
            formFieldZipper
                |> Zipper.first
                |> (\a -> NoFocus (Zipper.current a) (Zipper.after a))


add : UUID -> FormFields FormField -> FormFields FormField
add fieldId formFields =
    let
        newField =
            textField fieldId "new question"
    in
    case formFields of
        HasFocus formFieldZipper ->
            let
                newFormFieldsList =
                    Zipper.from
                        (Zipper.before formFieldZipper)
                        (Zipper.current formFieldZipper)
                        (newField
                            :: Zipper.after formFieldZipper
                        )
            in
            focus newField (HasFocus newFormFieldsList)

        NoFocus _ _ ->
            formFields


duplicate : UUID -> FormFields FormField -> FormFields FormField
duplicate fieldId formFields =
    case formFields of
        HasFocus formFieldZipper ->
            let
                duplicateField =
                    Zipper.current formFieldZipper
                        |> (\f -> { f | id = fieldId })

                newFormFieldsList =
                    Zipper.from
                        (Zipper.before formFieldZipper)
                        (Zipper.current formFieldZipper)
                        (duplicateField
                            :: Zipper.after formFieldZipper
                        )
            in
            focus duplicateField (HasFocus newFormFieldsList)

        NoFocus _ _ ->
            formFields


remove : FormFields FormField -> FormFields FormField
remove formFields =
    case previous formFields of
        HasFocus formFieldZipper ->
            let
                newFormFieldsList =
                    Zipper.from
                        (Zipper.before formFieldZipper)
                        (Zipper.current formFieldZipper)
                        (List.drop 1 <| Zipper.after formFieldZipper)
            in
            HasFocus newFormFieldsList

        NoFocus _ xs ->
            Maybe.map HasFocus (Zipper.fromList xs)
                |> Maybe.withDefault formFields


focusOnOption : ( UUID, String ) -> FormFields FormField -> FormFields FormField
focusOnOption option formFields =
    case formFields of
        HasFocus formFieldZipper ->
            HasFocus <|
                Zipper.mapCurrent
                    (\f ->
                        case f.questionType of
                            Open _ ->
                                f

                            MultipleChoice multiType options ->
                                { f | questionType = MultipleChoice multiType <| focus option options }
                    )
                    formFieldZipper

        NoFocus _ _ ->
            formFields


previous : FormFields a -> FormFields a
previous formFields =
    case formFields of
        NoFocus x xs ->
            HasFocus <|
                Zipper.last (Zipper.fromCons x xs)

        HasFocus zipper ->
            case Zipper.previous zipper of
                Nothing ->
                    NoFocus (Zipper.current (Zipper.first zipper))
                        (Zipper.after (Zipper.first zipper))

                Just zipper_ ->
                    HasFocus zipper_


next : FormFields a -> FormFields a
next formFields =
    case formFields of
        NoFocus x xs ->
            HasFocus <|
                Zipper.first (Zipper.fromCons x xs)

        HasFocus zipper ->
            case Zipper.next zipper of
                Nothing ->
                    NoFocus (Zipper.current (Zipper.first zipper))
                        (Zipper.after (Zipper.first zipper))

                Just zipper_ ->
                    HasFocus zipper_


toList : FormFields a -> List ( Mode, a )
toList formFields =
    case formFields of
        NoFocus x xs ->
            List.map (\f -> ( Viewing, f )) (x :: xs)

        HasFocus formFields_ ->
            List.concat
                [ List.map (\f -> ( Viewing, f )) (Zipper.before formFields_)
                , [ ( Editing, Zipper.current formFields_ ) ]
                , List.map (\f -> ( Viewing, f )) (Zipper.after formFields_)
                ]


textField : UUID -> String -> FormField
textField id string =
    { id = id
    , title = string
    , questionType = Open Short
    }


radioField : UUID -> UUID -> String -> FormField
radioField fieldId optionId string =
    { id = fieldId
    , title = string
    , questionType = MultipleChoice Radio (init ( optionId, "first option" ) [])
    }


editTitle : String -> FormFields FormField -> FormFields FormField
editTitle input formFields =
    case formFields of
        HasFocus formFieldZipper ->
            HasFocus <|
                Zipper.mapCurrent
                    (\f -> { f | title = input })
                    formFieldZipper

        NoFocus _ _ ->
            formFields


editOption : String -> FormFields FormField -> FormFields FormField
editOption input =
    updateOption
        (\options ->
            case options of
                HasFocus optionsZipper ->
                    HasFocus <|
                        Zipper.mapCurrent (\( id, _ ) -> ( id, input )) optionsZipper

                NoFocus _ _ ->
                    options
        )


addOption : UUID -> FormFields FormField -> FormFields FormField
addOption optionId =
    updateOption
        (\options ->
            case options of
                HasFocus optionsZipper ->
                    HasFocus <|
                        Zipper.from (Zipper.toList optionsZipper) ( optionId, "new option" ) []

                NoFocus x xs ->
                    init x (xs ++ [ ( optionId, "new option" ) ])
        )


removeOption : FormFields FormField -> FormFields FormField
removeOption =
    updateOption
        (\options ->
            case previous options of
                HasFocus optionsZipper ->
                    let
                        newOptions =
                            Zipper.from
                                (Zipper.before optionsZipper)
                                (Zipper.current optionsZipper)
                                (List.drop 1 <| Zipper.after optionsZipper)
                    in
                    HasFocus newOptions

                NoFocus _ xs ->
                    Maybe.map HasFocus (Zipper.fromList xs)
                        |> Maybe.withDefault options
        )


updateOption : (FormFields ( UUID, String ) -> FormFields ( UUID, String )) -> FormFields FormField -> FormFields FormField
updateOption updateFn formFields =
    case formFields of
        HasFocus formFieldZipper ->
            HasFocus <|
                Zipper.mapCurrent
                    (\f ->
                        case f.questionType of
                            Open _ ->
                                f

                            MultipleChoice multiType options ->
                                let
                                    newOptions =
                                        updateFn options
                                in
                                { f | questionType = MultipleChoice multiType newOptions }
                    )
                    formFieldZipper

        NoFocus _ _ ->
            formFields


setOpenType : OpenType -> FormFields FormField -> FormFields FormField
setOpenType openType formFields =
    case formFields of
        HasFocus formFieldZipper ->
            HasFocus <|
                Zipper.mapCurrent
                    (\f -> { f | questionType = Open openType })
                    formFieldZipper

        NoFocus _ _ ->
            formFields


setMultipleChoiceType : MultipleChoiceType -> UUID -> FormFields FormField -> FormFields FormField
setMultipleChoiceType multiType optionId formFields =
    case formFields of
        HasFocus formFieldZipper ->
            HasFocus <|
                Zipper.mapCurrent
                    (\f ->
                        { f
                            | questionType =
                                case f.questionType of
                                    MultipleChoice _ options ->
                                        MultipleChoice multiType options

                                    Open _ ->
                                        MultipleChoice multiType (init ( optionId, "first option" ) [])
                        }
                    )
                    formFieldZipper

        NoFocus _ _ ->
            formFields
