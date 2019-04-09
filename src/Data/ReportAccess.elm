module Data.ReportAccess exposing
    ( ReportAccess(..)
    , reportAccessFromString
    , reportAccessPrettyPrint
    , reportAccessToString
    )


type ReportAccess
    = FullAccess
    | NoAccess


reportAccessToString : ReportAccess -> String
reportAccessToString ra =
    case ra of
        FullAccess ->
            "full_access"

        NoAccess ->
            "no_access"


reportAccessPrettyPrint : ReportAccess -> String
reportAccessPrettyPrint ra =
    case ra of
        FullAccess ->
            "Полный доступ"

        NoAccess ->
            "Ограниченный доступ"


reportAccessFromString : String -> Maybe ReportAccess
reportAccessFromString ra =
    case ra of
        "full_access" ->
            Just FullAccess

        "no_access" ->
            Just NoAccess

        _ ->
            Nothing
