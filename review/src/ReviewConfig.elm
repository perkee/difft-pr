module ReviewConfig exposing (config)

import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoUnused.Variables.rule
    , NoUnused.CustomTypeConstructors.rule []
        |> Review.Rule.ignoreErrorsForFiles [ "src/Types.elm" ]
    , NoUnused.CustomTypeConstructorArgs.rule
        |> Review.Rule.ignoreErrorsForFiles [ "src/Types.elm" ]
    , NoUnused.Dependencies.rule
        |> Review.Rule.ignoreErrorsForFiles [ "elm.json" ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , Simplify.rule Simplify.defaults
    ]
        |> List.map (Review.Rule.ignoreErrorsForDirectories [ "src/Evergreen" ])
