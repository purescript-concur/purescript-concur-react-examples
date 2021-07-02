{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-concur-react-examples"
, dependencies =
  [ "aff"
  , "arrays"
  , "argonaut-core"
  , "argonaut-codecs"
  , "affjax"
  , "avar"
  , "concur-core"
  , "concur-react"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "react"
  , "routing"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, sources = [ "src/**/*.purs" ]
, packages = ../packages.dhall
}
