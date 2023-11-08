-- Html/Internal.hs

module Html.Internal where

-- \* Types

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( element_
        "html"
        ( element_ "head" (element_ "title" (escape title))
            <> element_ "body" (getStructureString content)
        )
    )

h1_ :: String -> Structure
h1_ =
  Structure . element_ "h1" . escape

p_ :: String -> Structure
p_ =
  Structure . element_ "p" . escape

element_ :: String -> String -> String
element_ tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39"
          _ -> [c]
   in concatMap escapeChar

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) =
  Structure
    (a <> b)

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

render :: Html -> String
render html =
  case html of
    Html str -> str