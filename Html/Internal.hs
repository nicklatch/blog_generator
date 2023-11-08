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

code_ :: String -> Structure
code_ =
  Structure . element_ "pre" . escape

ul_ :: [Structure] -> Structure
ul_ =
  Structure . element_ "ul" . concatMap (element_ "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
  Structure . element_ "ol" . concatMap (element_ "li" . getStructureString)

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) =
  Structure
    (a <> b)

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utils

element_ :: String -> String -> String
element_ tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

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
