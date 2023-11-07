-- hello.hs

main :: IO ()
main =
  putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( append_
        (h1_ "Heading")
        ( append_
            (p_ "Paragraph #1")
            (p_ "Paragraph #2")
        )
    )

newtype Html = Html String

newtype Structure = Structure String

type Title = String

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( element_
        "html"
        ( element_ "head" (element_ "title" title)
            <> element_ "body" (getStructureString content)
        )
    )

h1_ :: String -> Structure
h1_ =
  Structure . element_ "h1"

p_ :: String -> Structure
p_ =
  Structure . element_ "p"

element_ :: String -> String -> String
element_ tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

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