-- hello.hs

import Html

main :: IO ()
main =
  putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "<title></title>"
    ( append_
        (h1_ "<h1></h1>")
        ( append_
            (p_ "Paragraph #1")
            (p_ "Paragraph #2")
        )
    )