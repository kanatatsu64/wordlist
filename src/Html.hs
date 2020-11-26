module Html (
    Htmlizable (..),
    TagName (..),
    Content (..),
    Attr (..),
    Html (..),
    template,
    export,

    exportHtml,
    exportAttr,
    exportContent,
    cons
) where

class Htmlizable a where
    toHtml :: a -> Html

instance Htmlizable Html where
    toHtml = id

data TagName = HTML | HEAD | BODY | SPAN |
               TABLE | CAPTION | TD | TR

instance Show TagName where
    show HTML = "html"
    show HEAD = "head"
    show BODY = "body"
    show SPAN = "span"
    show TABLE = "table"
    show CAPTION = "caption"
    show TD = "td"
    show TR = "tr"

data Content = Text String | Child Html
type Attr = String

data Html = Tag {
    name :: TagName, 
    attributes :: [Attr],
    contents :: [Content]
}

{-
    template $ Tag SPAN [] [ Text "sample" ]
    <html>
        <head></head>
        <body>
            <span>sample</span>
        </body>
    </html>
-}

template :: Html -> Html
template body = Tag HTML [] [
        Child $ Tag HEAD [] [],
        Child $ Tag BODY [] [ Child body ]
    ]

export :: Html -> String
export = exportHtml

exportHtml :: Html -> String
exportHtml html = "<" ++ _name ++ _attributes ++ ">" ++ _contents ++ "</" ++ _name ++ ">"
    where _name = show (name html)
          _attributes = case attributes html of
              [] -> ""
              _ -> " " ++ cons " " (map exportAttr (attributes html))
          _contents = cons "" $ map exportContent (contents html)

exportAttr :: Attr -> String
exportAttr = id

exportContent :: Content -> String
exportContent (Text str) = str
exportContent (Child html) = exportHtml html

cons :: String -> [String] -> String
cons delim (l:ls)= foldl (\l r -> l ++ delim ++ r) l ls
cons _ [] = ""
