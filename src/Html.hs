module Html (
    TagName (..),
    Content (..),
    Attr (..),
    Html (..),
    export
) where

data TagName = TD | TR

instance Show TagName where
    show TD = "td"
    show TR = "tr"

data Content = Text String | Child Html
type Attr = String

data Html = Tag {
    name :: TagName, 
    attributes :: [Attr],
    contents :: [Content]
}

addChild :: Html -> Html -> Html
addChild base child = base { contents = (Child child):(contents base) }

addChildren :: Html -> [Html] -> Html
addChildren base children = base { contents = (map Child children) <> (contents base) }

export :: Html -> String
export = exportHtml

exportHtml :: Html -> String
exportHtml html = "<" ++ _name ++ " " ++ _attributes ++ ">" ++ _contents ++ "</" ++ _name ++ ">"
    where _name = show (name html)
          _attributes = cons " " $ map exportAttr (attributes html)
          _contents = cons "" $ map exportContent (contents html)

exportAttr :: Attr -> String
exportAttr = id

exportContent :: Content -> String
exportContent (Text str) = str
exportContent (Child html) = exportHtml html

cons :: String -> [String] -> String
cons delim = foldl (\l r -> l ++ delim ++ r) ""

