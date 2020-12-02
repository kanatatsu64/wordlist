{-# LANGUAGE RankNTypes #-}

module Plugins.German.Noun (
    parse,

    Genre (..),
    isMale,
    isFemale,
    isNeuter,
    parseAttrs
) where

import Prelude hiding ( word )

import Serializable ( Serializable (..), Serial (..) )
import Plugins.German.Utils ( parseWord, parseMeaning, parseNote, parseExamples, (>:>) )

data Genre = Male | Female | Neuter

instance Serializable Genre where
    serialize Male = "M."
    serialize Female = "F."
    serialize Neuter = "N."

parse = parseWord >:>
        parseAttrs >:>
        parseMeaning >:>
        parseNote >:>
        parseExamples

isMale :: String -> Bool
isMale "M." = True
isMale "m." = True
isMale "Male" = True
isMale "male" = True

isMale "Maskulinum" = True
isMale "maskulinum" = True

isMale _ = False

isFemale :: String -> Bool
isFemale "F." = True
isFemale "f." = True
isFemale "Female" = True
isFemale "female" = True

isFemale "Femininum" = True
isFemale "femininum" = True

isFemale _ = False

isNeuter :: String -> Bool
isNeuter "N." = True
isNeuter "n." = True
isNeuter "Neuter" = True
isNeuter "neuter" = True

isNeuter "Neutrum" = True
isNeuter "neutrum" = True

isNeuter _ = False

parseAttrs (pl:gen:rests) cons = Just $ \next -> case () of
    _ | isMale gen -> next rests (cons [Serial pl, Serial Male])
      | isFemale gen -> next rests (cons [Serial pl, Serial Female])
      | isNeuter gen -> next rests (cons [Serial pl, Serial Neuter])
parseAttrs _ _ = Nothing
