-- EscapedString.hs
{-# LANGUAGE FlexibleInstances #-}
module EscapedString
    ( EscapedString
    , fromString, toString
    ) where

import Data.List ( isPrefixOf )

-- | 이스케이프되어 있는 문자열
newtype EscapedString x = EscapedString { unEscapedString :: String } deriving Eq

-- | 이스케이프되어 있는 문자열의 Show인스턴스화
instance Show (EscapedString x) where
    show = show . unEscapedString

-- | 보통의 문자열을 이스케이프되어 있는 문자열로 한다.
fromString :: String -> EscapedString String
fromString = EscapedString

-- | 이스케이프되어 있는 문자열을 보통의 문자열로 한다.
toString :: EscapedString String -> String
toString = unEscapedString

-- | 이스케이프되어 있는 문자열과 같은 것
class EscapedStringLike s

-- | 순수 문자열은 이스케이프되어 있지 않지만
-- 이스케이프되어 있는 문자열 같은 것

-- (이스케이프를 위한 변환으로서 항등 변환이 걸려 있다고 생각)
instance EscapedStringLike String

-- | 이스케이프 방법
class EscapeMethod m where
    -- | 변환
    escape :: EscapedStringLike s => EscapedString s -> EscapedString (m s)
    -- | 역변환
    unescape :: EscapedStringLike s => EscapedString (m s) -> EscapedString s

-- | HTML이스케이프
data HTMLEscape s

-- | 이스케이프되어 있는 문자열과 같은 것을,
-- 재차 HTML이스케이프한 것. 또는 이스케이프되어 있는 문자열과 같은 것
instance EscapedStringLike s => EscapedStringLike (HTMLEscape s)

-- | 이스케이프 방법(HTML이스케이프)
instance EscapeMethod HTMLEscape where
    escape = EscapedString . escape' . unEscapedString where
        escape' :: String -> String
        escape' str = str >>= escapeAmp >>= escapeOther where
            escapeAmp '&'   = "&amp;"
            escapeAmp c     = [c]
            escapeOther '<' = "&lt;"
            escapeOther '>' = "&gt;"
            escapeOther '"' = "&quot;"
            escapeOther c   = [c]
    unescape = EscapedString . unescape' . unEscapedString where
        unescape' :: String -> String
        unescape' = foldr (\c s -> unescapePrefix (c:s)) "" where
            unescapePrefix str
                | "&quot;" `isPrefixOf` str = '"':drop 6 str
                | "&gt;"   `isPrefixOf` str = '>':drop 4 str
                | "&lt;"   `isPrefixOf` str = '<':drop 4 str
                | "&amp;"  `isPrefixOf` str = '&':drop 5 str
                | otherwise                 = str

-- | 문자열 이스케이프
data StringEscape s
-- | 이스케이프되어 있는 문자열과 같은 것을,
-- 재차 문자열 이스케이프한 것. 또는 이스케이프되어 있는 문자열과 같은 것
instance EscapedStringLike s => EscapedStringLike (StringEscape s)

-- | 이스케이프 방법(문자열 이스케이프)
instance EscapeMethod StringEscape where
    escape = EscapedString . show . unEscapedString
    unescape = EscapedString . read . unEscapedString
