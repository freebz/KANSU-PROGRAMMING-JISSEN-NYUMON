-- HTMLEscapedString.hs
module HTMLEscapedString
    ( HTMLEscapedString
    , escape
    , putHTMLEscapedStrLn
    ) where

-- 이스케이프된 문자열의 타입을 새롭게 정의한다.
data HTMLEscapedString = HTMLEscapedString String

-- 문자열을 이스케이프된 문자열로 변환한다.
escape :: String -> HTMLEscapedString
escape str = HTMLEscapedString (str >>= escapeAmp >>= escapeOther) where
    escapeAmp   '&' = "&amp;"
    escapeAmp    c  = [c]
    escapeOther '<' = "&lt;"
    escapeOther '>' = "&gt;"
    escapeOther '"' = "&quot;"
    escapeOther  c  = [c]

-- 이스케이프된 문자열을 사용하는 처리
-- 이번에는 그냥 출력
putHTMLEscapedStrLn :: HTMLEscapedString -> IO ()
putHTMLEscapedStrLn (HTMLEscapedString str) = putStrLn str
