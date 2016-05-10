-- escape.hs
-- $ echo '"&<>' | runghc escape.hs
-- &quot;&amp;&lt;&gt;

-- 문자열을 HTML이스케이프한다.
escape :: String -> String
escape str = str >>= escapeAmp >>= escapeOther where
    escapeAmp  '&' = "&amp;"
    escapeAmp   c  = [c]
    escapeOther '<' = "&lt;"
    escapeOther '>' = "&gt;"
    escapeOther '"' = "&quot;"
    escapeOther  c  = [c]

main :: IO ()
main = do
  -- 표준 입력으로부터 1행을 읽는다.
  rawString <- getLine
  -- HTML이스케이프한 문자열로 변환한다.
  let escapedString = escape rawString
  -- 표준 출력으로 표시한다.
  putStrLn escapedString
