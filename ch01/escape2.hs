-- escape2.hs
-- $ echo '"&<>' | runghc escape2.hs
-- &quot;&amp;&lt;&gt;
import HTMLEscapedString -- 이부분을 교체

main :: IO ()
main = do
  -- 표준 입력으로부터 1행을 읽는다.
  rawString <- getLine
  -- HTML이스케이프한 문자열로 변환한다.
  let escapedString = escape rawString
  -- 표준 출력으로 표시한다.
  putHTMLEscapedStrLn escapedString
