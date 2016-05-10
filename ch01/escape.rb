# escape.rb
# $ echo '"&<>' | ruby escape.rb
# &quot;&amp;&lt;&gt;
require 'cgi'

# 표준 입력으로부터 1행 읽는다.
raw_string = gets.chomp!
# HTML이스케이프한 문자열로 변환한다.
escaped_string = CGI.escapeHTML raw_string
# 표준 출력으로 표시한다.
puts escaped_string
