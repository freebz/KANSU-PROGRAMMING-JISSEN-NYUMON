// TestOptional.java
// $ javac TEstOptional.java
// $ echo "1 + 1" | java TestOptional
// 2
// $ echo "2 - 1" | java TestOptional
// 1
// $ echo "3 * 3" | java TestOptional
// 9
// $ echo "4 / 2" | java TestOptional
// 2
// $ echo "4 / 0" | java TestOptional
// invalid

import java.util.Optional;
import java.util.function.Function;
import java.util.function.BiFunction;
import java.io.*;

public class TestOptional {
    // 공백으로 분할하기, 세 개로 분할할 수 없다면 무효.
    // "1 + 2" -> "1", "+", "2"
    private static Optional< String[] > words(String expr) {
	String[] result = expr.split(" ");
	return 3 == result.length ? Optional.of(result) : Optional.empty();
    }

    // 문자열을 정수로 변환. 변환할 수 없다면 무효
    private static Optional< Integer > toNum(String s) {
	try { return Optional.of(Integer.parseInt(s)); }
	catch (NumberFormatException ex) { return Optional.empty(); }
    }

    // 사칙 연산. 연산할 수 없으면 무효
    private static Optional< Integer > add(Integer a, Integer b) {
	return Optional.of(a + b);
    }
    private static Optional< Integer > sub(Integer a, Integer b) {
	return Optional.of(a - b);
    }
    private static Optional< Integer > mul(Integer a, Integer b) {
	return Optional.of(a * b);
    }
    private static Optional< Integer > div(Integer a, Integer b) {
	return 0 != b ? Optional.of(a / b) : Optional.empty();
    }

    // "+","-","*","/" 중 하나의 문자열을 연산으로 변환. 그 외는 무효
    private static Optional< BiFunction< Integer, Integer, Optional< Integer >>> toBinOp(String s) {
	return
	    s.equals("+") ? Optional.of(TestOptional::add) :
	    s.equals("-") ? Optional.of(TestOptional::sub) :
	    s.equals("*") ? Optional.of(TestOptional::mul) :
	    s.equals("/") ? Optional.of(TestOptional::div) :
	    Optional.empty();
    }

    public static void main(String[] args) throws Exception {
	String expr = new BufferedReader(new InputStreamReader(System.in)).readLine();
	System.out.println(words(expr)
			   .flatMap(ss -> toNum(ss[0])
				    .flatMap(a -> toBinOp(ss[1])
					     .flatMap(op -> toNum(ss[2])
						      .flatMap(b ->
							       op.apply(a,b)))
					     )
				    )
			   .map(n -> "" + n)
			   .orElseGet(() -> "invalid")
			   );
    }
}

