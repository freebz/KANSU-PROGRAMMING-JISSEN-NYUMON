// TestVisitor.java
// $ javac TestVisitor.java
// $ java TestVisitor
// 1 + (2 + 3)^2
// 26

// Visitor
interface Visitor< N, R > {
    // 덧셈 식을 위한 메소드
    R plus(Expr< N > e1, Expr< N > e2);
    // 제곱 식을 위한 메소드
    R square(Expr< N > e);
    // 숫자를 위한 메소드
    R number(N e);
}

// 식의 인터페이스
interface Expr< N > {
    < R > R accept(Visitor< N, R > v);
}

// 덧셈 식
class Plus< N > implements Expr< N > {
    Expr< N > e1;
    Expr< N > e2;
    Plus(Expr< N > e1, Expr< N > e2) { this.e1 = e1; this.e2 = e2; }
    public < R > R accept(Visitor< N, R > v) { return v.plus(e1, e2); }
}

// 제곱 식
class Square< N > implements Expr< N > {
    Expr< N > e;
    Square(Expr< N > e) { this.e = e; }
    public < R > R accept(Visitor< N, R > v) { return v.square(e); }
}

// 숫자 식
class Number< N > implements Expr< N > {
    N n;
    Number(N n) { this.n = n; }
    public < R > R accept(Visitor< N, R > v) { return v.number(n); }
}

// 식의 평가를 실시하는 Visitor
class Eval implements Visitor< Integer, Integer > {
    public Integer plus(Expr< Integer > e1, Expr< Integer > e2) {
	return e1.accept(this) + e2.accept(this);
    }
    public Integer square(Expr< Integer > e) {
	Integer x = e.accept(this);
	return x * x;
    }
    public Integer number(Integer n) {
	return n;
    }
}

// 식을 문자열로 하는 Visitor
class Show implements Visitor< Integer, String > {
    public String plus(Expr< Integer > e1, Expr< Integer > e2) {
	return e1.accept(this) + " + " + e2.accept(this);
    }
    public String square(Expr< Integer > e) {
	return "(" + e.accept(this) + ")^2";
    }
    public String number(Integer n) {
	return n + "";
    }
}

public class TestVisitor {
    public static void main(String[] args) {
	// e = 1 + (2 + 3)^2
	// 실제로는 구문 해석 등에 의해서 좀 더 크고 복잡한 것을 가정
	Expr e = new Plus(new Number(1),
			  new Square(new Plus(new Number(2),
					      new Number(3))));
	System.out.println(e.accept(new Show()));
	System.out.println(e.accept(new Eval()));
    }
}
