package interpreter;

public class Demo {

    public static void main(String[] args) {
        TerminalExpression t1 = new TerminalExpression("Java");
        TerminalExpression t2 = new TerminalExpression("Design Patterns");
        TerminalExpression t3 = new TerminalExpression("Tough");

        OrExpression or1 = new OrExpression(t1, t2);
        System.out.println(or1.interpret("Java Programming")); // true (t1 || t2)

        AndExpression and1 = new AndExpression(t1, t2);
        System.out.println(and1.interpret("Java Design Patterns")); // true (t1 && t2)

        AndExpression and2 = new AndExpression(t1, new AndExpression(t2, t3));
        System.out.println(and2.interpret("Java Design Patterns moderate")); // false (t1 && (t2 && t3))

    }

}
