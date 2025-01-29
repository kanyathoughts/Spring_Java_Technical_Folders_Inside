package interpreter;

public class AndExpression implements Expression {
    Expression exp1; // In the final Demo class we are giving this as terminal expression so
                     // exp1.interpret will call
    // the method insdie the TerminalExpression class
    Expression exp2;

    public AndExpression(Expression exp1, Expression exp2) {
        this.exp1 = exp1;
        this.exp2 = exp2;
    }

    @Override
    public boolean interpret(String context) {
        return (exp1.interpret(context) && exp2.interpret(context));
    }

}
