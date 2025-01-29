package innowake.mining.moduleextraction.base;
import java.util.function.Function;

public class RuleSet<T> {
	private Function<T, T> composedFunction = null;
	
	protected RuleSet(@SuppressWarnings("unchecked") Function<T, T>... rules) {
		for (Function<T, T> func : rules) {
			composedFunction = 
					composedFunction == null ? 
					func : composedFunction.andThen(func);
		}
	}

	public static <U> RuleSet<U> of(@SuppressWarnings("unchecked") Function<U, U>... rules) {
		return new RuleSet<U>(rules);
	}
	
	@SuppressWarnings("unchecked")
	public static <U> RuleSet<U> compose(RuleSet<U> ruleSet1, RuleSet<U> ruleSet2) {
		return new RuleSet<U>(ruleSet1.composedFunction.andThen(ruleSet2.composedFunction));
	}
	
	public T apply(T input) {
		return composedFunction.apply(input);
	}
}
