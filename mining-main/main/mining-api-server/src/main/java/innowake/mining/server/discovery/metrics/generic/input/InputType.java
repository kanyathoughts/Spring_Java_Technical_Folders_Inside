package innowake.mining.server.discovery.metrics.generic.input;

import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.ast.AstModel;

public class InputType<T> {
	
	public static final InputType<AstModel> AST_MODEL = new InputType<>(AstModel.class);
	public static final InputType<String> SOURCE = new InputType<>(String.class);
	public static final InputType<ITokenPartitioning> TOKEN = new InputType<>(ITokenPartitioning.class);
	
	private final Class<T> typeClass;
	
	private InputType(final Class<T> typeClass) {
		this.typeClass = typeClass;
	}
	
	/**
	 * Returns the type class of the given input type
	 *
	 * @return type class of the given input type
	 */
	public Class<T> getTypeClass() {
		return typeClass;
	}
	
	/**
	 * Class representing the AstInputType.
	 */
	public static class AstInput extends InputType<AstModel> {
		
		private AstInput() {
			super(AstModel.class);
		}
		
	}
}
