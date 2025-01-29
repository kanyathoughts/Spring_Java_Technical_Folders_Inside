package innowake.mining.server.discovery.metrics.generic;

import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Exception dealing with errors during metric execution.  
 */
public class MetricException extends Exception {
	
	private static final long serialVersionUID = 1L;
	
	/**
	 * Initializes a Metric Exception.
	 */
	public MetricException() {
	}
	
	/**
	 * Initializes a Metric Exception.
	 * 
	 * @param msg the message to be printed along with the exception
	 */
	public MetricException(final String msg) {
		super(msg);
	}
	
	/**
	 * Initializes a Metric Exception.
	 * 
	 * @param cause the cause of the exception
	 */
	public MetricException(final Throwable cause) {
		super(cause);
	}

	
	/**
	 * Special exception to handle unparseable code (no root node available in {@link AstModel}). 
	 */
	public static class AstRootNotAvailableException extends MetricException {
		private static final long serialVersionUID = 1L;
	}
	
}
