package innowake.mining.shared.model;

import java.util.LinkedList;

public class AnnotationImportJobResult {
	private final int totalLines;

	private final LinkedList<String> errors;

	/**
	 * Creates a new AnnotationImportJobResult.
	 *
	 * @param totalLines the total lines that must be imported.
	 */
	public AnnotationImportJobResult(final Integer totalLines) {
		this.totalLines = totalLines;
		this.errors = new LinkedList<>();
	}

	public Integer getTotalLines() {
		return totalLines;
	}

	public LinkedList<String> getErrors() {
		return errors;
	}
}
