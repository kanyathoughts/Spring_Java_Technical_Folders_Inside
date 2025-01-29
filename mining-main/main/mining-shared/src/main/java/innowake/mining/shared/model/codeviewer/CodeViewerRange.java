/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.codeviewer;

import java.util.Objects;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * A range in the code viewer, expressed in line numbers and columns. This is for compatibility with Monaco editor.
 * As such <strong>the line numbers and columns are 1-based</strong> because that is how Monaco counts them.
 */
public class CodeViewerRange {

    private final int startLineNumber;
    private final int startColumn;
    private final int endLineNumber;
    private final int endColumn;

    @JsonCreator
    public CodeViewerRange(@JsonProperty("startLineNumber") final int startLineNumber,
                           @JsonProperty("startColumn") final int startColumn,
                           @JsonProperty("endLineNumber") final int endLineNumber,
                           @JsonProperty("endColumn") final int endColumn) {
        this.startLineNumber = startLineNumber;
        this.startColumn = startColumn;
        this.endLineNumber = endLineNumber;
        this.endColumn = endColumn;
    }

    /**
     * Returns the line number where the range starts (first line is 1).
     * @return the start line number
     */
    public int getStartLineNumber() {
        return startLineNumber;
    }

    /**
     * Returns the column where the range starts (first column is 1).
     * @return the start column
     */
    public int getStartColumn() {
        return startColumn;
    }

    /**
     * Returns the line number where the range ends (first line is 1).
     * @return the end line number
     */
    public int getEndLineNumber() {
        return endLineNumber;
    }

    /**
     * Returns the column where the range ends (first column is 1).
     * @return the end column
     */
    public int getEndColumn() {
        return endColumn;
    }

	@Override
	public int hashCode() {
		return Objects.hash(endColumn, endLineNumber, startColumn, startLineNumber);
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final CodeViewerRange other = (CodeViewerRange) obj;
		return endColumn == other.endColumn && endLineNumber == other.endLineNumber && startColumn == other.startColumn
				&& startLineNumber == other.startLineNumber;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
				.append("startLineNumber", startLineNumber)
				.append("startColumn", startColumn)
				.append("endLineNumber", endLineNumber)
				.append("endColumn", endColumn)
			.toString();
	}
}
