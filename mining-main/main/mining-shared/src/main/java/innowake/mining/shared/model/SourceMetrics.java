/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Objects;
import java.util.UUID;

import com.google.common.base.MoreObjects;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;

/**
 * Represents a LOC metric and complexity of a model artifact
 */
public class SourceMetrics implements AdditionalInfo {

	@Nullable
	private UUID moduleId;
	@Nullable
	private Integer physicalLines;
	@Nullable
	private Integer codeLines;
	@Nullable
	private Integer commentLines;
	@Nullable
	private Integer complexityMcCabe;
	private Integer deadCodeLines = Integer.valueOf(-1);
	
	private boolean modified;

	/**
	 * Creates a new instance of {@link SourceMetrics}.
	 */
	public SourceMetrics() {}
	
	/**
	 * Creates a new instance of {@link SourceMetrics}.
	 * 
	 * @param physicalLines total lines count (including code lines, empty lines and comment lines)
	 * @param codeLines Number of lines containing code (no empty lines, no comment lines)
	 * @param commentLines Number of lines containing a comment
	 * @param complexityMcCabe complexity of the given source code
	 * @param deadCodeLines Number of lines containing code, that is never executed
	 */
	public SourceMetrics(@Nullable final Integer physicalLines, @Nullable final Integer codeLines, @Nullable final Integer commentLines,
			@Nullable final Integer complexityMcCabe, final Integer deadCodeLines) {
		this.physicalLines = physicalLines;
		this.codeLines = codeLines;
		this.commentLines = commentLines;
		this.complexityMcCabe = complexityMcCabe;
		this.deadCodeLines = deadCodeLines;
		modified = true;
	}

	/**
	 * Get the total line count i.e, PLOC (including code lines, empty lines and comment lines).
	 *
	 * @return the total number of lines of code
	 */
	@Nullable
	public Integer getPhysicalLines() {
		return physicalLines;
	}
	
	/**
	 * Set total line count i.e, PLOC (including code lines, empty lines and comment lines).
	 * Must be > -1.
	 *
	 * @param physicalLines the total number of lines of code
	 * @return the {@link SourceMetrics} instance
	 */
	public SourceMetrics setPhysicalLines(@Nullable final Integer physicalLines) {
		final var newValue = positiveIntegerOrNull(physicalLines);
		if ( ! Objects.equals(this.physicalLines, newValue)) {
			this.physicalLines = newValue;
			modified = true;
		}
		return this;
	}
	
	/**
	 * Get the number of lines of code i.e, SLOC.
	 * If a line contain both, code and comment, it will be added to code.
	 *
	 * @return the number of lines with code
	 */
	@Nullable
	public Integer getCodeLines() {
		return codeLines;
	}
	
	/**
	 * Set the number of lines of code i.e, SLOC.
	 * Must be > -1.
	 *
	 * @param codeLines The number of lines of code
	 * @return the {@link SourceMetrics} instance
	 */
	public SourceMetrics setCodeLines(@Nullable final Integer codeLines) {
		final var newValue = positiveIntegerOrNull(codeLines);
		if ( ! Objects.equals(this.codeLines, newValue)) {
			this.codeLines = newValue;
			modified = true;
		}
		return this;
	}
	
	/**
	 * Get the number of lines of comment i.e, CLOC.
	 * If a line contain both, code and comment, it will be added to code.
	 *
	 * @return the number of lines with comment
	 */
	@Nullable
	public Integer getCommentLines() {
		return commentLines;
	}
	
	/**
	 * Set the number of lines of comment i.e, CLOC.
	 * Must be > -1.
	 *
	 * @param commentLines The number of lines of comment
	 * @return the {@link SourceMetrics} instance
	 */
	public SourceMetrics setCommentLines(@Nullable final Integer commentLines) {
		final var newValue = positiveIntegerOrNull(commentLines);
		if ( ! Objects.equals(this.commentLines, newValue)) {
			this.commentLines = newValue;
			modified = true;
		}
		return this;
	}
	
	/**
	 * Get the complexity value by McCabe.
	 *
	 * @return the complexity
	 */
	@Nullable
	public Integer getComplexityMcCabe() {
		return complexityMcCabe;
	}
	
	/**
	 * Set the module complexity calculated by McCabe.
	 * Must be > -1.
	 *
	 * @param complexityMcCabe the complexity value
	 * @return the {@link SourceMetrics} instance
	 */
	public SourceMetrics setComplexityMcCabe(@Nullable final Integer complexityMcCabe) {
		final var newValue = positiveIntegerOrNull(complexityMcCabe);
		if ( ! Objects.equals(this.complexityMcCabe, newValue)) {
			this.complexityMcCabe = newValue;
			modified = true;
		}
		return this;
	}
	
	/**
	 * Returns the number of code lines, that is never executed.
	 *
	 * @return number of dead code lines
	 */
	public Integer getDeadCodeLines() {
		return deadCodeLines;
	}

	/**
	 * Sets the number of code lines, that is never executed.
	 *
	 * @param deadCodeLines number of dead code lines 
	 * @return the {@link SourceMetrics} instance
	 */
	public SourceMetrics setDeadCodeLines(@Nullable final Integer deadCodeLines) {
		/* Although the default is 0, it can be -1 for virtual modules */
		final var newValue = deadCodeLines == null || deadCodeLines.intValue() < -1 ? Integer.valueOf(-1) : deadCodeLines;
		if ( ! Objects.equals(this.deadCodeLines, newValue)) {
			this.deadCodeLines = newValue;
			modified = true;
		}
		return this;
	}

	/**
	 * Sets the {@link UUID} of the module for which this source metrics got created.
	 *
	 * @param moduleId the module {@link UUID}
	 * @return this instance
	 */
	public SourceMetrics setModuleId(final UUID moduleId) {
		if ( ! Objects.equals(this.moduleId, moduleId)) {
			this.moduleId = moduleId;
			modified = true;
		}
		return this;
	}

	/**
	 * @return the {@link UUID} of the module for which this source metrics got created
	 */
	@Nullable
	public UUID getModuleId() {
		return moduleId;
	}

	@Nullable
	private static Integer positiveIntegerOrNull(@Nullable final Integer value) {
		return value == null || value.intValue() < 0 ? null : value;
	}
	
	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.add("moduleId", moduleId)
				.add("physicalLines", physicalLines)
				.add("codeLines", codeLines)
				.add("commentLines", commentLines)
				.add("deadCodeLines", deadCodeLines)
				.add("complexityMcCabe", complexityMcCabe)
				.toString();
	}

	@Override
	public int hashCode() {
		return 31 * super.hashCode() + Objects.hash(codeLines, commentLines, complexityMcCabe, physicalLines, deadCodeLines);
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}	
		if (obj == null || getClass() != obj.getClass() || ! super.equals(obj)) {
			return false;
		}

		final SourceMetrics other = (SourceMetrics) obj;
		return Objects.equals(codeLines, other.codeLines)
				&& Objects.equals(commentLines, other.commentLines)
				&& Objects.equals(complexityMcCabe, other.complexityMcCabe)
				&& Objects.equals(physicalLines, other.physicalLines)
				&& Objects.equals(deadCodeLines, other.deadCodeLines);
	}

	/**
	 * @return {@code true} if this {@link SourceMetrics} was modified. Otherwise {@code false}. 
	 */
	public boolean isModified() {
		return modified;
	}

	/**
	 * Sets the modified state of this {@link SourceMetrics} to the given {@code modified} value.
	 *
	 * @param modified the modified value
	 * @return this instance
	 */
	public SourceMetrics setModified(final boolean modified) {
		this.modified = modified;
		return this;
	}

	/**
	 * @return a new {@link SourceMetricsPojoPrototype} instance containing all non {@code null} values of this {@link SourceMetrics}.
	 */
	public SourceMetricsPojoPrototype convertToPojoPrototype() {
		final var prototype = new SourceMetricsPojoPrototype();

		if (moduleId != null) {
			prototype.setModule(EntityId.of(moduleId));
		}

		if (codeLines != null) {
			prototype.setCodeLines(codeLines);
		}

		if (commentLines != null) {
			prototype.setCommentLines(commentLines);
		}

		if (complexityMcCabe != null) {
			prototype.setComplexityMcCabe(complexityMcCabe);
		}

		prototype.setDeadCodeLines(deadCodeLines);

		if (physicalLines != null) {
			prototype.setPhysicalLines(physicalLines);
		}

		return prototype;
	}
}
