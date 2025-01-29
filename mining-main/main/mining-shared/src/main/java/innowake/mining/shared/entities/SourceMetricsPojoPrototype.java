/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;

/**
 * {@code source_metrics} entity request class, which holds the calculated source metrics of a {@code module} entity.
 */
public class SourceMetricsPojoPrototype implements PojoPrototype {

	public final Definable<EntityId> module = new Definable<>(false, "SourceMetrics.module");
	public final Definable<Integer> physicalLines = new Definable<>(true, "SourceMetrics.physicalLines");
	public final Definable<Integer> codeLines = new Definable<>(true, "SourceMetrics.codeLines");
	public final Definable<Integer> commentLines = new Definable<>(true, "SourceMetrics.commentLines");
	public final Definable<Integer> complexityMcCabe = new Definable<>(true, "SourceMetrics.complexityMcCabe");
	public final Definable<Integer> deadCodeLines = new Definable<>(true, "SourceMetrics.deadCodeLines");

	@JsonAlias("moduleId")
	public SourceMetricsPojoPrototype setModule(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}

	public SourceMetricsPojoPrototype setPhysicalLines(final Integer physicalLines) {
		this.physicalLines.set(positiveIntegerOrNull(physicalLines));
		return this;
	}

	public SourceMetricsPojoPrototype setCodeLines(final Integer codeLines) {
		this.codeLines.set(positiveIntegerOrNull(codeLines));
		return this;
	}
	
	@Nullable 
	private Integer positiveIntegerOrNull(@Nullable final Integer value) {
		return value == null || value.intValue() < 0 ? null : value;
	}

	public SourceMetricsPojoPrototype setCommentLines(final Integer commentLines) {
		this.commentLines.set(positiveIntegerOrNull(commentLines));
		return this;
	}
	
	public SourceMetricsPojoPrototype setComplexityMcCabe(final Integer complexityMcCabe) {
		this.complexityMcCabe.set(positiveIntegerOrNull(complexityMcCabe));
		return this;
	}
	
	public SourceMetricsPojoPrototype setDeadCodeLines(@Nullable final Integer deadCodeLines) {
		final int fixedDeadCodeLines = deadCodeLines == null || deadCodeLines < 0 ? -1 : deadCodeLines;
		this.deadCodeLines.set(fixedDeadCodeLines);
		return this;
	}
}
