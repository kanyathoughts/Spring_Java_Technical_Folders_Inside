/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.UUID;

import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * {@code source_metrics} entity class, which holds the calculated source metrics of a {@code module} entity.
 */
@MiningDataType(name = MiningEnitityNames.SOURCE_METRICS)
public final class SourceMetricsPojo {

	private final EntityId module;
	@MiningDataPoint(displayName = "Physical Lines of Code", description = "Number of lines of code in source file including code lines, empty lines and comment lines")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Metrics")
	})
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	@Usage(Usages.SORT_BY)
	@Nullable private final Integer physicalLines;
	@MiningDataPoint(displayName = "Source Lines of Code", description = "Number of source code lines")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Metrics")
	})
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	@Usage(value = Usages.METRICS_CHART_DETAILS_MODULE)
	@Usage(Usages.SORT_BY)
	@Nullable private final Integer codeLines;
	@MiningDataPoint(displayName = "Comment Lines of Code", description = "Number of lines containing a comment")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Metrics")
	})
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	@Usage(value = Usages.METRICS_CHART_DETAILS_MODULE)
	@Usage(Usages.SORT_BY)
	@Nullable private final Integer commentLines;
	@MiningDataPoint(displayName = "Program Complexity", description = "Cyclomatic Complexity of the Module")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Metrics")
	})
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	@Usage(Usages.SORT_BY)
	@Nullable private final Integer complexityMcCabe;
	@MiningDataPoint(displayName = "Lines of Dead Code", description = "Number of lines containing dead code")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Metrics")
	})
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	@Usage(Usages.SORT_BY)
	@Nullable private final Integer deadCodeLines;

	@JsonCreator
	public SourceMetricsPojo(
			@JsonProperty("moduleEntity") @Nullable final EntityId module,
			@JsonProperty("module") @Nullable final UUID moduleUid,
			@JsonProperty("moduleId") @Nullable final Long moduleNid,
			@JsonProperty("physicalLines") @Nullable final Integer physicalLines,
			@JsonProperty("codeLines") @Nullable final Integer codeLines,
			@JsonProperty("commentLines") @Nullable final Integer commentLines,
			@JsonProperty("complexityMcCabe") @Nullable final Integer complexityMcCabe,
			@JsonProperty("deadCodeLines") @Nullable final Integer deadCodeLines) {
		this.module = module != null ? module : EntityId.of(moduleUid, moduleNid);
		this.physicalLines = physicalLines;
		this.codeLines = codeLines;
		this.commentLines = commentLines;
		this.complexityMcCabe = complexityMcCabe;
		this.deadCodeLines = deadCodeLines;
	}

	/**
	 * @return the {@link EntityId} of the module.
	 */
	@JsonIgnore
	public EntityId getModule() {
		return module;
	}

	/**
	 * @return the {@link UUID} of the module.
	 */
	@JsonProperty("module")
	public UUID getModuleUid() {
		return module.getUid();
	}

	/**
	 * @return the numeric id of the module.
	 */
	@JsonProperty("moduleId")
	public Long getModuleNid() {
		return module.getNid();
	}

	/**
	 * @return the number of physical lines if available
	 */
	@Nullable
	public Integer getPhysicalLines() {
		return physicalLines;
	}

	/**
	 * @return the number of code lines if available
	 */
	@Nullable
	public Integer getCodeLines() {
		return codeLines;
	}

	/**
	 * @return the number of comment lines if available
	 */
	@Nullable
	public Integer getCommentLines() {
		return commentLines;
	}

	/**
	 * @return the cyclomatic complexity (McCabe) if available
	 */
	@Nullable
	public Integer getComplexityMcCabe() {
		return complexityMcCabe;
	}

	/**
	 * @return the number of dead code lines if available
	 */
	@Nullable
	public Integer getDeadCodeLines() {
		return deadCodeLines;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("physicalLines", physicalLines)
				.append("codeLines", codeLines)
				.append("commentLines", commentLines)
				.append("deadCodeLines", deadCodeLines)
				.toString();
	}

	/**
	 * @return a new {@link SourceMetricsPojoPrototype} instance containing all non {@code null} values of this {@link SourceMetricsPojo}.
	 */
	public SourceMetricsPojoPrototype convertToPojoPrototype() {
		final var prototype = new SourceMetricsPojoPrototype();

		if (codeLines != null) {
			prototype.setCodeLines(codeLines);
		}

		if (commentLines != null) {
			prototype.setCommentLines(commentLines);
		}

		if (complexityMcCabe != null) {
			prototype.setComplexityMcCabe(complexityMcCabe);
		}

		if (deadCodeLines != null) {
			prototype.setDeadCodeLines(deadCodeLines);
		}

		if (physicalLines != null) {
			prototype.setPhysicalLines(physicalLines);
		}

		return prototype;
	}
}
