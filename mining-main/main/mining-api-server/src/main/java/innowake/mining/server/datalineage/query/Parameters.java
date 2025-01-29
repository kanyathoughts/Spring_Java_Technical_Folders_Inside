/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.query;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type.DATABASE_TABLE;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo.Type;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.preset.AbstractBuilder;
import innowake.lib.job.api.ProgressMonitor;
import org.apache.commons.lang3.StringUtils;

/**
 * Parameters for {@linkplain DataFlowGraphQueryService#buildDataFlowGraph(ProgressMonitor, Parameters) Data Flow Graph Query}.
 */
public class Parameters implements Serializable {

	private final EntityId projectId;
	private final QueryDirection queryDirection;
	private final DetailLevel detailLevel;
	private final int maxModuleDistance;
	private final int maxRecursionDepth;
	private final List<EntityId> startModuleIds;
	private final List<SelectedField> startFields;
	private final boolean strictTracing;
	private final boolean assembled;

	private final Type containerType;

	private final String selectedTableColumnName;

	private Parameters(final Builder builder) {
		projectId = assertNotNull(builder.projectId);
		queryDirection = builder.queryDirection;
		detailLevel = builder.detailLevel;
		maxModuleDistance = builder.maxModuleDistance;
		startModuleIds = builder.startModuleIds;
		startFields = builder.startFields;
		strictTracing = builder.strictTracing;
		maxRecursionDepth = builder.maxRecursionDepth;
		assembled = builder.assembled;
		containerType = builder.containerType;
		selectedTableColumnName = builder.selectedTableColumnName;
	}

	/**
	 * Gets the project id on which the query is executed.
	 * @return the project id
	 */
	public EntityId getProjectId() {
		return projectId;
	}

	/**
	 * Gets the query direction of the data flow graph query.
	 * @return the query direction
	 */
	public QueryDirection getQueryDirection() {
		return queryDirection;
	}

	/**
	 * Gets the detail level of the data flow graph query.
	 * @return the detail level
	 */
	public DetailLevel getDetailLevel() {
		return detailLevel;
	}

	/**
	 * Gets the maximum distance in Modules from the starting Module that the query will cover.
	 * <p>
	 *     A negative value indicates that there is no limit. A value of 0 means that the
	 *     result will be empty. All other values mean that the query will stop after traversing
	 *     the given number of Modules counting from the start Module.
	 * </p>
	 * @return the maximum module distance
	 */
	public int getMaxModuleDistance() {
		return maxModuleDistance;
	}

	/**
	 * Sets the maximum recursion depth of the query, in other words the maximum number of statements and fields that are traversed.
	 * <p>
	 * A negative value indicates that there is no limit. A value of 0 means that the result will be empty.
	 * All other values mean that the query will stop after traversing the given number of Fields or Statements.
	 *
	 * @return the maximum recursion depth
	 */
	public int getMaxRecursionDepth() {
		return maxRecursionDepth;
	}

	/**
	 * Gets the list of module ids on which to start the query.
	 * <p>
	 *     When a module id is listed here, it indicates that all data fields inside of the Module shall be traced.
	 * </p>
	 * @return the list of start module ids
	 */
	public List<EntityId> getStartModuleIds() {
		return startModuleIds;
	}

	/**
	 * Gets the list of start fields as pairs of module id and field offset.
	 * <p>
	 *     This allows tracing of individual fields by giving the module id plus the offset
	 *     at which the field can be found within the module's source code (in characters).
	 * </p>
	 * @return the list of start fields
	 */
	public List<SelectedField> getStartFields() {
		return startFields;
	}

	/**
	 * Checks for strictTracing enable/disabled
	 *
	 * @return strictTracing is enabled or not
	 */
	public boolean isStrictTracing() {
		return strictTracing;
	}

	/**
	 * Checks if assembled mode is enabled.
	 *
	 * @return if assembled mode is enabled or not
	 */
	public boolean isAssembled() {
		return assembled;
	}

	/**
	 * Gets the selected table column name.
	 *
	 * @return the selected table column name
	 */
	public String getSelectedTableColumnName() {
		return selectedTableColumnName;
	}

	/**
	 * Gets the proxy container type.
	 *
	 * @return the proxy container type
	 */
	public ProxyContainerPojo.Type getContainerType() {
		return containerType;
	}
	/**
	 * Builder for {@link Parameters}.
	 */
	public static class Builder extends AbstractBuilder<Parameters, Builder> {

		@Nullable
		private EntityId projectId;
		private QueryDirection queryDirection = QueryDirection.BOTH;
		private DetailLevel detailLevel = DetailLevel.STATEMENT;
		private int maxModuleDistance = -1;
		private int maxRecursionDepth = -1;
		private List<EntityId> startModuleIds = new ArrayList<>();
		private List<SelectedField> startFields = new ArrayList<>();
		private boolean strictTracing;
		private boolean assembled;
		private ProxyContainerPojo.Type containerType;
		private String selectedTableColumnName;

		@Override
		protected Builder reset() {
			projectId = null;
			queryDirection = QueryDirection.BOTH;
			detailLevel = DetailLevel.STATEMENT;
			maxModuleDistance = -1;
			maxRecursionDepth = -1;
			startModuleIds = new ArrayList<>();
			startFields = new ArrayList<>();
			strictTracing = false;
			assembled = false;
			containerType = DATABASE_TABLE;
			selectedTableColumnName = "";
			return this;
		}

		@Override
		protected Parameters internalBuild() {
			if (projectId == null) {
				throw new IllegalArgumentException("Project id must be set.");
			}
			if (startModuleIds.isEmpty() && startFields.isEmpty()) {
				throw new IllegalArgumentException("At least one start module id or start field is required.");
			}
			return new Parameters(this);
		}

		/**
		 * Populate the builder from a map of query parameters.
		 * <p>
		 * The following parameters can be given (parameters marked as "multi" can be specified multiple times):
		 * <ul>
		 *  <li> "moduleId" (required, multi) id of the module where the start field is located
		 *  <li> "fieldOffset" (optional, multi, see below) offset (in characters) of the start field within its module
		 *  <li> "detailLevel" (optional) either "MODULE", "FIELD" or "STATEMENT". Default: "MODULE"
		 *  <li> "queryDirection" (optional) either "UPSTREAM", "DOWNSTREAM" or "BOTH". Default: "BOTH"
		 *  <li> "maxModuleDistance" (optional) maximum distance for the trace, counting modules from the start module. Passing 0 will produce an empty result.
		 *       Values < 0 mean there's no limit. Default: -1 (no limit)
		 * </ul>
		 * Note: specifying "fieldOffset" is optional. If absent, then all fields in the given module(s) will be traced.
		 * When giving multiple moduleIds you must either provide an equal number of fieldOffsets or no fieldOffsets at all!
		 *  </p>
		 *
		 * @param parameters the parameter map
		 * @return this builder
		 */
		public Builder fromMap(final Map<String, List<String>> parameters) {
			final List<String> moduleIds = Optional.ofNullable(parameters.get("moduleId"))
					.orElse(Collections.emptyList());
			final List<String> fieldOffsets = Optional.ofNullable(parameters.get("fieldOffset"))
					.orElse(Collections.emptyList());
			final String detailLevelParam = Optional.ofNullable(parameters.get("detailLevel")).map(l -> l.get(0))
					.orElse(DetailLevel.STATEMENT.name());
			final String queryDirectionParam = Optional.ofNullable(parameters.get("queryDirection")).map(l -> l.get(0))
					.orElse(QueryDirection.BOTH.name());
			final String maxModuleDistanceParam = Optional.ofNullable(parameters.get("maxModuleDistance")).map(l -> l.get(0))
					.orElse("-1");
			final String maxRecursionDepthParam = Optional.ofNullable(parameters.get("maxRecursionDepth")).map(l -> l.get(0))
					.orElse("-1");
			final boolean strictTraceParam = Optional.ofNullable(parameters.get("strictTracing")).map(t -> Boolean.valueOf(t.get(0))).orElse(Boolean.FALSE);
			final boolean isAssembled = Optional.ofNullable(parameters.get("assembled")).map(t -> Boolean.valueOf(t.get(0))).orElse(Boolean.FALSE);
			final List<String> includingModuleParameter = Optional.ofNullable(parameters.get("includingModule")).orElse(Collections.emptyList());
			
			if ( ! fieldOffsets.isEmpty() && fieldOffsets.size() != moduleIds.size()) {
				throw new IllegalArgumentException("When fieldOffset is specified, it must occur the same number of times as moduleId.");
			}

			/* the idea is that if you specify includingModule once, the same including module is used for all fields */
			if (includingModuleParameter.size() > 1 && includingModuleParameter.size() != fieldOffsets.size()) {
				throw new IllegalArgumentException("When includingModule is specified more than once, it must occur the same number of times as fieldOffset.");
			}

			if ( ! fieldOffsets.isEmpty()) {
				if (includingModuleParameter.isEmpty()) {
					setStartFields(IntStream.range(0, fieldOffsets.size())
							.mapToObj(i -> new SelectedField(EntityId.of(moduleIds.get(i)), Integer.parseInt(fieldOffsets.get(i))))
							.collect(Collectors.toList()));
				} else if (includingModuleParameter.size() == 1) {
					final EntityId includingModuleId = EntityId.of(includingModuleParameter.get(0));
					setStartFields(IntStream.range(0, fieldOffsets.size())
							.mapToObj(i -> new SelectedField(EntityId.of(moduleIds.get(i)), Integer.parseInt(fieldOffsets.get(i)), includingModuleId))
							.collect(Collectors.toList()));
				} else {
					setStartFields(IntStream.range(0, fieldOffsets.size())
							.mapToObj(i -> new SelectedField(EntityId.of(moduleIds.get(i)), Integer.parseInt(fieldOffsets.get(i)),
									EntityId.of(includingModuleParameter.get(i))))
							.collect(Collectors.toList()));
				}
			} else {
				setStartModuleIds(moduleIds.stream()
						.map(EntityId::of)
						.collect(Collectors.toList()));
			}

			setStrictTracing(strictTraceParam);
			setDetailLevel(DetailLevel.valueOf(detailLevelParam));
			setQueryDirection(QueryDirection.valueOf(queryDirectionParam));
			setMaxModuleDistance(Integer.parseInt(maxModuleDistanceParam));
			setMaxRecursionDepth(Integer.parseInt(maxRecursionDepthParam));
			setAssembled(isAssembled);

			return this;
		}

		/**
		 * Sets the project id on which the query is executed.
		 * @param projectId the project id
		 * @return this builder
		 */
		public Builder setProjectId(final EntityId projectId) {
			this.projectId = projectId;
			return this;
		}

		/**
		 * Sets the query direction of the data flow graph query.
		 * @param queryDirection the query direction
		 * @return this builder
		 */
		public Builder setQueryDirection(final QueryDirection queryDirection) {
			this.queryDirection = queryDirection;
			return this;
		}

		/**
		 * Sets the detail level of the data flow graph query.
		 * @param detailLevel the detail level
		 * @return this builder
		 */
		public Builder setDetailLevel(final DetailLevel detailLevel) {
			this.detailLevel = detailLevel;
			return this;
		}

		/**
		 * Sets the maximum distance in Modules from the starting Module that the query will cover.
		 * <p>
		 * A negative value indicates that there is no limit. A value of 0 means that the
		 * result will be empty. All other values mean that the query will stop after traversing
		 * the given number of Modules counting from the start Module.
		 * <p>
		 * This is different from {@linkplain #setMaxRecursionDepth(int) maxRecursionDepth} because the latter counts all fields and statements, whereas
		 * this limit only counts the number of Modules traversed.
		 * <p>
		 * Both parameters can be used in combination, but in order to get predictable results, it is recommended to use
		 * either {@code maxModuleDistance} or {@linkplain #setMaxRecursionDepth(int) maxRecursionDepth}.
		 * @param maxModuleDistance the maximum module distance
		 * @return this builder
		 */
		public Builder setMaxModuleDistance(final int maxModuleDistance) {
			this.maxModuleDistance = maxModuleDistance;
			return this;
		}

		/**
		 * Sets the maximum recursion depth of the query, in other words the maximum number of statements and fields that are traversed.
		 * <p>
		 * A negative value indicates that there is no limit. A value of 0 means that the result will be empty.
		 * All other values mean that the query will stop after traversing the given number of Fields or Statements.
		 * <p>
		 * This is different from {@linkplain #setMaxModuleDistance(int) maxModuleDistance} because the latter only counts the distance in Modules traversed,
		 * whereas here all fields and statements are counted, regardless of which Module they are in.
		 * <p>
		 * Both parameters can be used in combination, but in order to get predictable results, it is recommended to use
		 * either {@linkplain #setMaxModuleDistance(int) maxModuleDistance} or {@code maxRecursionDepth}.
		 *
		 * @param maxRecursionDepth the maximum recursion depth (number of fields and statements traversed by the query)
		 * @return this builder
		 */
		public Builder setMaxRecursionDepth(final int maxRecursionDepth) {
			this.maxRecursionDepth = maxRecursionDepth;
			return this;
		}

		/**
		 * Sets the list of module ids on which to start the query.
		 * <p>
		 *     When a module id is listed here, it indicates that all data fields inside of the Module shall be traced.
		 * </p>
		 * @param startModuleIds the list of start module ids
		 * @return this builder
		 */
		public Builder setStartModuleIds(final List<EntityId> startModuleIds) {
			this.startModuleIds = startModuleIds;
			return this;
		}

		/**
		 * Adds a module id to the list of start module ids.
		 * @param startModuleId the module id
		 * @return this builder
		 * @see #setStartModuleIds(List)
		 */
		public Builder addStartModuleId(final EntityId startModuleId) {
			startModuleIds.add(startModuleId);
			return this;
		}

		/**
		 * Sets the list of start fields as pairs of module id and field offset.
		 * <p>
		 *     This allows tracing of individual fields by giving the module id plus the offset
		 *     at which the field can be found within the module's source code (in characters).
		 * </p>
		 * @param startFields the list of start fields
		 * @return this builder
		 */
		public Builder setStartFields(final List<SelectedField> startFields) {
			this.startFields = startFields;
			return this;
		}

		/**
		 * Adds a field to the list of start fields.
		 * @param startField the start field
		 * @return this builder
		 * @see #setStartFields(List)
		 */
		public Builder addStartField(final SelectedField startField) {
			startFields.add(startField);
			return this;
		}

		/**
		 * Adds a field to the list of start fields.
		 * @param moduleId id of the module containing the field
		 * @param offset offset of the field within the module
		 * @return this builder
		 * @see #setStartFields(List)
		 */
		public Builder addStartField(final EntityId moduleId, final int offset) {
			startFields.add(new SelectedField(moduleId, offset));
			return this;
		}

		/**
		 * Adds a field to the list of start fields.
		 * @param moduleId id of the module containing the field
		 * @param offset offset of the field within the module
		 * @param includingModule the id another module that includes {@code module}
		 * @return this builder
		 * @see #setStartFields(List)
		 */
		public Builder addStartField(final EntityId moduleId, final int offset, @Nullable final EntityId includingModule) {
			if (offset > -1) {
				startFields.add(new SelectedField(moduleId, offset, includingModule));
			}
			return this;
		}

		/**
		 * Sets if strict tracing is required.
		 *
		 * @param strictTracing {@code true} for strict tracing. Otherwise {@code false} 
		 * @return this builder instance
		 */
		public Builder setStrictTracing(final boolean strictTracing) {
			this.strictTracing = strictTracing;
			return this;
		}

		/**
		 * Sets if tracing field in assembled mode.
		 *
		 * @param assembled {@code true} if field is traced from assembled mode
		 * @return this builder instance
		 */
		public Builder setAssembled(final boolean assembled) {
			this.assembled = assembled;
			return this;
		}

		/**
		 * Sets the information related to the tracing of the table column. It traces the module id and in the matching proxy container we search for the
		 * selected column name.
		 *
		 * @param moduleId the module id
		 * @param type the proxy container type
		 * @param name the table column name
		 * @return this builder instance
		 */
		public Builder addStartProxyField(final EntityId moduleId, final Type type, final String name) {
			if (StringUtils.isNotBlank(name) && StringUtils.isNotBlank(type.name())) {
				addStartModuleId(moduleId);
			}
			this.containerType = type;
			this.selectedTableColumnName = name;
			return this;
		}
	}
}
