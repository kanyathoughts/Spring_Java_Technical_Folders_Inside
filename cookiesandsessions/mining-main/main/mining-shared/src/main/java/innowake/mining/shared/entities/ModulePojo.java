/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Supplier;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.GraphMLAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * {@code module} entity class.
 */
@MiningDataType(name = MiningEnitityNames.MODULE)
public final class ModulePojo extends MiningPojo {

	public interface Id extends Supplier<EntityId> { }
	public interface UniqueId extends Supplier<UUID> { }
	public enum Representation { PHYSICAL, VIRTUAL }

	private final EntityId project;
	private final String name;
	private final Optional<String> path;

	@MiningDataPoint(displayName = "Type", description = "Type of the Module")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "2"),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
					value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_DISTINCT_FIELD_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "TYPE" /* ModuleFieldName.TYPE */),
	})
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
					value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "MODULE_TYPE" /* AnnotationFieldName.MODULE_TYPE */),
	})
	@Usage(value = Usages.METRICS_CHART_DETAILS_MODULE, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
					value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_DISTINCT_FIELD_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "TYPE" /* ModuleFieldName.TYPE */),
	})
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
	})
	@Usage(Usages.SORT_BY)
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	@Usage(value = Usages.VIEW_MODE, attributes = {
			@UsageAttribute(key = ViewModeAttributes.LABEL_MAPPING, value = "TYPE_LABELS")
	})
	private final Type type;

	@MiningDataPoint(displayName = "Technology", description = "Technology of the Module")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "1"),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
					value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_DISTINCT_FIELD_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME,
					value = "TECHNOLOGY" /* ModuleFieldName.TECHNOLOGY */),
	})
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
					value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "MODULE_TECHNOLOGY" /* AnnotationFieldName.MODULE_TECHNOLOGY */),
	})
	@Usage(value = Usages.METRICS_CHART_DETAILS_MODULE, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
					value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_DISTINCT_FIELD_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "TECHNOLOGY" /* ModuleFieldName.TECHNOLOGY */),
	})
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
	})
	@Usage(Usages.SORT_BY)
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	@Usage(value = Usages.VIEW_MODE, attributes = {
			@UsageAttribute(key = ViewModeAttributes.LABEL_MAPPING, value = "TECHNOLOGY_LABELS")
	})
	private final Technology technology;
	private final Storage storage;
	private final Origin origin;
	private final Creator creator;
	private final Identification identification;
	private final Optional<Map<String, Object>> info;
	@MiningDataPoint(displayName = "Module Description", description = "Text describing the functionality of the Module")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Description")
	})
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT, attributes = {
			@UsageAttribute(key = GraphMLAttributes.SQL_FRAGMENT, value = "description")
	})
	private final Optional<String> description;
	private final Optional<UUID> source;
	private final Optional<BinaryValue> contentHash;
	@MiningDataPoint(displayName = "Link Hash", description = "Unique link hash of the Module")
	private final String linkHash;
	private final Optional<ModuleLocation> location;
	@MiningDataPoint(displayName = "Module Representation")
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	private final Optional<Representation> representation;
	@MiningDataPoint(displayName = "Requires Review", description = "Whether the annotations and other metadata of this Module require review by a user")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
					value = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_DISTINCT_FIELD_VALUES),
			@UsageAttribute(key = SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, value = "REQUIRES_REVIEW")
	})
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	@Usage(Usages.SORT_BY)
	private final boolean requiresReview;
	@MiningDataPoint(displayName = "Last modified", description = "The date when this Module was last modified (either through code scanning or manually)")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Modifications")
	})
	@Usage(value = Usages.VIEW_MODE, attributes = {
			@UsageAttribute(key = ViewModeAttributes.DISPLAY_AS, value = ViewModeAttributes.DISPLAY_AS_DATE)
	})
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	@Usage(Usages.SORT_BY)
	private final Optional<Instant> modifiedDate;
	@MiningDataPoint(displayName = "Last Scan", description = "The date and time when the code metrics and dependencies for this Module were last updated")
	@Usage(value = Usages.MINING_UI_MODULES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Modifications"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "3")
	})
	@Usage(value = Usages.VIEW_MODE, attributes = {
			@UsageAttribute(key = ViewModeAttributes.DISPLAY_AS, value = ViewModeAttributes.DISPLAY_AS_DATE)
	})
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	@Usage(value = Usages.METRICS_CHART_DETAILS_MODULE)
	@Usage(Usages.SORT_BY)
	private final Optional<Instant> metricsDate;
	private final Optional<SourceMetricsPojo> sourceMetrics;
	private final Optional<String> content;
	@MiningDataPoint(displayName = "Module Errors")
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	private final int errors;
	@MiningDataPoint(displayName = "Statement Count")
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	private final int statements;
	@MiningDataPoint(displayName = "SQL Statement Count")
	@Usage(value = Usages.MINING_UI_GRAPHML_EXPORT)
	private final int sqlStatements;
	private final boolean sourceCodeAvailable;
	private final Optional<EntityId> parent;
	private final Optional<String> parentPath;
	
	@Nullable
	private List<ModuleRelationshipBasePojo> dynamicRelations;
	
	private final Optional<String> dependencyHash;

	@JsonCreator
	public ModulePojo(
			@JsonProperty("uid") final UUID uid,
			@JsonProperty("nid") @JsonAlias("id") final Long nid,
			@JsonProperty("customProperties") final CustomPropertiesMap customProperties,
			@JsonProperty("projectEntity") @Nullable final EntityId project,
			@JsonProperty("project") @Nullable final UUID projectUid,
			@JsonProperty("projectId") @Nullable final Long projectNid,
			@JsonProperty("name") final String name,
			@JsonProperty("path") @Nullable final String path,
			@JsonProperty("technology") final Technology technology,
			@JsonProperty("type") final Type type,
			@JsonProperty("storage") final Storage storage,
			@JsonProperty("origin") final Origin origin,
			@JsonProperty("creator") final Creator creator,
			@JsonProperty("identification") final Identification identification,
			@JsonProperty("info") @Nullable final Map<String, Object> info,
			@JsonProperty("description") @Nullable final String description,
			@JsonProperty("source") @Nullable final UUID source,
			@JsonProperty("contentHash") @Nullable final BinaryValue contentHash,
			@JsonProperty("linkHash") final String linkHash,
			@JsonProperty("location") @Nullable final ModuleLocation location,
			@JsonProperty("representation") @Nullable final Representation representation,
			@JsonProperty("requiresReview") final boolean requiresReview,
			@JsonProperty("modifiedDate") @Nullable final Instant modifiedDate,
			@JsonProperty("metricsDate") @Nullable final Instant metricsDate,
			@JsonProperty("sourceMetrics") @Nullable final SourceMetricsPojo sourceMetrics,
			@JsonProperty("content") @Nullable final String content,
			@JsonProperty("errors") final int errors,
			@JsonProperty("statements") final int statements,
			@JsonProperty("sqlStatements") final int sqlStatements,
			@JsonProperty("sourceCodeAvailable") final boolean sourceCodeAvailable,
			@JsonProperty("parentEntity") @Nullable final EntityId parent,
			@JsonProperty("parent") @Nullable final UUID parentUid,
			@JsonProperty("parentId") @Nullable final Long parentNid,
			@JsonProperty("parentPath") @Nullable final String parentPath,
			@JsonProperty("dependencyHash") @Nullable final String dependencyHash) {
		super(EntityId.of(uid, nid), customProperties);

		this.project = project != null ? project : EntityId.of(projectUid, projectNid);
		this.name = name;
		this.path = Optional.ofNullable(path);
		this.technology = technology;
		this.type = type;
		this.storage = storage;
		this.origin = origin;
		this.creator = creator;
		this.identification = identification;
		this.info = Optional.ofNullable(info);
		this.description = Optional.ofNullable(description);
		this.source = Optional.ofNullable(source);
		this.contentHash = Optional.ofNullable(contentHash);
		this.linkHash = linkHash;
		this.location = Optional.ofNullable(location);
		this.representation = Optional.ofNullable(representation);
		this.requiresReview = requiresReview;
		this.modifiedDate = Optional.ofNullable(modifiedDate);
		this.metricsDate = Optional.ofNullable(metricsDate);
		this.sourceMetrics = Optional.ofNullable(sourceMetrics);
		this.content = Optional.ofNullable(content);
		this.errors = errors;
		this.statements = statements;
		this.sqlStatements = sqlStatements;
		this.sourceCodeAvailable = sourceCodeAvailable;
		this.parent = Optional.ofNullable(parent != null ? parent : EntityId.orNull(parentUid, parentNid));
		this.parentPath = Optional.ofNullable(parentPath);
		this.dependencyHash = Optional.ofNullable(dependencyHash);
	}
	
	@JsonIgnore
	public EntityId getProject() {
		return project;
	}
	
	@JsonProperty("project")
	public UUID getProjectUid() {
		return project.getUid();
	}
	
	@JsonProperty("projectId")
	public Long getProjectNid() {
		return project.getNid();
	}

	/**
	 * @return the name of this module
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the path of this module source if available
	 */
	public Optional<String> getPath() {
		return path;
	}

	/**
	 * @return the technology of this module
	 */
	public Technology getTechnology() {
		return technology;
	}

	/**
	 * @return the type of this module
	 */
	public Type getType() {
		return type;
	}

	/**
	 * @return the storage of this module
	 */
	public Storage getStorage() {
		return storage;
	}

	/**
	 * @return the origin of this module
	 */
	public Origin getOrigin() {
		return origin;
	}

	/**
	 * @return the creator of this module
	 */
	public Creator getCreator() {
		return creator;
	}

	/**
	 * @return {@code IDENTIFIED} if this module was found in the project. Otherwise {@code MISSING}
	 */
	public Identification getIdentification() {
		return identification;
	}

	/**
	 * @return a map containing additional meta information of this module
	 */
	public Optional<Map<String, Object>> getInfo() {
		return info;
	}

	/**
	 * @return the description of this module
	 */
	public Optional<String> getDescription() {
		return description;
	}

	/**
	 * @return the ID of this module's project
	 */
	public Optional<UUID> getSource() {
		return source;
	}

	/**
	 * @return the content hash of this module
	 */
	public Optional<BinaryValue> getContentHash() {
		return contentHash;
	}

	/**
	 * @return the link hash of this module
	 */
	public String getLinkHash() {
		return linkHash;
	}

	/**
	 * @return the module location
	 */
	public Optional<ModuleLocation> getLocation() {
		return location;
	}

	/**
	 * @return the representation of this module
	 */
	public Optional<Representation> getRepresentation() {
		return representation;
	}

	/**
	 * @return {@code true} if this module requires a review. Otherwise {@code false}
	 */
	public boolean isRequiresReview() {
		return requiresReview;
	}

	/**
	 * @return the timestamp when this module got modified, if available
	 */
	public Optional<Instant> getModifiedDate() {
		return modifiedDate;
	}

	/**
	 * @return the timestamp of the last metrics discovery 
	 */
	public Optional<Instant> getMetricsDate() {
		return metricsDate;
	}

	/**
	 * @return the {@link SourceMetricsPojo} of the last metrics discovery 
	 */
	public Optional<SourceMetricsPojo> getSourceMetrics() {
		return sourceMetrics;
	}

	/**
	 * @return the content of the module, if available.
	 */
	public Optional<String> getContent() {
		return content;
	}

	/**
	 * @return the number of error markers of the module
	 */
	public int getErrors() {
		return errors;
	}

	/**
	 * @return the number of statements of the module
	 */
	public int getStatements() {
		return statements;
	}

	/**
	 * @return the number of SQL statements of the module
	 */
	public int getSqlStatements() {
		return sqlStatements;
	}

	/**
	 * Returns if source code is available for this module. Source code is available when either this {@code module} or it's parent {@code module} have a
	 * reference to a {@code source}.
	 *
	 * @return {@code true} if source code available. Otherwise {@code false}
	 */
	public boolean isSourceCodeAvailable() {
		return sourceCodeAvailable;
	}

	/**
	 * @return the {@link EntityId} of the parent module if available
	 */
	@JsonIgnore
	public Optional<EntityId> getParent() {
		return parent;
	}
	
	@JsonProperty("parent")
	public Optional<UUID> getParentUid() {
		return parent.map(EntityId::getUid);
	}
	
	@JsonProperty("parentId")
	public Optional<Long> getParentNid() {
		return parent.map(EntityId::getNid);
	}

	/**
	 * @return the path of the parent module if available
	 */
	public Optional<String> getParentPath() {
		return parentPath;
	}
	
	/**
	 * @return the dependency hash of the module if available
	 */
	public Optional<String> getDependencyHash() {
		return dependencyHash;
	}

	public Optional<List<ModuleRelationshipBasePojo>> dynamicRelations() {
		return Optional.ofNullable(dynamicRelations).map(Collections::unmodifiableList);
	}

	public void dynamicRelations(final List<ModuleRelationshipBasePojo> dynamicRelations) {
		this.dynamicRelations = dynamicRelations;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
			.appendSuper(super.toString())
			.append("project", project)
			.append("name", name)
			.append("path", path)
			.append("technology", technology)
			.append("type", type)
			.append("storage", storage)
			.append("origin", origin)
			.append("creator", creator)
			.append("identification", identification)
			.append("info", info)
			.append("description", description)
			.append("source", source)
			.append("contentHash", contentHash)
			.append("linkHash", linkHash)
			.append("location", location)
			.append("representation", representation)
			.append("requiresReview", requiresReview)
			.append("modifiedDate", modifiedDate)
			.append("metricsDate", metricsDate)
			.append("sourceMetrics", sourceMetrics)
			.append("content", content)
			.append("errors", errors)
			.append("statements", statements)
			.append("sqlStatements", sqlStatements)
			.append("parent", parent)
			.append("parentPath", parentPath)
			.append("dependencyHash", dependencyHash)
			.toString();
	}
	
	@Override
	public boolean equals(@Nullable Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		ModulePojo that = (ModulePojo) o;
		return requiresReview == that.requiresReview && errors == that.errors && statements == that.statements && sqlStatements == that.sqlStatements
				&& sourceCodeAvailable == that.sourceCodeAvailable && Objects.equals(project, that.project) && Objects.equals(name, that.name)
				&& Objects.equals(path, that.path) && technology == that.technology && type == that.type && storage == that.storage && origin == that.origin
				&& creator == that.creator && identification == that.identification && Objects.equals(info, that.info)
				&& Objects.equals(description, that.description) && Objects.equals(source, that.source) && Objects.equals(contentHash, that.contentHash)
				&& Objects.equals(linkHash, that.linkHash) && Objects.equals(location, that.location) && Objects.equals(representation, that.representation)
				&& Objects.equals(modifiedDate, that.modifiedDate) && Objects.equals(metricsDate, that.metricsDate)
				&& Objects.equals(sourceMetrics, that.sourceMetrics) && Objects.equals(content, that.content) && Objects.equals(parent, that.parent)
				&& Objects.equals(parentPath, that.parentPath)
				&& Objects.equals(dependencyHash, that.dependencyHash);
	}

	@Override
	public int hashCode() {
		return Objects.hash(project, name, path, technology, type, storage, origin, creator, identification, info, description, source, contentHash, linkHash,
				location, representation, requiresReview, modifiedDate, metricsDate, sourceMetrics, content, errors, statements, sqlStatements,
				sourceCodeAvailable, parent, parentPath, dependencyHash);
	}
}
