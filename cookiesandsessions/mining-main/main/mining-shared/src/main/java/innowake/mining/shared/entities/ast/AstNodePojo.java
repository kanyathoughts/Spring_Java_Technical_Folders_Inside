/* Copyright (c) 2024 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.entities.ast;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.collect.Streams;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.KeyedSupplier;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.model.AstNodeLocation;

/**
 * {@code ast_node} entity class. Formerly know as {@code AstNodeV2} and {@code AstNode}.
 */
@MiningDataType(name = MiningEnitityNames.AST_NODE)
public final class AstNodePojo {

	private final UUID id;
	private final EntityId module;
	private final AstNodeLocation location;
	private final Optional<KeyedSupplier<UUID, AstNodePojo>> parent;
	private final Optional<EntityId> includedModule;
	private final Optional<Integer> sibling;
	private final Optional<KeyedSupplier<UUID, AstNodePojo>> nextSibling;
	private final Optional<KeyedSupplier<UUID, AstNodePojo>> previousSibling;
	private final KeyedSupplier<List<UUID>, List<AstNodePojo>> children;
	private final Collection<AstRelationshipPojo> relationsIn;
	private final Collection<AstRelationshipPojo> relationsOut;
	private final String type;
	private final Set<String> superTypes;
	private final String label;
	private final Map<String, Object> properties;
	
	private Set<String> additionalSuperTypes;
	
	@JsonCreator
	public AstNodePojo(
			@JsonProperty("id") final UUID id,
			@JsonProperty("moduleEntity") final EntityId module,
			@JsonProperty("module") final UUID moduleUid,
			@JsonProperty("moduleId") final Long moduleNid,
			@JsonProperty("location") @JsonAlias("advancedModuleLocation") final AstNodeLocation location,
			@JsonProperty("parent") @Nullable final KeyedSupplier<UUID, AstNodePojo> parent,
			@JsonProperty("includedModuleEntity") @Nullable final EntityId includedModule,
			@JsonProperty("includedModule") @Nullable final UUID includedModuleUid,
			@JsonProperty("includedModuleId") @Nullable final Long includedModuleNid,
			@JsonProperty("sibling") @Nullable final Integer sibling,
			@JsonProperty("nextSiblingId") @Nullable final KeyedSupplier<UUID, AstNodePojo> nextSibling,
			@JsonProperty("previousSiblingId") @Nullable final KeyedSupplier<UUID, AstNodePojo> previousSibling,
			@JsonProperty("children") final KeyedSupplier<List<UUID>, List<AstNodePojo>> children,
			@JsonProperty("relations") final @Nullable Collection<AstRelationshipPojo> relations,
			@JsonProperty("incomingRelations") @Nullable final Collection<AstRelationshipPojo> relationsIn,
			@JsonProperty("outgoingRelations") @Nullable final Collection<AstRelationshipPojo> relationsOut,
			@JsonProperty("type") final String type,
			@JsonProperty("superTypes") final Set<String> superTypes,
			@JsonProperty("label") final String label,
			@JsonProperty("properties") final Map<String, Object> properties) {
		this.id = id;
		this.module = module != null ? module : EntityId.of(moduleUid, moduleNid);
		this.location = location;
		this.parent = Optional.ofNullable(parent);
		this.includedModule = Optional.ofNullable(includedModule != null 
				? includedModule : EntityId.orNull(includedModuleUid, includedModuleNid));
		this.sibling = Optional.ofNullable(sibling);
		this.nextSibling = Optional.ofNullable(nextSibling);
		this.previousSibling = Optional.ofNullable(previousSibling);
		this.children = children;
		if (relations != null) {
			this.relationsIn = relations.stream().filter(b -> id.equals(b.getDst())).collect(Collectors.toUnmodifiableList());
			this.relationsOut = relations.stream().filter(b -> id.equals(b.getSrc())).collect(Collectors.toUnmodifiableList());
		} else {
			this.relationsIn = relationsIn != null ? relationsIn : Collections.emptyList();
			this.relationsOut = relationsOut != null ? relationsOut : Collections.emptyList();
		}
		this.type = type;
		this.superTypes = superTypes;
		this.label = label;
		this.properties = properties;
	}

	public UUID getId() {
		return id;
	}

	@JsonIgnore
	public EntityId getModule() {
		return module;
	}

	@JsonProperty("module")
	public UUID getModuleUid() {
		return module.getUid();
	}

	@JsonProperty("moduleId")
	public Long getModuleNid() {
		return module.getNid();
	}

	public AstNodeLocation getLocation() {
		return location;
	}

	@JsonProperty("parent")
	public Optional<UUID> getParentId() {
		return parent.map(KeyedSupplier::getKey);
	}
	
	@JsonIgnore
	public Optional<AstNodePojo> getParent() {
		return parent.map(KeyedSupplier::get);
	}

	@JsonIgnore
	public Optional<EntityId> getIncludedModule() {
		return includedModule;
	}

	@JsonProperty("inclusionCalleeModule")
	public Optional<UUID> getIncludedModuleUid() {
		return includedModule.map(EntityId::getUid);
	}

	@JsonProperty("inclusionCalleeId")
	public Optional<Long> getIncludedModuleNid() {
		return includedModule.map(EntityId::getNid);
	}

	public Optional<Integer> getSibling() {
		return sibling;
	}

	@JsonProperty("nextSibling")
	public Optional<UUID> getNextSiblingId() {
		return nextSibling.map(KeyedSupplier::getKey);
	}
	
	@JsonIgnore
	public Optional<AstNodePojo> getNextSibling() {
		return nextSibling.map(KeyedSupplier::get);
	}

	@JsonProperty("previousSibling")
	public Optional<UUID> getPreviousSiblingId() {
		return previousSibling.map(KeyedSupplier::getKey);
	}

	@JsonIgnore
	public Optional<AstNodePojo> getPreviousSibling() {
		return previousSibling.map(KeyedSupplier::get);
	}
	
	@JsonProperty("children")
	public List<UUID> getChildIds() {
		return children.getKey();
	}
	
	@JsonIgnore
	public List<AstNodePojo> getChildren() {
		return children.get();
	}
	
	public Collection<AstRelationshipPojo> getIncomingRelations() {
		return relationsIn;
	}
	
	public Collection<AstRelationshipPojo> getOutgoingRelations() {
		return relationsOut;
	}
	
	public String getType() {
		return type;
	}
	
	public Set<String> getSuperTypes() {
		if (additionalSuperTypes != null) {
			return Streams.concat(superTypes.stream(), additionalSuperTypes.stream()).collect(Collectors.toSet());
		} else {
			return superTypes;
		}
	}
	
	public void addSuperType(final String type) {
		if (additionalSuperTypes == null) {
			additionalSuperTypes = new HashSet<>();
		}
		additionalSuperTypes.add(type);
	}
	
	@JsonIgnore
	public Optional<Set<String>> getAdditionalSuperTypes() {
		return Optional.ofNullable(additionalSuperTypes);
	}
	
	public String getLabel() {
		return label;
	}
	
	public Map<String, Object> getProperties() {
		return properties;
	}
	
	@Override
	public String toString() {
		return new ToStringBuilder(this)
					.append("id", id)
					.append("label", label)
					.append("type", type)
					.append("superTypes", superTypes)
					.append("location", location)
					.append("module", module)
					.append("includedModule", includedModule)
					.append("parent", parent)
					.append("sibling", sibling)
					.append("nextSibling", nextSibling)
					.append("previousSibling", previousSibling)
					.append("properties", properties)
				.toString();
	}

}
