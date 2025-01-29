/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.ast;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.AstNodeLocation;

/**
 * AST Node prototype with additional attributes for the generation process.
 */
public class StoreAstPrototype extends AstNodePojoPrototype {
	
	private final List<StoreAstPrototype> children = new ArrayList<>();
	private final List<StoreAstRelationship> relAst = new ArrayList<>();
	private final List<AstModuleRelationshipPojoPrototype> relMod = new ArrayList<>();
	
	private final int inclusionLevel;
	private final int inclusionOrdinal;
	
	/**
	 * Constructor for node which are not part of an inclusion.
	 * 
	 * @param type the type 
	 * @param label the label to be displayed for this node
	 * @param module the module for which the ast was calculated, e.g. not the CopyBook if a Module imported it.
	 * @param location the advanced module location
	 */
	public StoreAstPrototype(final String type, final String label, final EntityId module, final AstNodeLocation location) {
		this(type, label, module, location, -1, -1, null);
	}
	
	/**
	 * Constructor for nodes which are part of an inclusion.
	 * 
	 * @param type the type 
	 * @param label the label to be displayed for this node
	 * @param module the module
	 * @param location the advanced module location
	 * @param inclusionLevel the inclusion level if this node is part of an inclusion
	 * @param inclusionOrdinal the inclusion ordinal if this node is part of an inclusion
	 * @param includedModule the called module if this node is part of an inclusion
	 */
	public StoreAstPrototype(
			final String type, 
			final String label,
			final EntityId module, 
			final AstNodeLocation location,
			final int inclusionLevel,
			final int inclusionOrdinal,
			@Nullable final EntityId includedModule) {
		this.type.set(type);
		this.label.set(label);
		this.module.set(module);
		this.location.set(location);
		this.inclusionLevel = inclusionLevel;
		this.inclusionOrdinal = inclusionOrdinal;
		this.includedModule.set(includedModule);
	}
	
	public boolean hasChildren() {
		return ! children.isEmpty();
	}
	
	public List<StoreAstPrototype> children() {
		return Collections.unmodifiableList(children);
	}

	public StoreAstPrototype resetChildren(final Collection<StoreAstPrototype> children) {
		this.children.clear();
		children.stream().forEach(this::addChild);
		return this;
	}

	public StoreAstPrototype addChild(final StoreAstPrototype child) {
		children.add(child);
		id.overrides(child.parent, Definable::getNonNull);
		new Definable<>(false, "AstNode.parent.child").overrides(child.sibling, d -> children.indexOf(child)).set(-1);
		return this;
	}
	
	public AstRelationshipPojoPrototype addRelationshipTo(StoreAstPrototype dst, AstRelationshipType type) {
		final var rel = new StoreAstRelationship();
		rel.setSrc(this);
		rel.setDst(dst);
		rel.setType(type);
		relAst.add(rel);
		return rel;
	}
	
	public AstModuleRelationshipPojoPrototype addModuleRelationship(EntityId module, AstModuleRelationshipType type) {
		final var rel = new AstModuleRelationshipPojoPrototype();
		id.overrides(rel.node, Definable::getNonNull);
		rel.setModule(module);
		rel.setType(type);
		relMod.add(rel);
		return rel;
	}
	
	public List<StoreAstRelationship> getRelationships() {
		return relAst;
	}
	
	public List<AstModuleRelationshipPojoPrototype> getModuleRelationships() {
		return relMod;
	}

	/**
	 * Returns the inclusion level if this node is part of an inclusion or {@code -1} if not.
	 * 
	 * @return the inclusion level
	 */
	public int getInclusionLevel() {
		return inclusionLevel;
	}
	
	/**
	 * Returns the inclusion ordinal if this node is part of an inclusion or {@code -1} if not.
	 * 
	 * @return the inclusion ordinal
	 */
	public int getInclusionOrdinal() {
		return inclusionOrdinal;
	}
	
}
