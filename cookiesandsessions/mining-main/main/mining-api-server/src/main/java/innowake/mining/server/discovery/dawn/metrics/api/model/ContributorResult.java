/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.model;


import java.io.Serializable;
import java.util.List;
import java.util.Set;

import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.StatementPojoPrototype;

/**
 * The result of a contributor execution. Contains the information collected by the contributor.
 */
public class ContributorResult implements Serializable {
	
	/**
	 * Describes the type of result. This determines how the result is treated when importing it into the database.
	 */
	public enum Type {
		/**
		 * The result describes an external module. An external module is identified by its name and type
		 * and there can be only one external module with each name and type combination. Multiple contributors
		 * may declare the same external module, in which case the declarations are merged.
		 */
		EXTERNAL_MODULE,
		/**
		 * The result needs to be "anchored" to an already existing module. Therefore importing of this result
		 * is delayed until all other module definitions have been imported. Then a matching "anchor module" is searched
		 * and the results are applied to the existing module.
		 */
		ANCHOR_TO,
		/**
		 * The result describes a module that represents an entire source file.
		 */
		ROOT_MODULE,
		/**
		 * The result describes a module that is part of another module.
		 */
		SUB_MODULE
	}
	
	private final Type type;
	
	private final ModuleFilter moduleFilter;
	private final Set<ResolutionFlag> resolutionFlags;

	private final ModulePojoPrototype moduleDefinition;
	private final List<ErrorMarker> errors;
	private final List<ModuleDeadCodePojoPrototype> deadCode;
	private final List<DeferredActionDefinition> deferrredActions;
	private final List<StatementPojoPrototype> statements;
	private final List<DependencyDefinitionPojoPrototype> dependencies;
	
	/**
	 * Creates a new contributor result
	 * 
	 * @param type the type of this contributor result
	 * @param moduleFilter the module filter identifying the described module
	 * @param resolutionFlags resolution flags for resolving the described module
	 * @param moduleDefinition the definition of the module containing basic module information
	 * @param errors error markers that shall be attached to the module
	 * @param deadCode dead code regions that shall be attached to the module
	 * @param deferrredActions deferred actions that will be executed on the module
	 * @param statements statements that shall be attached to the module
	 * @param dependencies dependencies declared on the module
	 */
	public ContributorResult(final Type type, final ModuleFilter moduleFilter, final Set<ResolutionFlag> resolutionFlags, final ModulePojoPrototype moduleDefinition,
			final List<ErrorMarker> errors, final List<ModuleDeadCodePojoPrototype> deadCode,
			final List<DeferredActionDefinition> deferrredActions, final List<StatementPojoPrototype> statements,
			final List<DependencyDefinitionPojoPrototype> dependencies) {
		this.type = type;
		this.moduleFilter = moduleFilter;
		this.resolutionFlags = resolutionFlags;
		this.moduleDefinition = moduleDefinition;
		this.errors = errors;
		this.deadCode = deadCode;
		this.deferrredActions = deferrredActions;
		this.statements = statements;
		this.dependencies = dependencies;
	}

	/**
	 * Returns the type of this contributor result.
	 *
	 * @return the type of this contributor result
	 */
	public Type getType() {
		return type;
	}
	
	/**
	 * Returns the module filter identifying the described module.
	 *
	 * @return the module filter identifying the described module
	 */
	public ModuleFilter getModuleFilter() {
		return moduleFilter;
	}
	
	/**
	 * Returns the resolution flags for resolving the described module.
	 *
	 * @return the resolution flags for resolving the described module
	 */
	public Set<ResolutionFlag> getResolutionFlags() {
		return resolutionFlags;
	}

	/**
	 * Returns the definition of the module containing basic module information.
	 *
	 * @return the definition of the module containing basic module information
	 */
	public ModulePojoPrototype getModuleDefinition() {
		return moduleDefinition;
	}
	
	/**
	 * Returns error markers that shall be attached to the module.
	 *
	 * @return error markers that shall be attached to the module
	 */
	public List<ErrorMarker> getErrors() {
		return errors;
	}
	
	/**
	 * Returns dead code regions that shall be attached to the module.
	 *
	 * @return dead code regions that shall be attached to the module
	 */
	public List<ModuleDeadCodePojoPrototype> getDeadCodes() {
		return deadCode;
	}
	
	/**
	 * Returns deferred actions that will be executed on the module.
	 *
	 * @return deferred actions that will be executed on the module
	 */
	public List<DeferredActionDefinition> getDeferrredActions() {
		return deferrredActions;
	}
	
	/**
	 * Returns statements that shall be attached to the module.
	 *
	 * @return statements that shall be attached to the module
	 */
	public List<StatementPojoPrototype> getStatements() {
		return statements;
	}
	
	/**
	 * Returns dependencies declared on the module.
	 *
	 * @return dependencies declared on the module
	 */
	public List<DependencyDefinitionPojoPrototype> getDependencies() {
		return dependencies;
	}
}
