/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.contributor;

import java.io.Serializable;
import java.util.Map;
import java.util.List;

import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.AdditionalInfo;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Builder interface used by contributors to contribute modules, metrics, dependencies, statements and other Discovery related information.
 */
public interface DiscoveryBuilder {

	/**
	 * Builder for declaring Modules.
	 */
	interface ModuleBuilder {
		/**
		 * Attaches additional information to the module.
		 *
		 * @param additionalInfo the object containing the addition information
		 * @return this builder
		 */
		ModuleBuilder addAdditionalInfo(AdditionalInfo additionalInfo);

		/**
		 * Adds an error marker to the module.
		 *
		 * @param severity the severity of the error
		 * @param key the key indicating the error category
		 * @param message the error message
		 * @return this builder
		 */
		ModuleBuilder addError(Severity severity, ErrorKey key, String message);

		/**
		 * Adds an error marker to the module.
		 *
		 * @param severity the severity of the error
		 * @param key the key indicating the error category
		 * @param message the error message
		 * @param cause the exception that caused the error
		 * @return this builder
		 */
		ModuleBuilder addError(Severity severity, ErrorKey key, String message, Throwable cause);

		/**
		 * Adds an error marker to the module.
		 *
		 * @param severity the severity of the error
		 * @param key the key indicating the error category
		 * @param message the error message
		 * @param location the location in the source file where the error occurred
		 * @return this builder
		 */
		ModuleBuilder addError(Severity severity, ErrorKey key, String message, AstNodeLocation location);

		/**
		 * Adds an error marker to the module.
		 *
		 * @param severity the severity of the error
		 * @param key the key indicating the error category
		 * @param message the error message
		 * @param cause the exception that caused the error
		 * @param location the location in the source file where the error occurred
		 * @return this builder
		 */
		ModuleBuilder addError(Severity severity, ErrorKey key, String message, Throwable cause, AstNodeLocation location);

		/**
		 * Marks a region of the module as "dead code".
		 * <p>
		 * Note that due to the way we currently store dead code, this method uses <i>line numbers</i> instead of the usual character offsets.
		 *
		 * @param label a label for the identified block of dead code.
		 * @param line the first line of the dead code region within the module
		 * @param numberOfLines the number of lines of the dead code region (must be >= 1)
		 * @return this builder
		 */
		ModuleBuilder addDeadCode(String label, int line, int numberOfLines);

		/**
		 * Defers an action that is run on the module after all other contributors have been executed, their results have been imported
		 * and dependencies have been resolved. Use this for advanced analysis that requires to examine results from other contributors.
		 * <p>
		 * The method implementing the deferred action must exist on the main contributor class and must be annotated with {@link DeferredAction}.
		 *
		 * @param name the name of the method implementing the deferred action on the contributor class
		 * @return this builder
		 */
		ModuleBuilder deferAction(String name);

		/**
		 * Defers an action that is run on the module after all other contributors have been executed, their results have been imported
		 * and dependencies have been resolved. Use this for advanced analysis that requires to examine results from other contributors.
		 * <p>
		 * The method implementing the deferred action must exist on the main contributor class and must be annotated with {@link DeferredAction}.
		 * <p>
		 * This overload allows to define a context object which will be passed to the deferred action. The context object must be serializable.
		 *
		 * @param name the name of the method implementing the deferred action on the contributor class
		 * @param context an object containing arbitrary user-defined information which is passed to the deferred action
		 * @return this builder
		 */
		ModuleBuilder deferAction(String name, Serializable context);

		/**
		 * Declares a statement on the module.
		 * <p>
		 * Note: to declare SQL statements use {@link StatementBuilder#setTechnology(Technology)} to set the {@link Technology#SQL} and
		 * use {@link StatementBuilder#setProperties(Map)} to set the map containing the SQL metrics of the statement.
		 *
		 * @param statementType the type of statement
		 * @return a statement builder
		 */
		StatementBuilder declareStatement(StatementType statementType);

		/**
		 * Declares an outgoing dependency on the module.
		 *
		 * @param relationship the module Relationship type
		 * @param filter the ModuleFilter describing the set of possible targets for the dependency
		 * @param flags the resolution flags that control the dependency resolution
		 * @return a dependency builder
		 */
		DependencyBuilder declareDependency(RelationshipType relationship, ModuleFilter filter, ResolutionFlag... flags);

		/**
		 * Declares an outgoing dependency on the module.
		 *
		 * @param relationship the relationship type
		 * @param filters the list of ModuleFilter describing the set of possible targets for the dependency
		 * @param flags the resolution flags that control the dependency resolution
		 * @return a dependency builder
		 */
		DependencyBuilder declareDependency(RelationshipType relationship, List<ModuleFilter> filters, ResolutionFlag... flags);
		
		/**
		 * Declares an outgoing dependency on the module.
		 * <p>
		 * This overload allows to specify the dependency target directly via a ModuleBuilder which represents the target module.
		 * This is useful when a contributor wants to create a dependency to a module it just created itself.
		 *
		 * @param relationship the module Relationship type
		 * @param module the builder for the target module
		 * @return a dependency builder
		 */
		DependencyBuilder declareDependency(RelationshipType relationship, ModuleBuilder module);
	}

	/**
	 * Extended {@link ModuleBuilder} with special operations for {@linkplain DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...) anchorTo()}.
	 */
	interface AnchorToBuilder extends ModuleBuilder {
		/**
		 * Creates a Module with the given name and type if no target for the anchoring can be found. The given name and type do not strictly have to match
		 * those supplied in the {@code ModuleFilter} passed to {@linkplain DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...) anchorTo()},
		 * but it usually makes sense if they do. The new module will be created as an "external" Module as if
		 * {@link DiscoveryBuilder#declareExternalModule(String, ModuleType, ResolutionFlag...)} was called. That is the only kind of Module that can be created through
		 * this method.
		 *
		 * @param name the name of the Module that is created
		 * @param type the type of the Module that is created
		 * @return a {@link ModuleBuilder} to add properties to the newly created OR the existing anchored Module
		 */
		ModuleBuilder createIfMissing(String name, ModuleType type);

		/**
		 * Creates a Module with the given name, type and identification if no target for the dependency can be found. The given name and type do not strictly
		 * have to match those supplied in the {@code ModuleFilter} passed to
		 * {@linkplain ModuleBuilder#declareDependency(RelationshipType, ModuleFilter, ResolutionFlag...) declareDependency()}, but it usually makes sense
		 * if they do, as otherwise the dependency still won't be resolved successfully. The new module will be created as an "external" Module as if
		 * {@link DiscoveryBuilder#declareExternalModule(String, ModuleType, ResolutionFlag...)} was called. That is the only kind of Module that can be created through
		 * this method.
		 * <p>
		 * Using this method is equivalent to calling {@linkplain DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...) anchorTo()} and
		 * {@linkplain AnchorToBuilder#createIfMissing(String, ModuleType) orCreateIfMissing()} but using the latter allows specifying additional
		 * properties on the created Module.
		 *
		 * @param name the name of the Module that is created
		 * @param type the type of the Module that is created
		 * @param identification the identification status of the Module that is created
		 * @return the dependency builder
		 */
		ModuleBuilder createIfMissing(String name, ModuleType type, Identification identification);
		
		/**
		 * Creates a Module with the given name, type and origin if no target for the dependency can be found. The given name and type do not strictly
		 * have to match those supplied in the {@code ModuleFilter} passed to
		 * {@linkplain ModuleBuilder#declareDependency(RelationshipType, ModuleFilter, ResolutionFlag...) declareDependency()}, but it usually makes sense
		 * if they do, as otherwise the dependency still won't be resolved successfully. The new module will be created as an "external" Module as if
		 * {@link DiscoveryBuilder#declareExternalModule(String, ModuleType, ResolutionFlag...)} was called. That is the only kind of Module that can be created through
		 * this method.
		 * <p>
		 * Using this method is equivalent to calling {@linkplain DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...) anchorTo()} and
		 * {@linkplain AnchorToBuilder#createIfMissing(String, ModuleType) orCreateIfMissing()} but using the latter allows specifying additional
		 * properties on the created Module.
		 *
		 * @param name the name of the Module that is created
		 * @param type the type of the Module that is created
		 * @param origin the origin of the Module that is created
		 * @return the dependency builder
		 */
		ModuleBuilder createIfMissing(String name, ModuleType type, Origin origin);

		/**
		 * Resets the type of the anchored Module only if a target Module (or multiple Modules) were found successfully.
		 *
		 * @param type the new type to set on the anchored Module
		 * @return a {@link ModuleBuilder} for the anchored Module
		 */
		ModuleBuilder andResetType(ModuleType type);
	}
	
	/**
	 * Builder for declaring dependencies.
	 */
	interface DependencyBuilder {
		/**
		 * Declares the location within the module where the dependency was found (i.e. the location of the statement that indicates the dependency).
		 *
		 * @param location the location within the module
		 * @return the dependency builder
		 */
		DependencyBuilder setLocation(ModuleLocation location);

		/**
		 * Sets the binding type.
		 *
		 * @param binding the binding type
		 * @return the dependency builder
		 */
		DependencyBuilder setBinding(Binding binding);

		/**
		 * Adds an attribute to the dependency.
		 *
		 * @param key the key of the attribute
		 * @param attribute the value of the attribute
		 * @return the dependency builder
		 */
		DependencyBuilder addAttribute(ModelAttributeKey key, Object attribute);

		/**
		 * Sets the moduleFilters of the reaching modules for conditional dependencies to the declared dependency.
		 *
		 * @param reachedFromModules list of reaching modules (conditional dependency). Empty when there are none.
		 * @return the dependency builder
		 */
		DependencyBuilder setReachedFromModules(List<ModuleFilter> reachedFromModules);

		/**
		 * Sets the moduleFilter of the reaching module to the declared dependency
		 *
		 * @param reachedFromModule the moduleFilter of the reached from module
		 * @return the dependency builder
		 */
		DependencyBuilder setReachedFromModules(ModuleFilter reachedFromModule);

		/**
		 * Sets the moduleBuilder of the reaching module for conditional dependencies to the declared dependency.
		 *
		 * @param reachedFromModule the moduleBuilder of the reachedFromModule.
		 * @return the dependency builder
		 */
		DependencyBuilder setReachedFromModules(ModuleBuilder reachedFromModule);


		/**
		 * Creates a Module with the given name and type if no target for the dependency can be found. The given name and type do not strictly have to match
		 * those supplied in the {@code ModuleFilter} passed to
		 * {@linkplain ModuleBuilder#declareDependency(RelationshipType, ModuleFilter, ResolutionFlag...) declareDependency()}, but it usually makes sense
		 * if they do, as otherwise the dependency still won't be resolved successfully. The new module will be created as an "external" Module as if
		 * {@link DiscoveryBuilder#declareExternalModule(String, ModuleType, ResolutionFlag...)} was called. That is the only kind of Module that can be created through
		 * this method.
		 * <p>
		 * Using this method is equivalent to calling {@linkplain DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...) anchorTo()} and
		 * {@linkplain AnchorToBuilder#createIfMissing(String, ModuleType) orCreateIfMissing()} but using the latter allows specifying additional
		 * properties on the created Module.
		 *
		 * @param name the name of the Module that is created
		 * @param type the type of the Module that is created
		 * @return the dependency builder
		 */
		DependencyBuilder createIfMissing(String name, ModuleType type);

		/**
		 * Creates a Module with the given name, type and identification if no target for the dependency can be found. The given name and type do not strictly
		 * have to match those supplied in the {@code ModuleFilter} passed to
		 * {@linkplain ModuleBuilder#declareDependency(RelationshipType, ModuleFilter, ResolutionFlag...) declareDependency()}, but it usually makes sense
		 * if they do, as otherwise the dependency still won't be resolved successfully. The new module will be created as an "external" Module as if
		 * {@link DiscoveryBuilder#declareExternalModule(String, ModuleType, ResolutionFlag...)} was called. That is the only kind of Module that can be created through
		 * this method.
		 * <p>
		 * Using this method is equivalent to calling {@linkplain DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...) anchorTo()} and
		 * {@linkplain AnchorToBuilder#createIfMissing(String, ModuleType) orCreateIfMissing()} but using the latter allows specifying additional
		 * properties on the created Module.
		 *
		 * @param name the name of the Module that is created
		 * @param type the type of the Module that is created
		 * @param identification the identification status of the Module that is created
		 * @return the dependency builder
		 */
		DependencyBuilder createIfMissing(String name, ModuleType type, Identification identification);

		/**
		 * Creates a Module with the given name, type and identification if no target for the dependency can be found. The given name and type do not strictly
		 * have to match those supplied in the {@code ModuleFilter} passed to
		 * {@linkplain ModuleBuilder#declareDependency(RelationshipType, ModuleFilter, ResolutionFlag...) declareDependency()}, but it usually makes sense
		 * if they do, as otherwise the dependency still won't be resolved successfully. The new module will be created as an "external" Module as if
		 * {@link DiscoveryBuilder#declareExternalModule(String, ModuleType, ResolutionFlag...)} was called. That is the only kind of Module that can be created
		 * through this method.
		 * <p>
		 * Using this method is equivalent to calling {@linkplain DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...) anchorTo()} and
		 * {@linkplain AnchorToBuilder#createIfMissing(String, ModuleType) orCreateIfMissing()} but using the latter allows specifying additional
		 * properties on the created Module.
		 *
		 * @param name the name of the Module that is created
		 * @param type the type of the Module that is created
		 * @param identification the identification status of the Module that is created
		 * @param flag the resolution flag that control the anchored module resolution
		 * @return the dependency builder
		 */
		DependencyBuilder createIfMissing(String name, ModuleType type, Identification identification, ResolutionFlag flag);
		
		/**
		 * Creates a Module with the given name, type, identification and origin, if no target for the dependency can be found. The given name and type do not
		 *  strictly have to match those supplied in the {@code ModuleFilter} passed to
		 * {@linkplain ModuleBuilder#declareDependency(RelationshipType, ModuleFilter, ResolutionFlag...) declareDependency()}, but it usually makes sense
		 * if they do, as otherwise the dependency still won't be resolved successfully. The new module will be created as an "external" Module as if
		 * {@link DiscoveryBuilder#declareExternalModule(String, ModuleType)} was called. That is the only kind of Module that can be created through
		 * this method.
		 * <p>
		 * Using this method is equivalent to calling {@linkplain DiscoveryBuilder#anchorTo(ModuleFilter, ResolutionFlag...) anchorTo()} and
		 * {@linkplain AnchorToBuilder#createIfMissing(String, ModuleType) orCreateIfMissing()} but using the latter allows specifying additional
		 * properties on the created Module.
		 *
		 * @param name the name of the Module that is created
		 * @param type the type of the Module that is created
		 * @param identification the identification status of the Module that is created
		 * @param origin the origin of the Module that is created
		 * @return the dependency builder
		 */
		DependencyBuilder createIfMissing(String name, ModuleType type, Origin origin);


		/**
		 * Sets the attributes to the dependency.
		 *
		 * @param attributeMap to set
		 * @return the dependency builder
		 */
		DependencyBuilder setAttributes(ModelAttributeMap<?> attributeMap);
	}
	
	/**
	 * Builder for declaring statements.
	 */
	interface StatementBuilder {
		/**
		 * Sets the statement text
		 *
		 * @param text the statement text
		 * @return this statement builder
		 */
		StatementBuilder setText(String text);

		/**
		 * Sets the additional properties of the statement.
		 *
		 * @param properties the map containing the addition properties (information) of the statement
		 * @return this statement builder
		 */
		StatementBuilder setProperties(Map<String, Object> properties);

		/**
		 * Sets the {@link Technology} of the statement.
		 * <p>If no {@link Technology} is set then the technology of the module is assigned</p>
		 *
		 * @param technology the {@link Technology}
		 * @return this statement builder
		 */
		StatementBuilder setTechnology(Technology technology);
	}
	
	/**
	 * Declares an "external" module. An external module is a module for which no source code is present and no source code is expected to be present.
	 * It is therefore fully identified by its name and {@code LanguageType} and only one external module with a certain name and type can exist in each project.
	 * If multiple contributors declare the same external module, the declarations are merged.
	 *
	 * @param name the name of the external module
	 * @param type the type of the external module
	 * @param flags resolution flags to control creation of module
	 * @return a module builder for the external module
	 */
	ModuleBuilder declareExternalModule(String name, ModuleType type, ResolutionFlag... flags);
	
	/**
	 * Declares an "external" module. An external module is a module for which no source code is present and no source code is expected to be present.
	 * It is therefore fully identified by its name and {@code LanguageType} and only one external module with a certain name and type can exist in each project.
	 * If multiple contributors declare the same external module, the declarations are merged.
	 * 
	 * This is also support the creation of external module with {@link Origin} which differentiate the utility module and normal external module.
	 *
	 * @param name the name of the external module
	 * @param type the type of the external module
	 * @param origin the {@link Origin}
	 * @param flags resolution flags to control creation of module
	 * @return a module builder for the external module
	 */
	ModuleBuilder declareExternalModule(String name, ModuleType type, Origin origin, ResolutionFlag... flags);

	/**
	 * Anchor to an existing module declared by another contributor. Do this to augment existing modules with additional information, react to the presence
	 * of a certain type of module or declare dependencies on included members.
	 * <p>
	 * You can optionally specify resolution flags controlling the resolution of the anchored module. In particular, when using
	 * {@link ResolutionFlag#MULTIPLE_MATCH_RESOLVE_ALL}, then the declaration provided through the builder will be applied to <i>all</i> matching modules.
	 * <p>
	 * If only existing and <b>changed</b> modules must be processed (for incremental discovery), then set the {@code metricsDate} into the {@code filter}.
	 * <pre>
	 * new ModuleFilter().setMetricsDate(discoveryContext.getModuleParameters().getMetricsDate())
	 * </pre>
	 *
	 * @param filter a filter describing the target module
	 * @param flags resolution flags controlling the target resolution
	 * @return a module builder for the anchored module
	 */
	AnchorToBuilder anchorTo(ModuleFilter filter, ResolutionFlag... flags);
}
