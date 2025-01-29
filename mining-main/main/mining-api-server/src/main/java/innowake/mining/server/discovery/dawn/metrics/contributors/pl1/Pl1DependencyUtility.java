/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.pl1;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.metrics.DawnOriginResolver;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.parsing.parser.dependency.ast.pl1.AbstractPl1ProcedureNode;

import java.util.Optional;

/**
 * Pl1 dependency utility consisting of common functionality to declare dependencies for Pl1 programs.
 *
 * @param <T> Type of procedure node
 */
public class Pl1DependencyUtility<T extends AbstractPl1ProcedureNode> {

	final DawnOriginResolver originResolver;
	final DiscoveryBuilder discoveryBuilder;
	final DiscoveryBuilder.ModuleBuilder moduleBuilder;
	final String sourcePath;

	/**
	 * Constructor.
	 *
	 * @param originResolver the origin resolver
	 * @param discoveryBuilder the discovery builder
	 * @param moduleBuilder the current module builder
	 */
	Pl1DependencyUtility(final DawnOriginResolver originResolver, final DiscoveryBuilder discoveryBuilder, final DiscoveryBuilder.ModuleBuilder moduleBuilder
			, final String sourcePath) {
		this.originResolver = originResolver;
		this.discoveryBuilder = discoveryBuilder;
		this.moduleBuilder = moduleBuilder;
		this.sourcePath = sourcePath;
	}

	/**
	 * Declares the dependencies for pl1 program.
	 * @param targetName the name of the target
	 * @param source the ast node
	 * @param levelOneProcedureNode the first level procedure node
	 * @param binding the binding
	 * @param relationship the relationship
	 * @param isAssembled the assembling of the program
	 * @param attributes the attributes to be added
	 * @param createIfMissing create a virtual module if not found
	 * @param moduleTypes the module type of the dependencies
	 */
	public void createModelDependencies(final String targetName, final AstNode source, @Nullable final T levelOneProcedureNode,
			final Binding binding, final RelationshipType relationship, final boolean isAssembled, final ModelAttributeMap<Object> attributes,
			final boolean createIfMissing, final ModuleType... moduleTypes) {
		/* Sources to which dependency should be created */
		final var actualSource = getActualSource(source, isAssembled);
		final var actualSourceObject = originResolver.resolveSourceObject(source);
		final Optional<DiscoveryBuilder.ModuleBuilder> anchoredProcedure;
		if (actualSourceObject.isPresent() && (actualSourceObject.get().getType() == Type.COPYBOOK) && isAssembled) {
			anchoredProcedure = Optional.empty();
		} else {
			anchoredProcedure = getAnchoredProcedureModule(levelOneProcedureNode);
		}

		/* Target related information */
		final var targetFilter = new ModuleFilter().setNames(targetName).setTypes(moduleTypes);
		final ModuleLocation moduleLocation = isAssembled ? originResolver.resolveLocation(source) :
				new ModuleLocation(source.getStartOffset(), source.getLength());

		anchoredProcedure.ifPresent(procedure -> declareDependency(procedure, targetFilter, moduleLocation, relationship,
				binding, attributes, createIfMissing));
		declareDependency(actualSource, targetFilter, moduleLocation, relationship, binding, attributes, createIfMissing);
	}

	/**
	 * Adds the dependency to the module builder.
	 * @param moduleBuilder the module builder
	 * @param reference	the module relationship pojo
	 * @param attributeMap the attribute map
	 * @param moduleFilter the module filter
	 * @param resolutionFlags the resolution flags
	 */
	public void addDependency(final DiscoveryBuilder.ModuleBuilder moduleBuilder, final ModuleRelationshipPojo reference,
			final Optional<ModelAttributeMap<Object>> attributeMap, final ModuleFilter moduleFilter, final ResolutionFlag... resolutionFlags) {
		final DiscoveryBuilder.DependencyBuilder dependency = moduleBuilder.declareDependency(reference.getRelationship(), moduleFilter, resolutionFlags);
		reference.getDependencyBinding().ifPresent(dependency::setBinding);
		reference.getSrcLocation().ifPresent(dependency::setLocation);
		attributeMap.ifPresent(dependency::setAttributes);
	}

	/**
	 * Gets the module builder of the actual source.
	 *
	 * @param astNode the ast node
	 * @param isAssembled whether the program is assembled or not
	 *                       
	 * @return the module builder of the actual source
	 */
	public DiscoveryBuilder.ModuleBuilder getActualSource(final AstNode astNode, final boolean isAssembled) {
		return isAssembled ? originResolver.resolve(astNode, discoveryBuilder, moduleBuilder) : moduleBuilder;
	}

	/**
	 * Gets the origin resolver.
	 *
	 * @return the OriginResolver
	 */
	public DawnOriginResolver getOriginResolver() {
		return originResolver;
	}

	private void declareDependency(final DiscoveryBuilder.ModuleBuilder moduleBuilder, final ModuleFilter targetFilter, final ModuleLocation location,
			final RelationshipType relationship, final Binding binding, final ModelAttributeMap<Object> attributeMap, final boolean createIfMissing) {
		final var dependency = moduleBuilder.declareDependency(relationship, targetFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE).setLocation(location).setBinding(binding);
		final var parentDependency = moduleBuilder.declareDependency(relationship, targetFilter,
				ResolutionFlag.RESOLVE_TO_PARENT).setBinding(binding).setLocation(location);
		if ( ! attributeMap.isEmpty()) {
			attributeMap.forEach((modelAttributeKey, value) -> {
				dependency.addAttribute(modelAttributeKey, value);
				parentDependency.addAttribute(modelAttributeKey, value);
			});
		}
		if (createIfMissing) {
			final var name = targetFilter.getNames().iterator().next();
			final var type = targetFilter.getTypes().iterator().next();
			final var origin = name.equalsIgnoreCase("SQLCA") ? Origin.ENVIRONMENT : Origin.CUSTOM;
			dependency.createIfMissing(name, type, origin);
			parentDependency.createIfMissing(name, type, origin);
		}
	}

	public Optional<DiscoveryBuilder.ModuleBuilder> getAnchoredProcedureModule(@Nullable final T procedure) {
		if (procedure == null) {
			return Optional.empty();
		}
		final var procedureType = procedure.hasReturns() ? ModuleType.PL1_FUNCTION : ModuleType.PL1_SUBROUTINE;
		final var anchoredFilter = new ModuleFilter()
				.setNames(procedure.getPrimaryEntryLabel())
				.setTypes(procedureType);
		final var sourceObject= originResolver.resolveSourceObject(procedure);
		if (sourceObject.isPresent()) {
			anchoredFilter.setContainedIn(new ModuleFilter().setPaths(sourceObject.get().getPath()));
		} else {
			anchoredFilter.setPaths(sourcePath);
		}

		return Optional.of(discoveryBuilder.anchorTo(anchoredFilter, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR));
	}
}
