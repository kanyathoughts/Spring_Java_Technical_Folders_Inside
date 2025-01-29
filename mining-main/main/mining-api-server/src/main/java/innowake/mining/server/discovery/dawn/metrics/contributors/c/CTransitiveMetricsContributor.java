package innowake.mining.server.discovery.dawn.metrics.contributors.c;

import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.TYPE_REFERENCE_TYPE;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeValue.TypeReferenceType.IMPLEMENT;
import static java.util.Objects.requireNonNull;

import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

import java.util.List;

/**
 * This class is responsible for calculating the transitive dependencies between the functions in a C project.
 */
public class CTransitiveMetricsContributor {
	private final DiscoveryBuilder builder;
	private final EntityId moduleId;
	private final DiscoveryContext context;
	private final ModuleService moduleService;

	/**
	 * Constructor for the CTransitiveMetricsContributor.
	 *
	 * @param context the discovery context
	 * @param builder the discovery builder
	 * @param moduleId the entityId of the module
	 * @param moduleService the module service
	 */
	public CTransitiveMetricsContributor(final DiscoveryContext context, final DiscoveryBuilder builder, final EntityId moduleId,
			final ModuleService moduleService) {
		this.context = context;
		this.builder = builder;
		this.moduleId = moduleId;
		this.moduleService = moduleService;
	}

	/**
	 * This method calculates the transitive dependencies between the functions and C_PROGRAM.
	 */
	public void calculateTransitiveMetrics() {
		final var cFunctionModuleIds = moduleService.findRelationship(b -> b.ofProject(context.getProjectId())
				.ofModuleInDirection(moduleId.getUid(), RelationshipDirection.OUT)
				.withType(RelationshipType.CONTAINS)
				.withDestinationType(Type.FUNCTION)
				.includeModuleDetails(false, false)).stream().map(ModuleRelationshipPojo::getDstModule).toList();

		cFunctionModuleIds.forEach(functionId -> {
			final List<ModuleRelationshipPojo> references
					= moduleService.findRelationship(b -> b.ofProject(context.getProjectId())
					.ofModuleInDirection(requireNonNull(functionId), RelationshipDirection.IN)
					.withType(RelationshipType.CALLS));
			if (references.isEmpty()) {
				return;
			}
			/* We get the entityId of foo() in this relation foo() ---Implements--> functionDefinition */
			final var implementCalls = references.stream()
					.filter(reference -> {
						final var properties = reference.getProperties();
						return properties.isPresent() && ! properties.get().isEmpty() && IMPLEMENT.name().equals(properties.get().get(TYPE_REFERENCE_TYPE.name()));
					})
					.map(ModuleRelationshipPojo::getSrcModule)
					.map(EntityId::of)
					.toList();
			/* We get the entityId of main() in this relation main() --calls(without Implement)--> functionDefinition */
			final var withoutImplementCalls = references.stream()
					.filter(reference -> {
						final var properties = reference.getProperties();
						return properties.isEmpty() || properties.get().isEmpty() || ! properties.get().containsKey(TYPE_REFERENCE_TYPE.name());
					})
					.map(ModuleRelationshipPojo::getSrcModule)
					.map(EntityId::of)
					.toList();

			declareTransitiveDependencies(implementCalls, withoutImplementCalls);
		});

	}

	/**
	 *
	 * For the following given modules:
	 * ImplementCalls contains functions in this relationship foo() ---Implements--> functionDefinition
	 * WithOutImplementCalls contains functions in this relationship main() --calls(without Implement)--> functionDefinition
	 * We declare the dependency from the call function to the implement function, i.e.main() --> foo() and their parents
	 *
	 * @param implementCalls list of modules that implement the function
	 * @param withoutImplementCalls list of modules that call the function
	 */
	private void declareTransitiveDependencies(final List<EntityId> implementCalls,
			final List<EntityId> withoutImplementCalls) {
		/* For each function that calls the functionDefinition, we declare the dependency to the functionDefinition */
		implementCalls.forEach(implementCall ->
				withoutImplementCalls.forEach(withoutImplementCall -> declareDependencies(withoutImplementCall, implementCall)));
	}


	private void declareDependencies(final EntityId source, final EntityId target) {
		/* Dependency declaration between the functions */
		final var sourceBuilder = builder.anchorTo(new ModuleFilter().setModuleIds(source),
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR);
		sourceBuilder.declareDependency(RelationshipType.CALLS, new ModuleFilter().setModuleIds(target), ResolutionFlag.MERGE_DUPLICATES)
				.setBinding(Binding.EARLY);

		/* Dependency declarations between the parent modules of the functions */
		final var sourceParentUid = moduleService.findModuleUids(q -> q.withRelationship(source, RelationshipDirection.IN,
				List.of(RelationshipType.CONTAINS)));
		final var targetParentUid = moduleService.findModuleUids(q -> q.withRelationship(target, RelationshipDirection.IN,
				List.of(RelationshipType.CONTAINS)));
		if (sourceParentUid.isEmpty() && targetParentUid.isEmpty()) {
			addError("Parent modules not found for source and target modules");
			return;
		}
		if (sourceParentUid.isEmpty()) {
			addError("Parent module not found for source module");
			return;
		}
		if (targetParentUid.isEmpty()) {
			addError("Parent module not found for target module");
			return;
		}
		/* Add the dependency for parents only if both are present */
		final var sourceParentBuilder = builder.anchorTo(new ModuleFilter().setModuleIds(EntityId.of(sourceParentUid.get(0))),
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR);
		sourceParentBuilder.declareDependency(RelationshipType.CALLS, new ModuleFilter().setModuleIds(EntityId.of(targetParentUid.get(0))),
				ResolutionFlag.MERGE_DUPLICATES)
				.setBinding(Binding.EARLY);
	}

	private void addError(final String message) {
		final var anchorToBuilder = builder.anchorTo(new ModuleFilter().setModuleIds(moduleId),
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR);
		anchorToBuilder.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY, message);
	}
}
