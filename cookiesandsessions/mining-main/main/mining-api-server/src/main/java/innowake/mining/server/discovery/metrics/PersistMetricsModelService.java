/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.metrics;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.shared.entities.ModuleUndiscoveredPojoPrototype;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.io.DiscoveryModuleImporter;
import innowake.mining.data.io.DiscoveryReferenceImporter;
import innowake.mining.data.io.LazyModelArtifact;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.data.model.discovery.ModelDeadCode;
import innowake.mining.data.model.discovery.ModelDependency;
import innowake.mining.data.model.discovery.ModelSqlStatement;
import innowake.mining.data.model.discovery.ModelStatement;
import innowake.mining.server.util.ProgressMonitorThrottle;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Persistence Metrics Model Service to import Discover metrics result from discovery to DB.
 */
@Service
public class PersistMetricsModelService {

	private static final Logger LOG = LoggerFactory.getLogger(PersistMetricsModelService.class);
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private DiscoveryModuleImporter discoveryModuleImporter;

	/**
	 * Imports the given {@code undiscoveredEntities} into the database.
	 * 
	 * @param undiscoveredEntities list of {@link ModuleUndiscoveredPojoPrototype}
	 * @param progressMonitor the {@link ProgressMonitor}
	 */
	public void importUndiscoveredEntities(final List<ModuleUndiscoveredPojoPrototype> undiscoveredEntities,
			final ProgressMonitor progressMonitor) {
		for (int index = 0; index < undiscoveredEntities.size(); index++) {
			final ModuleUndiscoveredPojoPrototype entity = undiscoveredEntities.get(index);
			progressMonitor.checkCanceled();
			final String message = String.format("Importing undiscovered entities (%d/%d)", Integer.valueOf(index), Integer.valueOf(undiscoveredEntities.size()));
			LOG.debug(() -> message);
			ProgressMonitorThrottle.throttleStepDescription(message, progressMonitor);
			moduleService.createUndiscovered(entity);
		}
	}

	/**
	 * Imports the given {@code modelArtifact} into the database.
	 * 
	 * @param projectId the project Id
	 * @param modelArtifact the {@link ModelArtifact} to import
	 * @param moduleParameters the additional module parameters
	 * @return the imported {@link Module}
	 */
	public EntityId importModule(final EntityId projectId, final ModelArtifact modelArtifact, final ModuleParameters moduleParameters) {
		return discoveryModuleImporter.importModule(projectId, modelArtifact, moduleParameters);
	}

	/**
	 * Updates all properties of the given {@code modelArtifact} including its generic and dependent metrics if they have been modified.
	 * 
	 * @param projectId the project Id
	 * @param modelArtifact the {@link LazyModelArtifact} to update
	 * @param referenceImporter the {@link DiscoveryReferenceImporter} required to update any modified references
	 * @param moduleParameters the additional module parameters
	 * @return the updated {@link Module} or @code null} if the module was not stored
	 */
	@Nullable
	public EntityId updateModuleAll(final EntityId projectId, final LazyModelArtifact modelArtifact, final DiscoveryReferenceImporter referenceImporter, 
			final ModuleParameters moduleParameters) {
		EntityId updatedModule = null;
		
		if (modelArtifact.isModified()) {
			updatedModule = updateModule(projectId, modelArtifact, moduleParameters);
			importGenericMetrics(projectId, modelArtifact);
			importDependentMetrics(projectId, modelArtifact, referenceImporter);
			modelArtifact.setUnmodified(true);
		}

		return updatedModule;
	}

	/**
	 * Updates all properties of the given {@code modelArtifact} if any has been modified.
	 * 
	 * @param projectId the project Id
	 * @param modelArtifact the {@link ModelArtifact} to update
	 * @param moduleParameters the additional module parameters
	 * @return the updated {@link Module}
	 */
	@Nullable
	public EntityId updateModule(final EntityId projectId, final LazyModelArtifact modelArtifact, final ModuleParameters moduleParameters) {
		if (modelArtifact.hasModifiedGenericProperty() || modelArtifact.hasModifiedDeadCode()) {
			return discoveryModuleImporter.updateModule(projectId, modelArtifact, moduleParameters);
		}
		return null;
	}

	/**
	 * Imports the generic metrics of the given {@code modelArtifact} like errors, SQL statements, general statements and dead code.
	 * All existing data will be deleted from the database beforehand and the current data of the {@code modelArtifact} is inserted.
	 * 
	 * @param projectId the project Id
	 * @param modelArtifact the {@link ModelArtifact} to update the generic metrics for
	 */	
	public void importGenericMetrics(final EntityId projectId, final ModelArtifact modelArtifact) {
		final EntityId moduleId = Objects.requireNonNull(modelArtifact.getModuleId());
		final Stream<ErrorMarker> errorMarkers;
		final Stream<ModelDeadCode> deadCodes;

		final SourceMetrics sourceMetrics;
		if (modelArtifact instanceof LazyModelArtifact) {
			final LazyModelArtifact lma = (LazyModelArtifact) modelArtifact;

			/* we do not load any existing errors from DB and modify them so we don't have to check, if the resolved
			 * ones have been modified */
			errorMarkers = lma.getAddedErrors();

			if (lma.getResolvedSqlStatements().anyMatch(ModelSqlStatement::isModified)) {
				moduleService.deleteStatement(b -> b.ofModule(moduleId).withTechnology(Technology.SQL));
				importStatements(moduleId, Technology.SQL, Stream.concat(lma.getResolvedSqlStatements(), lma.getAddedSqlStatements()), checkForDuplicates(modelArtifact));
			} else {
				/* No duplicates check for SQL statements like for none SQL statements: checkForDuplicates(modelArtifact) */
				importStatements(moduleId, Technology.SQL, lma.getAddedSqlStatements(), false);
			}

			/* we do not load any existing ModelDeadCode from DB and modify them so we don't have to check, if the resolved
			 * once have been modified */
			deadCodes = lma.getAddedModelDeadCode();

			if (lma.getResolvedStatements().anyMatch(ModelStatement::isModified)) {
				moduleService.deleteStatement(b -> b.ofModule(moduleId).notWithTechnology(Technology.SQL));

				importStatements(moduleId, ResolveTargetHelper.toTechnology(modelArtifact.getLanguage()),
						Stream.concat(lma.getAddedStatements(), lma.getResolvedStatements()), checkForDuplicates(modelArtifact));
			} else {
				importStatements(moduleId, ResolveTargetHelper.toTechnology(modelArtifact.getLanguage()), lma.getAddedStatements(), checkForDuplicates(modelArtifact));
			}

			sourceMetrics = lma.getResolvedSourceMetrics();
		} else {
			errorMarkers = modelArtifact.getErrors();

			importStatements(moduleId, Technology.SQL, modelArtifact.getSqlStatements(), checkForDuplicates(modelArtifact));

			deadCodes = modelArtifact.getDeadCode();

			importStatements(moduleId, ResolveTargetHelper.toTechnology(modelArtifact.getLanguage()), modelArtifact.getStatements(), checkForDuplicates(modelArtifact));

			sourceMetrics = modelArtifact.getSourceMetrics();
		}

		moduleService.createErrorMarkers(errorMarkers.map(errorMarker -> errorMarker.convertToPojoPrototype().setModule(moduleId).setProject(projectId)).collect(Collectors.toList()));

		moduleService.createDeadCodes(deadCodes.map(deadCode -> deadCode.convertToPojoPrototype().setModule(moduleId)).collect(Collectors.toList()));

		if (sourceMetrics != null && sourceMetrics.isModified()) {
			final var prototype = sourceMetrics.convertToPojoPrototype();
			prototype.setModule(moduleId);
			moduleService.putSourceMetrics(prototype);
		}
	}

	/**
	 * Imports the dependent metrics of the given {@code modelArtifact} like dependency and virtual modules references.
	 * All existing data will be deleted from the database beforehand and the current data of the {@code modelArtifact} is inserted.
	 * 
	 * @param projectId the project Id
	 * @param modelArtifact the {@link ModelArtifact} to update the dependent metrics for
	 * @param referenceImporter the {@link DiscoveryReferenceImporter}
	 */
	public void importDependentMetrics(final EntityId projectId, final ModelArtifact modelArtifact, final DiscoveryReferenceImporter referenceImporter) {
		Set<ModelArtifact> virtualModules = Collections.emptySet();

		if (modelArtifact instanceof LazyModelArtifact) {
			final LazyModelArtifact lma = (LazyModelArtifact) modelArtifact;
			/* delete all modified dependencies and recreate them */
			lma.getResolvedDependencies().filter(ModelDependency::isModified)
										 .forEach(dependency -> {
				moduleService.deleteRelationship(b -> b.byId(Objects.requireNonNull(dependency.getId(), "Reference id must not be null")));
				importReference(projectId, dependency, modelArtifact, referenceImporter);
			});

			/* Store newly added dependencies */
			lma.getAddedDependencies().forEach(dependency -> importReference(projectId, dependency, modelArtifact, referenceImporter));

			if (lma.hasModifiedVirtualModules()) {
				moduleService.deleteRelationship(q -> q.ofSource(Objects.requireNonNull(modelArtifact.getModuleId(), "Module id must not be null"))
													 .withType(RelationshipType.CONTAINS));
				virtualModules = modelArtifact.getVirtualModules();
			}
		} else {
			modelArtifact.getDependencies().forEach(modelDependency -> importReference(projectId, modelDependency, modelArtifact, referenceImporter));
			virtualModules = modelArtifact.getVirtualModules();
		}

		for (final ModelArtifact virtualModule : virtualModules) {
			if (virtualModule != null) {
				referenceImporter.importContainsModule(projectId, modelArtifact, virtualModule);
			}
		}
	}

	private void importStatements(final EntityId moduleId, final Technology technology, final Stream<? extends ModelStatement> statements, final boolean checkForDuplicates) {
		if (checkForDuplicates) {
			statements.forEach(statement -> moduleService.createStatement(statement.convertToPojoPrototype()
																					.setModule(moduleId)
																					.setTechnology(technology), checkForDuplicates));
		} else {
			moduleService.createStatements(statements.map(statement -> statement.convertToPojoPrototype()
																				.setModule(moduleId)
																				.setTechnology(technology)).collect(Collectors.toList()));
		}
	}

	private void importReference(final EntityId projectId, @Nullable final ModelDependency modelDependency, final ModelArtifact modelArtifact,
			final DiscoveryReferenceImporter referenceImporter) {
		if (modelDependency != null) {
			final ModelArtifact target = modelDependency.getTarget();
			final Technology targetTechnology = ResolveTargetHelper.toTechnology(target.getLanguage());
			final Type targetType = ResolveTargetHelper.toType(target.getType());
			final RelationshipType referenceType = RelationshipType.from(targetTechnology, targetType);
			final ModuleLocation fromLocation = modelDependency.getLocation().orElse(null);
			final ModuleLocation toLocation = target.getLocation().orElse(null);
			final List<ModelArtifact> conditionalDependencies = modelDependency.getConditionalDependencies();
			final List<EntityId> validIfReachedFrom = conditionalDependencies.stream().map(ModelArtifact::getModuleId).collect(Collectors.toList());
			Identification targetIdentification = target.getIdentification();
			if (targetIdentification == Identification.MISSING && target.getOrigin() == Origin.ENVIRONMENT) {
				targetIdentification = Identification.IDENTIFIED;
			}

			final Optional<UUID> importReference = referenceImporter.importReference(projectId, modelArtifact, target,
					target.getName(), targetTechnology, targetType, 
					Objects.requireNonNull(target.getOrigin(), "Origin of target module must not be null"), 
					targetIdentification,
					modelDependency.getBinding().name(), modelDependency.getAttributes().toString(), referenceType, fromLocation, toLocation,
					modelDependency.isCheckForDuplicates() || checkForDuplicates(modelArtifact), validIfReachedFrom);

			/* if a reference was created then we set it into the ModelDependency for which it was created */
			importReference.ifPresent(modelDependency::setId);
		}
	}

	private static boolean checkForDuplicates(final ModelArtifact sourceModule) {
		final ResolveTarget type = sourceModule.getType();
		return type == ResolveTarget.COBOL_COPYBOOK || type == ResolveTarget.COBOL_COPYLIB || type == ResolveTarget.COBOL_COPYPROC ||
				type == ResolveTarget.NATURAL_COPYCODE ||
				type == ResolveTarget.PL1_COPYBOOK ||
				type == ResolveTarget.JCL_PROC || type == ResolveTarget.JCL_EXEC_PGM || type == ResolveTarget.JCL_EXEC;
	}
}
