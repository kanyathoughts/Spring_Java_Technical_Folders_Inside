/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

import innowake.lib.core.api.lang.NonNullByDefault;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.data.access.postgres.ModelArtifactPgDao;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.data.model.discovery.ModelDeadCode;
import innowake.mining.data.model.discovery.ModelDependency;
import innowake.mining.data.model.discovery.ModelSqlStatement;
import innowake.mining.data.model.discovery.ModelStatement;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Special implementation of {@link ModelArtifact} that lazily loads additional information from the database and also knows when it has been modified.
 */
public class LazyModelArtifact extends ModelArtifact {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.DISCOVERY_MODULE_REPO);
	
	private final transient ModelArtifactPgDao modelArtifactDao;
	
	@Nullable
	private final EntityId parentId;

	private List<ModelDependency> addedDependencies = new ArrayList<>();
	private List<ModelStatement> addedStatements = new ArrayList<>();
	private List<ModelSqlStatement> addedSqlStatements = new ArrayList<>();
	private List<ErrorMarker> addedErrors = new ArrayList<>();
	private List<ModelDeadCode> addedModelDeadCode = new ArrayList<>();
	
	private boolean resolvedErrorMakers;
	private boolean resolvedStatements;
	private boolean resolvedSqlStatements;
	private boolean resolvedExcelSheetDeadCode;
	private boolean resolvedDependencies;
	private boolean resolvedVirtualModules;
	private boolean resolvedParentModule;
	private boolean resolvedSourceMetrics;
	
	private boolean modifiedGenericProperty;
	private boolean modifiedVirtualModules;

	/**
	 * Constructor.
	 * 
	 * @param modelArtifactDao the {@link ModelArtifactPgDao} for lazy loading
	 * @param parentId the {@link EntityId} of the parent module of this artifact for lazy loading
	 */
	public LazyModelArtifact(final ModelArtifactPgDao modelArtifactDao, @Nullable final EntityId parentId) {
		this.modelArtifactDao = modelArtifactDao;
		this.parentId = parentId;
	}
	
	/**
	 * Copy constructor. Does not clone modifications to the LazyModelArtifact.
	 * 
	 * @param other the other {@code LazyModelArtifact} to copy
	 */
	public LazyModelArtifact(final LazyModelArtifact other) {
		super(other);
		modelArtifactDao = other.modelArtifactDao;
		parentId = other.parentId;
		/* the "modified" and "resolved" flags are not copied */
	}
	
	/**
	 * Sets this artifact to an unmodified state.
	 * <p>If {@code stored} is {@code true} and if {@link ModelDependency}s have already been resolved from the database then all added
	 * {@link ModelDependency}s are copied into the list of resolved {@link ModelDependency}s. If no dependencies were resolved yet, then
	 * the list for the added {@link ModelDependency}s is just cleared. </p>
	 * 
	 * @param stored {@code true} if this model artifact was stored in the database. Otherwise {@code false}.
	 */
	public void setUnmodified(final boolean stored) {
		modifiedGenericProperty = false;
		modifiedVirtualModules = false;

		if (stored) {
			if (resolvedDependencies) {
				dependencies.addAll(addedDependencies);
			}
			addedDependencies.clear();

			if (resolvedStatements) {
				statements.addAll(addedStatements);
			}
			addedStatements.clear();
			
			if (resolvedSqlStatements) {
				sqlStatements.addAll(addedSqlStatements);
			}
			addedSqlStatements.clear();

			if (resolvedErrorMakers) {
				errors.addAll(addedErrors);
			}

			if (resolvedExcelSheetDeadCode) {
				deadCode.addAll(addedModelDeadCode);
			}
		}
	}

	@Override
	protected void reset() {
		super.reset();

		addedDependencies = new ArrayList<>();
		addedStatements = new ArrayList<>();
		addedSqlStatements = new ArrayList<>();
		addedErrors = new ArrayList<>();
		addedModelDeadCode = new ArrayList<>();

		resolvedDependencies = false;
		resolvedExcelSheetDeadCode = false;
		resolvedErrorMakers = false;
		resolvedParentModule = false;
		resolvedSqlStatements = false;
		resolvedStatements = false;
		resolvedVirtualModules = false;
		resolvedSourceMetrics = false;
		
		modifiedGenericProperty = false;
		modifiedVirtualModules = false;
	}
	
	/**
	 * @return {@code true} if any property of this artifact has been modified; {@code false} otherwise
	 */
	public boolean isModified() {
		return hasModifiedGenericProperty()
			|| hasModifiedDependencies()
			|| hasModifiedStatements()
			|| hasModifiedSqlStatements()
			|| hasModifiedErrors()
			|| hasModifiedDeadCode()
			|| hasModifiedVirtualModules()
			|| sourceMetrics != null && sourceMetrics.isModified();
	}
	
	/**
	 * @return {@code true} if any generic property has been modified; {@code false} otherwise 
	 */
	public boolean hasModifiedGenericProperty() {
		return modifiedGenericProperty;
	}
	
	/**
	 * @return {@code true} if any dependency information has been modified; {@code false} otherwise 
	 */
	public boolean hasModifiedDependencies() {
		return ! addedDependencies.isEmpty() || dependencies.stream().anyMatch(ModelDependency::isModified);
	}
	
	/**
	 * @return {@code true} if any statement information has been modified; {@code false} otherwise 
	 */
	public boolean hasModifiedStatements() {
		return ! addedStatements.isEmpty() || statements.stream().anyMatch(ModelStatement::isModified);
	}
	
	/**
	 * @return {@code true} if any SQL statement information has been modified; {@code false} otherwise 
	 */
	public boolean hasModifiedSqlStatements() {
		return ! addedSqlStatements.isEmpty() || sqlStatements.stream().anyMatch(ModelSqlStatement::isModified);
	}
	
	/**
	 * @return {@code true} if any error information has been modified; {@code false} otherwise 
	 */
	public boolean hasModifiedErrors() {
		/* we do not load any existing errors from DB and modify them so we don't check the resolved errors here */
		return ! addedErrors.isEmpty();
	}
	
	/**
	 * @return {@code true} if any dead code information has been modified; {@code false} otherwise 
	 */
	public boolean hasModifiedDeadCode() {
		/* we do not load any existing ModelDeadCode from DB and modify them so we don't check the resolved once here */
		return ! addedModelDeadCode.isEmpty();
	}
	
	/**
	 * @return {@code true} if any virtual module information has been modified; {@code false} otherwise 
	 */
	public boolean hasModifiedVirtualModules() {
		return modifiedVirtualModules || (virtualModules instanceof ModifyAwareSet && ((ModifyAwareSet<?>) virtualModules).modified);
	}
	
	@Override
	public ModelArtifact setModuleId(final EntityId moduleId) {
		modifiedGenericProperty = true;
		return super.setModuleId(moduleId);
	}
	
	@Override
	public ModelArtifact setName(final String name) {
		modifiedGenericProperty = true;
		return super.setName(name);
	}
	
	@Override
	public ModelArtifact setPath(@Nullable final String path) {
		modifiedGenericProperty = true;
		return super.setPath(path);
	}
	
	@Override
	public ModelArtifact setType(final ResolveTarget type) {
		modifiedGenericProperty = true;
		return super.setType(type);
	}
	
	@Override
	public ModelArtifact setLocation(final ModuleLocation location) {
		modifiedGenericProperty = true;
		return super.setLocation(location);
	}
	
	@Override
	public ModelArtifact setMetadata(final Map<String, Object> metadata) {
		modifiedGenericProperty = true;
		return super.setMetadata(metadata);
	}
	
	@Override
	public void addMetadata(final String name, final String value) {
		modifiedGenericProperty = true;
		super.addMetadata(name, value);
	}

	@Override
	public Stream<ModelDependency> getDependencies() {
		if ( ! resolvedDependencies) {
			try {
				dependencies = new ArrayList<>(modelArtifactDao.findDependencies(Objects.requireNonNull(moduleId)));
				resolvedDependencies = true;
			} catch (final Exception exception) {
				LOG.error("Strange exception", exception);
			}
		}

		return toStream(dependencies, addedDependencies);
	}

	/**
	 * @return the {@link ModelDependency} that have been resolved from database or an empty stream, if none have been resolved yet.
	 */
	public Stream<ModelDependency> getResolvedDependencies() {
		return dependencies.stream();
	}

	/**
	 * @return the {@link ModelDependency} that have been added but not yet stored in the database.
	 */
	public Stream<ModelDependency> getAddedDependencies() {
		return addedDependencies.stream();
	}

	@Override
	public Stream<ErrorMarker> getErrors() {
		if ( ! resolvedErrorMakers) {
			errors = modelArtifactDao.findErrorMarkers(Objects.requireNonNull(moduleId));
			resolvedErrorMakers = true;
		}

		return toStream(errors, addedErrors);
	}

	/**
	 * @return the {@link ModelDependency} that have been added but not yet stored in the database.
	 */
	public Stream<ErrorMarker> getAddedErrors() {
		return addedErrors.stream();
	}

	@Override
	public Stream<ModelStatement> getStatements() {
		if ( ! resolvedStatements) {
			statements = modelArtifactDao.findStatements(Objects.requireNonNull(moduleId));
			resolvedStatements = true;
		}

		return toStream(statements, addedStatements);
	}

	/**
	 * @return a new stream of {@link  ModelStatement ModelStatements} that have been resolved from the DB. The stream is empty if they have not been
	 * resolved yet or if none exists.
	 */
	public Stream<ModelStatement> getResolvedStatements() {
		return statements.stream();
	}

	/**
	 * @return a new stream of {@link ModelStatement ModelStatements} that have been added but not yet stored in the DB.
	 */
	public Stream<ModelStatement> getAddedStatements() {
		return addedStatements.stream();
	}
	
	@Override
	public Stream<ModelSqlStatement> getSqlStatements() {
		if ( ! resolvedSqlStatements) {
			sqlStatements = modelArtifactDao.findSqlStatements(Objects.requireNonNull(moduleId));
			resolvedSqlStatements = true;
		}

		return toStream(sqlStatements, addedSqlStatements);
	}

	/**
	 * @return a new stream of {@link  ModelSqlStatement ModelSqlStatements} that have been resolved from the DB. The stream is empty if they have not been
	 * resolved yet or if none exists.
	 */
	public Stream<ModelSqlStatement> getResolvedSqlStatements() {
		return sqlStatements.stream();
	}

	/**
	 * @return a new stream of {@link ModelSqlStatement ModelSqlStatements} that have been added but not yet stored in the DB.
	 */
	public Stream<ModelSqlStatement> getAddedSqlStatements() {
		return addedSqlStatements.stream();
	}
	
	@Override
	public void addDependencies(final List<ModelDependency> dependencies) {
		LOG.trace("Adding model dependencies on [{},{},{}]. {}", moduleId, getName(), getType(), getDebugString(errors));
		addedDependencies.addAll(dependencies);
	}
	
	@Override
	public void addDependency(final ModelDependency dependency) {
		LOG.trace("Adding model dependency on [{},{},{}]. {}", moduleId, getName(), getType(), getDebugString(errors));
		addedDependencies.add(dependency);
	}
	
	@Override
	public boolean addStatement(final ModelStatement s) {
		if ( ! addedStatements.contains(s)) {
			if ( ! resolvedStatements) {
				statements = modelArtifactDao.findStatements(Objects.requireNonNull(moduleId));
				resolvedStatements = true;
			}

			if ( ! statements.contains(s)) {
				addedStatements.add(s);
				return true;
			}
		}
		return false;
	}

	@Override
	public void addSqlStatement(final ModelSqlStatement s) {
		addedSqlStatements.add(s);
	}

	@Override
	public void addError(final ErrorMarker e) {
		LOG.trace("Adding model error on [{},{},{}]. {}", moduleId, getName(), getType(), getDebugString(errors));
		addedErrors.add(e);
	}
	
	@Override
	public void addErrors(final List<ErrorMarker> errors) {
		/* If errors are empty then we don't need to mark the module as modified */
		if ( ! errors.isEmpty()) {
			LOG.trace("Adding model errors on [{},{},{}]. {}", moduleId, getName(), getType(), getDebugString(errors));
			addedErrors.addAll(errors);
		}
	}

	@Override
	public void addDeadCode(final ModelDeadCode d) {
		addedModelDeadCode.add(d);
	}

	@Override
	public Stream<ModelDeadCode> getDeadCode() {
		if ( ! resolvedExcelSheetDeadCode) {
			deadCode = modelArtifactDao.findDeadCodes(Objects.requireNonNull(moduleId));
			resolvedExcelSheetDeadCode = true;
		}

		return toStream(deadCode, addedModelDeadCode);
	}

	/**
	 * @return the {@link ModelDeadCode} that have been added but not yet stored in the database.
	 */
	public Stream<ModelDeadCode> getAddedModelDeadCode() {
		return addedModelDeadCode.stream();
	}
	
	@Override
	public Set<ModelArtifact> getVirtualModules() {
		if ( ! resolvedVirtualModules) {
			virtualModules = new HashSet<>(modelArtifactDao.find(b -> b.withSourceRelationshipsFrom(Objects.requireNonNull(getModuleId()), RelationshipType.CONTAINS)));
			resolvedVirtualModules = true;
		}
		if ( ! (virtualModules instanceof ModifyAwareSet)) {
			virtualModules = new ModifyAwareSet<>(virtualModules);
		}
		return virtualModules;
	}
	
	@Override
	@Nullable
	public ModelArtifact getParentModule() {
		if ( ! resolvedParentModule && parentId != null) {
			parentModule = modelArtifactDao.findOne(b -> b.byId(parentId))
											.orElseThrow(() -> new MiningEntityNotFoundException("Unable to find parent module with id: " + parentId));
			resolvedParentModule = true;
		}
		return parentModule;
	}

	/**
	 * @return the {@link EntityId} of the parent {@link ModelArtifact} of this {@link ModelArtifact}
	 */
	@Nullable
	public EntityId getParentModuleId() {
		return parentId;
	}
	
	@Override
	public ModelArtifact setVirtualModules(final Set<ModelArtifact> virtualModules) {
		LOG.trace("Setting virtual model artifacts on [{},{},{}]. {}", moduleId, getName(), getType(), getDebugString(errors));
		modifiedVirtualModules = true;
		resolvedVirtualModules = true; /* avoids unnecessary DB access */
		return super.setVirtualModules(virtualModules);
	}
	
	/**
	 * Adds the given {@code artifact} as a virtual module contained in this artifact.
	 * 
	 * @param artifact the {@link LazyModelArtifact} contained in this artifact
	 */
	public void addVirtualModule(final LazyModelArtifact artifact) {
		modifiedVirtualModules = true;
		if ( ! resolvedVirtualModules) {
			virtualModules = new HashSet<>(modelArtifactDao.find(b -> b.withSourceRelationshipsFrom(Objects.requireNonNull(getModuleId()), RelationshipType.CONTAINS)));
			resolvedVirtualModules = true;
		}
		LOG.trace("Adding virtual model artifact on [{},{},{}]. {}", moduleId, getName(), getType(), getDebugString(errors));
		virtualModules.add(artifact);
	}

	/**
	 * Returns whether virtual modules have been resolved or not.
	 *
	 * @return {@code true} if virtual modules have been resolved. Otherwise {@code false}
	 */
	public boolean isResolvedVirtualModules() {
		return resolvedVirtualModules;
	}

	/**
	 * @return the {@link SourceMetrics} that has been resolved from database or {@code null}, if it has not been resolved yet
	 * or doesn't exists.
	 */
	@Nullable
	public SourceMetrics getResolvedSourceMetrics() {
		return sourceMetrics;
	}

	/**
	 * Returns the lazy loaded {@link SourceMetrics} of this {@link ModelArtifact}.
	 * <p>If the {@link SourceMetrics} wasn't resolved yet then the database is queried for it.</p>
	 * 
	 * @return the {@link SourceMetrics} or {@code null}
	 */
	@Nullable
	@Override
	public SourceMetrics getSourceMetrics() {
		if ( ! resolvedSourceMetrics) {
			modelArtifactDao.findOneSourceMetrics(Objects.requireNonNull(moduleId)).ifPresent(this::setSourceMetrics);
		}

		return sourceMetrics;
	}

	/**
	 * Sets the source metrics of the model artifact.
	 *
	 * @param sourceMetrics source metrics {@link SourceMetrics} of model artifact
	 * @return the Model artifact instance
	 */
	@Override
	public ModelArtifact setSourceMetrics(final SourceMetrics sourceMetrics) {
		resolvedSourceMetrics = true; /* avoids unnecessary DB access */
		return super.setSourceMetrics(sourceMetrics);
	}
	
	private static <T> Stream<T> toStream(final Collection<T> resolved, final Collection<T> added) {
		if (added.isEmpty()) {
			return resolved.isEmpty() ? Stream.empty() : resolved.stream();
		}
		
		return Stream.concat(resolved.stream(), added.stream());
	}
	
	/*
	 * Set decorator that knows when it has been modified.
	 */
	@NonNullByDefault(false)
	private static class ModifyAwareSet<T extends Object> implements Set<T> {
		
		private final Set<T> decorated;
		private boolean modified;
		
		private ModifyAwareSet(final Set<T> decorated) {
			this.decorated = decorated;
		}

		@Override
		public int size() {
			return decorated.size();
		}

		@Override
		public boolean isEmpty() {
			return decorated.isEmpty();
		}

		@Override
		public boolean contains(final Object o) {
			return decorated.contains(o);
		}

		@Override
		public Iterator<T> iterator() {
			return decorated.iterator();
		}

		@Override
		public Object[] toArray() {
			return decorated.toArray();
		}

		@Override
		public <X> X[] toArray(final X[] a) {
			return decorated.toArray(a);
		}

		@Override
		public boolean add(final T e) {
			modified = true;
			return decorated.add(e);
		}

		@Override
		public boolean remove(final Object o) {
			modified = true;
			return decorated.remove(o);
		}

		@Override
		public boolean containsAll(final Collection<?> c) {
			return decorated.containsAll(c);
		}

		@Override
		public boolean addAll(final Collection<? extends T> c) {
			modified = true;
			return decorated.addAll(c);
		}

		@Override
		public boolean retainAll(final Collection<?> c) {
			modified = true;
			return decorated.retainAll(c);
		}

		@Override
		public boolean removeAll(final Collection<?> c) {
			modified = true;
			return decorated.removeAll(c);
		}

		@Override
		public void clear() {
			modified = true;
			decorated.clear();
		}

		@Override
		public String toString() {
			return decorated.toString();
		}
	}

}
