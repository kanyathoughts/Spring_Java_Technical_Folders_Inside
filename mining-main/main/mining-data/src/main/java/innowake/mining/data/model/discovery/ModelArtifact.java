/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.model.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;

import com.google.common.base.MoreObjects;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * The base model artifact that should represent every module/entry/file/etc that is found by discovery.
 * The intention is for all discovery projects to use this as the base data model and extend as necessary.
 */
public class ModelArtifact implements Serializable {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.DISCOVERY_MODULE_REPO);
	
	@Nullable
	protected EntityId moduleId;

	@Nullable protected String name;

	@Nullable protected ResolveTarget type;

	@Nullable protected Representation representation;
	
	protected Identification identification = Identification.MISSING;
	@Nullable Origin origin = Origin.CUSTOM;
	
	@Nullable protected String path;
	
	/* Path of the parent module which contains this module */
	@Nullable
	private String containsPath;
	
	protected int statementCount;
	
	protected List<ModelDependency> dependencies;

	protected List<ErrorMarker> errors;

	protected List<ModelStatement> statements;
	
	protected List<ModelSqlStatement> sqlStatements;
	
	protected List<ModelDeadCode> deadCode;
	
	protected Map<String, Object> metadata;
	
	@Nullable protected ModuleLocation location;
	
	protected Set<ModelArtifact> virtualModules;
	
	/* Below members are not used locally but are needed for serialization */
	@Nullable private ResolveTarget language;

	@Nullable protected ModelArtifact parentModule;
	
	@Nullable 
	protected SourceMetrics sourceMetrics;
	
	public ModelArtifact() {
		moduleId = null;
		name = "";

		dependencies = new ArrayList<>();
		statements = new ArrayList<>();
		errors = new ArrayList<>();
		sqlStatements = new ArrayList<>();
		deadCode = new ArrayList<>();
		virtualModules = new HashSet<>();
		parentModule = null;
		metadata = new HashMap<>();
	}
	
	public ModelArtifact(final ModelArtifact artifact) {
		artifact.validate();
		
		this.moduleId = artifact.moduleId;
		this.name = artifact.name;
		this.type = artifact.type;
		this.representation = artifact.representation;
		this.identification = artifact.identification;
		this.origin = artifact.origin;
		this.language = artifact.language;
		
		this.path = artifact.path;
		this.containsPath = artifact.containsPath;
		
		this.dependencies = artifact.dependencies;
		this.errors = artifact.errors;
		this.statements = artifact.statements;
		this.sqlStatements = artifact.sqlStatements;
		
		this.deadCode = artifact.deadCode;
		this.virtualModules = artifact.virtualModules;

		this.location = artifact.location;
		this.parentModule = artifact.parentModule;
		this.metadata = artifact.metadata;
		this.sourceMetrics = artifact.sourceMetrics;
	}
	
	protected void reset() {
		moduleId = null;
		name = "";
		type = null;
		representation = null;
		identification = Identification.MISSING;
		origin = null;
		path = null;
		
		dependencies = new ArrayList<>();
		statements = new ArrayList<>();
		errors = new ArrayList<>();
		sqlStatements = new ArrayList<>();
		
		deadCode = new ArrayList<>();
		virtualModules = new HashSet<>();

		parentModule = null;
		metadata = new HashMap<>();
		sourceMetrics = null;
	}

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				.omitNullValues()
				.add("moduleId", moduleId)
				.add("name", name)
				.add("type", type)
				.add("representation", representation)
				.add("identification", identification)
				.add("origin", origin)
				.add("path", path)
				.add("parentModule", parentModule)
				.add("metadata", metadata)
				.toString();

	}
	
	@Override
	public int hashCode() {
		final String tmpName = name != null ? name.toUpperCase() : "NULL";
		return Objects.hash(moduleId, tmpName, type, representation, identification, path);
	}
	
	@Override
	public boolean equals(@Nullable final Object object) {
		if (object == this) {
			return true;
		}
		if ( ! (object instanceof ModelArtifact)) {
			return false;
		}
		
		final ModelArtifact other = (ModelArtifact) object;
		return Objects.equals(moduleId, other.moduleId) &&
				type == other.type && 
				representation == other.representation &&
				identification == other.identification &&
				origin == other.origin &&
				StringUtils.equalsIgnoreCase(name, other.name) &&
				Objects.equals(path, other.path) &&
				Objects.equals(parentModule, other.parentModule);
	}
	
	/*--- GETTERS ---*/

	/**
	 * @return the {@link EntityId} of this {@link ModelArtifact}
	 */
	@Nullable
	public EntityId getModuleId() {
		return moduleId;
	}
	
	
	/**
	 * Returns the name of the artifact.
	 *
	 * @return name of artifact
	 */
	public String getName() {
		validate();
		return assertNotNull(name, "Name must not be NULL");
	}

	/**
	 * Returns the name of the artifact without any validation.
	 * <p>This method should be called only for logging purpose and such where validation is not required!</p>
	 *
	 * @return name of artifact
	 */
	@Nullable
	public String getNameUnchecked() {
		return name;
	}
	
	/**
	 * 
	 * Return language: enum {@link ResolveTarget}.
	 * Note: type id() should be used in place of enum.name() to get the display name of target type.
	 *
	 * @return language type of artifact
	 */
	public ResolveTarget getLanguage() {
		return assertNotNull(getType().getLanguage(), "Language must not be NULL");
	}
	
	/**
	 * Returns the type of the artifact. 
	 * Note: type id() should be used in place of enum.name() to get the display name of target type.
	 *
	 * @return type of artifact
	 */
	public ResolveTarget getType() {
		validate();
		return assertNotNull(type, "Type must not be NULL");
	}

	/**
	 * Returns the type of the artifact without any validation
	 * <p>This method should be called only for logging purpose and such where validation is not required!</p>
	 *
	 * @return name of artifact
	 */
	@Nullable
	public ResolveTarget getTypeUnchecked() {
		return type;
	}

	/**
	 * Return representation enum: {@link Representation}.
	 *
	 * @return Representation of artifact.
	 */
	public Representation getRepresentation() {
		validate();
		return assertNotNull(representation, "Representation must not be NULL");
	}
	
	/**
	 * Return identification enum {@link Identification} or null if not set.
	 *
	 * @return Identification of artifact.
	 */
	public  Identification getIdentification() {
		return identification;
	}

	/**
	 * @return the {@link Origin} of this artifact or {@code null} if unset
	 */
	@Nullable
	public Origin getOrigin() {
		return origin;
	}
	
	/**
	 * Returns the path of the artifact.
	 *
	 * @return path or Optional.empty().
	 */
	public Optional<String> getPath() {
		validate();
		return Optional.ofNullable(path);
	}
	
	/**
	 * Get the path of the module which contains the current module.
	 *
	 * @return the path of the module which contains the current module
	 */
	public Optional<String> getParentPath() {
		return containsPath == null ? Optional.empty() : Optional.of(containsPath);
	}

	
	/**
	 * Sets the path of the module which contains the current module.
	 *
	 * @param containsPath the path of the module which contains the current module
	 */
	public void setContainsPath(@Nullable final String containsPath) {
		this.containsPath = containsPath;
	}
	
	/**
	 * @return the complexity value by McCabe.
	 */
	public int getComplexity() {
		final SourceMetrics sm = getSourceMetrics();
		final Integer complexity = sm != null ? sm.getComplexityMcCabe() : null;
		return complexity != null ? complexity.intValue() : -1;
	}

	/**
	 * Returns the number of lines of code. If a line contain both, code and comment, it will be added to code.
	 *
	 * @return the number of lines with code.
	 */
	public int getLinesOfCode() {
		final SourceMetrics sm = getSourceMetrics();
		final Integer linesOfCode = sm != null ? sm.getCodeLines() : null;
		return linesOfCode != null ? linesOfCode.intValue() : -1;
	}

	/**
	 * Get the number of lines with comments. If a line contain both, code and comment, it will be added to code.
	 *
	 * @return the number of lines with comments.
	 */
	public int getLinesOfComments() {
		final SourceMetrics sm = getSourceMetrics();
		final Integer linesOfComment = sm != null ? sm.getCommentLines() : null;
		return linesOfComment != null ? linesOfComment.intValue() : -1;
	}

	/**
	 * @return the parent {@link ModelArtifact} of this module.
	 */
	@Nullable
	public ModelArtifact getParentModule() {
		return parentModule;
	}
	
	/**
	 * Returns a new stream of the dependencies (i.e. references) this artifact has to other artifacts.
	 *
	 * @return ModelDependency objects as described above
	 */
	public Stream<ModelDependency> getDependencies() {
		return dependencies.stream();
	}
	
	/** 
	 * Returns a read-only unmodifiable {@code Set} of the virtual modules.
	 * <p>
	 * If you want to add virtual modules, either use {@link #addVirtualModule(ModelArtifact)} or {@link #setVirtualModules(Set)}.
	 * 
	 * @return the virtual modules for the given artifact
	 */
	public Set<ModelArtifact> getVirtualModules() {
		return Collections.unmodifiableSet(virtualModules);
	}
	
	/**
	 * Adds a virtual module to this artifact.
	 *
	 * @param virtualModule the virtual module to add
	 * @return a reference to the artifact for chaining
	 */
	public ModelArtifact addVirtualModule(final ModelArtifact virtualModule) {
		virtualModules.add(virtualModule);
		virtualModule.setParentModule(this);
		return this;
	}
	
	/**
	 * Sets the virtual modules for the artifact.
	 * 
	 * @param virtualModules to be set
	 * @return reference to this artifact
	 */
	public ModelArtifact setVirtualModules( final Set<ModelArtifact> virtualModules) {
		LOG.trace(() -> String.format("Setting virtual model artifacts on [%s,%s,%s]. %s", 
				moduleId, getName(), getType(), getDebugArtifactDetails(virtualModules)));
		this.virtualModules = virtualModules;
		this.virtualModules.forEach(virtualModule -> virtualModule.setParentModule(this));
		return this;
	}
	
	/**
	 * Sets the parent module for the artifact.
	 *
	 * @param modelArtifact the parent module
	 * @return reference to this artifact
	 */
	public ModelArtifact setParentModule(final ModelArtifact modelArtifact) {
		parentModule = modelArtifact;
		return this;
	}

	/**
	 * Returns a new stream of all the errors associated with this artifact as a result of discovery
	 *
	 * @return all ModelError objects as described above
	 */
	public Stream<ErrorMarker> getErrors() {
		return errors.stream();
	}
	
	/**
	 * Returns a new stream of all the statements associated with this artifact
	 *
	 * @return all the ModelStatements as described above
	 */
	public Stream<ModelStatement> getStatements() {
		return statements.stream();
	}

	/**
	 * Returns a new stream of all the SQL statements associated with this artifact
	 *
	 * @return all the ModelSqlStatement as described above
	 */
	public Stream<ModelSqlStatement> getSqlStatements() {
		return sqlStatements.stream();
	}

	/**
	 * @return map of additional artifact metadata
	 */
	public Map<String, Object> getMetadata() {
		return metadata;
	}
	
	/*--- SETTER METHODS ---*/

	public ModelArtifact setModuleId(final EntityId moduleId) {
		this.moduleId = moduleId;
		return this;
	}

	/**
	 * Set the module name.
	 *
	 * @param name The name of the module. Must not be empty and is mandatory.
	 * @return This ModelArtifact instance.
	 */
	public ModelArtifact setName(final String name) {
		this.name = name;
		return this;
	}
	
	/**
	 * Set the workspace relative path to the module including the project name.
	 *
	 * @param path The path of the module. Must be set if representation 
	 * is {@link Representation#PHYSICAL}.
	 * @return The ModelArtifact instance.
	 */
	public ModelArtifact setPath(@Nullable final String path) {
		this.path = path;
		return this;
	}

	/**
	 * Set the type of the language. Example {@link ResolveTarget#COBOL_PROGRAM}; 
	 *
	 * @param type Set the type of the language. Mandatory setting.
	 * @return The ModelArtifact instance.
	 */
	public ModelArtifact setType(final ResolveTarget type) {
		this.type = type;
		
		return this;
	}
	
	/**
	 * Sets the module location of the model artifact.
	 *
	 * @param location of the model artifact, instance of {@link ModuleLocation}
	 * @return The Model artifact instance
	 */
	public ModelArtifact setLocation(final ModuleLocation location) {
		this.location = location;
		return this;
	}
	
	/**
	 * Returns the module location of the model artifact.
	 *
	 * @return The module location of the model artifact
	 */
	public Optional<ModuleLocation> getLocation() {
		return Optional.ofNullable(location);
	}
	
	/**
	 * Set the representation type {@link Representation#PHYSICAL} or {@link Representation#VIRTUAL}; 
	 *
	 * @param representation The representation. Mandatory setting.
	 * @return The ModelArtifact instance.
	 */
	public ModelArtifact setRepresentation(final Representation representation) {
		this.representation = representation;
		return this;
	}
	
	/**
	 * Set the identification type
	 *
	 * @param identification The identification. Mandatory setting.
	 * @return The ModelArtifact instance.
	 */
	public ModelArtifact setIdentification(final Identification identification) {
		this.identification = identification;
		
		return this;
	}

	/**
	 * Set the {@link Origin} type of this artifact
	 *
	 * @param origin The {@link Origin}. Mandatory setting.
	 * @return The ModelArtifact instance.
	 */
	public ModelArtifact setOrigin(final Origin origin) {
		this.origin = origin;
		return this;
	}
	
	/**
	 * Set the module complexity calculated by McCabe.
	 * Must be > -1.
	 *
	 * @param complexity the complexity value.
	 * @return The ModelArtifact instance.
	 */
	public ModelArtifact setComplexity(final int complexity) {
		getOrCreateSourceMetrics().setComplexityMcCabe(Integer.valueOf(complexity));
		return this;
	}

	/**
	 * Set the number of lines of code.
	 * Must be > -1.
	 *
	 * @param loc the number of lines of code.
	 * @return the ModelArtifact instance.
	 */
	public ModelArtifact setLinesOfCode(final int loc) {
		getOrCreateSourceMetrics().setCodeLines(Integer.valueOf(loc));
		return this;
	}
	
	/**
	 * Set the number of lines of comment.
	 * Must be > -1.
	 *
	 * @param linesOfComments the number of lines of comment.
	 * @return the ModelArtifact instance.
	 */
	public ModelArtifact setLinesOfComments(final int linesOfComments) {
		getOrCreateSourceMetrics().setCommentLines(Integer.valueOf(linesOfComments));
		return this;
	}

	/**
	 * Sets the metadata map for this artifact.
	 * 
	 * @param metadata the metadata map
	 * @return reference to this artifact
	 */
	public ModelArtifact setMetadata(final Map<String, Object> metadata) {
		this.metadata = metadata;
		return this;
	}
	
	/**
	 * Gets the source metrics {@link SourceMetrics}
	 *
	 * @return source metrics of model artifact
	 */
	@Nullable
	public SourceMetrics getSourceMetrics() {
		return sourceMetrics;
	}

	/**
	 * Sets the source metrics of the model artifact.
	 *
	 * @param sourceMetrics source metrics {@link SourceMetrics} of model artifact
	 * @return the Model artifact instance
	 */
	public ModelArtifact setSourceMetrics(final SourceMetrics sourceMetrics) {
		this.sourceMetrics = sourceMetrics;
		return this;
	}

	public ModelArtifact setFromArtifact( final ModelArtifact artifact ) {
		this.name = artifact.name;
		this.type = artifact.type;
		this.representation = artifact.representation;
		this.identification = artifact.identification;
		
		this.path = artifact.path;
		
		this.dependencies = artifact.dependencies;
		this.errors = artifact.errors;
		this.statements = artifact.statements;
		this.sqlStatements = artifact.sqlStatements;
		
		this.deadCode = artifact.deadCode;
		
		this.virtualModules = artifact.virtualModules;
		this.metadata = artifact.metadata;
		this.sourceMetrics = artifact.sourceMetrics;
		
		return this;
	}
	
	private void handleError(final String message) {
			throw new IllegalArgumentException("Error while creating Artifact " +
			/* we do not use uid as identification because the module won't exist in module collection */
					(StringUtils.isBlank(path) ? name : path) + ": " + message + ".");		
	}
	
	/*--- ADD TO LISTS METHODS ---*/
	
	/**
	 * Adds a dependency to the dependency list of this artifact.
	 *
	 * @param d the new dependency to add
	 */
	public void addDependency(final ModelDependency d) {
		LOG.trace(() -> String.format("Adding model dependency on [%s,%s,%s]. %s", moduleId, getName(), getType(), d.toString()));
		dependencies.add(d);
	}

	/**
	 * Adds the given list of new {@code dependencies} to the dependency list of this artifact.
	 * 
	 * @param dependencies to add
	 */
	public void addDependencies(final List<ModelDependency> dependencies) {
		LOG.trace(() -> String.format("Adding model dependencies on [%s,%s,%s]. %s", moduleId, getName(), getType(), getDebugString(errors)));
		this.dependencies.addAll(dependencies);
	}

	/**
	 * Adds a statement to the list of statements for the artifact
	 *
	 * @param s the new statement to add
	 *
	 * @return true if statement is added, else false is returned.
	 */
	public boolean addStatement(final ModelStatement s) {
		if ( ! statements.contains(s)) {
			statements.add(s);
			return true;
		}
		return false;
	}
	
	/**
	 * 
	 * Adds an SQL statement to the list of SQL statements for the artifact
	 *
	 * @param s the new SQL statement to add
	 */
	public void addSqlStatement(final ModelSqlStatement s) {
		sqlStatements.add(s);
	}
 	
	/**
	 * 
	 * Adds an error to the list of errors for the artifact
	 *
	 * @param e the new error to add
	 */
	public void addError(final ErrorMarker e) {
		LOG.trace(() -> String.format("Adding model error on [%s,%s,%s]. %s", moduleId, getName(), getType(), e.toString()));
		errors.add(e);
	}
	
	/**
	 * 
	 * Adds errors to the list of errors for the artifact
	 * @param errors to add
	 *
	 */
	public void addErrors(final List<ErrorMarker> errors) {
		LOG.trace(() -> String.format("Adding model errors on [%s,%s,%s]. %s", moduleId, getName(), getType(), getDebugString(errors)));
		this.errors.addAll(errors);
	}
	
	/**
	 * Adds additional metadata to this artifact.
	 * 
	 * @param name the name for the metadata
	 * @param value the metadata value
	 */
	public void addMetadata(final String name, final String value) {
		metadata.put(name, value);
	}

	public void addDeadCode(final ModelDeadCode d) {
		deadCode.add(d);
	}

	/**
	 * @return new stream of the {@link ModelDeadCode} of this artifact.
	 */
	public Stream<ModelDeadCode> getDeadCode() {
		return deadCode.stream();
	}

	public int getLinesOfDeadCode() {
		final SourceMetrics sm = getSourceMetrics();
		final Integer lines = sm != null ? sm.getDeadCodeLines() : null;
		return lines != null ? lines.intValue() : -1;
	}
	
	public final void setLinesOfDeadCode(final int linesOfDeadCode) {
		getOrCreateSourceMetrics().setDeadCodeLines(Integer.valueOf(linesOfDeadCode));
	}

	private SourceMetrics getOrCreateSourceMetrics() {
		SourceMetrics sm = getSourceMetrics();
		if (sm == null) {
			sm = new SourceMetrics();
			sourceMetrics = sm;
		}

		return sm;
	}

	/**
	 * Checks if the identification is MISSING.
	 * 
	 * @return {@code true} if this module is of type 'missing'
	 */
	public boolean isMissing() {
		return identification == Identification.MISSING;
	}

	/**
	 * 
	 * Used in conjunction with builder pattern, this validate method can
	 * be used to ensure the object has all the necessary members set and
	 * in addition will assign appropriate default values as needed.
	 * <br>
	 * Should only be used when all builder methods have been called and object
	 * is appropriately "built".
	 *
	 * @return a reference to this object
	 */
	
	public ModelArtifact validate() {
		if (StringUtils.isBlank(name)) {
			handleError("Name must be set");
		}
		if (type == null) {
			handleError("Type must be set");
		}
		if (StringUtils.isBlank(path) && representation == Representation.PHYSICAL) {
			handleError("Path must be set for physical modules");
		}
		if (representation == null) {
			if (StringUtils.isBlank(path)) {
				representation = Representation.VIRTUAL;
			} else {
				representation = Representation.PHYSICAL;
			}
		}	
		return this;
	}
	
	protected String getDebugString(final Collection<?> collection) {
		if (collection.isEmpty()) {
			return "empty collection";
		}
		
		final StringBuilder sb = new StringBuilder();
		for (final Object element : collection) {
			sb.append("\n\t ");
			sb.append(element.toString());
		}
		
		return sb.toString();
	}

	protected String getDebugArtifactDetails(final Collection<ModelArtifact> artifacts) {
		if (artifacts.isEmpty()) {
			return "empty collection";
		}
	
		final var sb = new StringBuilder();
		for (final ModelArtifact artifact : artifacts) {
			sb.append("\n\t [");
			sb.append(artifact.moduleId);
			sb.append(",");
			sb.append(artifact.getName());
			sb.append(",");
			sb.append(artifact.getType());
			sb.append("]");
		}
		
		return sb.toString();
	}

}
