/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.DB_ACCESS_OPERATION;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.DB_ACCESS_TYPE;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.FILE_ACCESS_TYPE;
import static innowake.mining.server.discovery.metrics.natural.NaturalProgrammingMode.REPORTING;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_ADAPTER;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_ADAPTVIEW;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_CLASS;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_COPYCODE;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_COPYCODE_REPORTING;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_CPM;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_DDM;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_DIALOG;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_DIALOG_PRIV_RES;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_ERROR_MESSAGE;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_FUNCTION;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_FUNCTION_REPORTING;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_GDA;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_HELP;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_HELP_REPORTING;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_LDA;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_MAP;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_MAP_REPORTING;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_PDA;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_PROGRAM;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_PROGRAM_REPORTING;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_SUBPROGRAM;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_SUBPROGRAM_REPORTING;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_SUBROUTINE;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_SUBROUTINE_REPORTING;
import static innowake.mining.shared.model.discovery.ResolveTarget.NATURAL_TEXT;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.io.FilenameUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.IModuleRepository;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.data.model.discovery.ModelDependency;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.FileAccess;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.metrics.MetricsUtility;
import innowake.mining.server.discovery.metrics.natural.NaturalDbAccessCollector.AccessType;
import innowake.mining.server.discovery.parser.batch.JclParseResultProvider;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Collects outgoing Natural dependencies for a module. Supports value resolving of variables and constants.
 * <p>
 * <b>Data areas:</b>
 * <pre>
 * GLOBAL USING GDA1
 * LOCAL USING LDA1
 * LOCAL USING PDA1
 * </pre>
 * <p>
 * <b>DDMs:</b>
 * <pre>
 * DEFINE DATA LOCAL
 *  1 #VIEW VIEW EMPLOYEES
 *   2 NAME
 * END-DEFINE
 * </pre>
 * <p>
 * <b>Programs:</b>
 * <pre>
 * FETCH 'MAIN2'
 * FETCH #PROG1
 * </pre>
 * <p>
 * <b>Subprograms:</b>
 * <pre>
 * CALLNAT 'SUB1'
 * </pre>
 * <p>
 * <b>(External) Subroutines:</b>
 * <pre>
 * PERFORM SUBR1
 * </pre>
 * <p>
 * <b>Maps:</b>
 * <pre>
 * INPUT USING MAP 'MAP1'
 * </pre>
 * <p>
 * <b>Helproutines:</b>
 * <pre>
 * INPUT #PROG1 (HE='HELPR1')
 * </pre>
 * <p>
 * <b>Copycodes:</b>
 * <pre>
 * INCLUDE CC1
 * </pre>
 * <p>
 * <b>External Objects:</b>
 * <pre>
 * CALL 'PROG1'
 * </pre>
 */
class NaturalDependencyCollector {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);

	private final IModuleRepository repo;
	private final ModelArtifact entry;
	private final SourcePojo sourceObject;
	private final List<NaturalDependency> outgoing;
	private final FieldValueResolver fieldValueResolver;
	private final NaturalDbAccessCollector dbCollector;
	private final NaturalWorkfileCollector workfileCollector;
	private final SourceObjectResolver sourceObjectResolver;
	private final NaturalSourceObjectManager sourceObjectManager;
	private final Config config;

	NaturalDependencyCollector(final IModuleRepository repo,
			final JclParseResultProvider jclParseResultProvider,
			final ModelArtifact entry,
			final SourcePojo sourceObject,
			final SourceObjectResolver sourceObjectResolver,
			final NaturalSourceObjectManager sourceObjectManager,
			final NaturalParseResultProvider naturalParserResultProvider,
			final Config config) {

		this.repo = repo;
		this.entry = entry;
		this.sourceObject = sourceObject;
		this.sourceObjectResolver = sourceObjectResolver;
		this.sourceObjectManager = sourceObjectManager;
		this.config = config;
		/* dependency calculation is lazy, so cache outgoings */
		outgoing = sourceObjectManager.getOutgoingDependencies(sourceObject);
		fieldValueResolver = new FieldValueResolver(sourceObject, naturalParserResultProvider);
		dbCollector = new NaturalDbAccessCollector(sourceObject, naturalParserResultProvider);
		workfileCollector = new NaturalWorkfileCollector(repo, entry, sourceObject, jclParseResultProvider, naturalParserResultProvider);
	}

	/**
	 * Available bindings are dependencies to objects which can be resolved by natclipse.
	 * <p>
	 * <pre>
	 * FETCH 'MAIN2'
	 * </pre>
	 *
	 * @return available bindings
	 */
	List<ModelDependency> collectAvailableBindings() {
		return outgoing.stream()
				.filter(NaturalDependency::targetExists)
				.flatMap(this::createAvailable)
				.collect(Collectors.toList());
	}

	/**
	 * Unavailable bindings are dependencies to objects which are basically supported by natclipse but cannot be resolved because natclipse does not support
	 * value resolving of variables and constants.
	 * <p>
	 * <pre>
	 * FETCH #PROG1
	 * </pre>
	 *
	 * @return unavailable bindings
	 */
	List<ModelDependency> collectUnavailableBindings() {
		return outgoing.stream()
				.filter(dependency -> ! dependency.targetExists())
				.map(this::createUnavailable)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.collect(Collectors.toList());
	}

	/**
	 * Additional bindings are dependencies to objects which are not supported by natclipse.
	 * <p>
	 * <pre>
	 * CALL 'PROG1'
	 * </pre>
	 *
	 * @return additional bindings
	 */
	List<ModelDependency> collectAdditionalBindings() throws DiscoveryException {
		return new LightweightParser(sourceObject, fieldValueResolver, sourceObjectResolver)
				.getOutgoingCalls()
				.stream()
				.map(this::createAdditional)
				.collect(Collectors.toList());
	}

	/**
	 * Collects all database access bindings.
	 */
	@SuppressWarnings("unchecked")
	void collectDatabaseBindings() throws DiscoveryException {
		final MultiValuedMap<String, AccessType> ddms = dbCollector.doCollect();

		ddms.asMap().entrySet().forEach(e -> {
			final String ddmName = e.getKey();
			final Optional<ModelArtifact> ddm = repo.getEntry(entry, ddmName, ResolveTarget.NATURAL_DDM);
			if (ddm.isPresent()) {
				final ModelDependency dependency = new ModelDependency().setTarget(ddm.get()).setBinding(Binding.EARLY).validate();

				final List<ModelDependency> dependencies = entry.getDependencies()
						.filter(d -> d.getTarget().equals(dependency.getTarget()))
						.collect(Collectors.toList());
				if ( ! dependencies.isEmpty()) {
					/* add to already existing dependencies */
					dependencies.stream().forEach(d -> {
						List<DatabaseAccessType> accessTypes = (List<DatabaseAccessType>) d.getAttribute(DB_ACCESS_TYPE);
						if (accessTypes == null) {
							accessTypes = new ArrayList<>();
							d.addAttribute(DB_ACCESS_TYPE, accessTypes);
						}
						addDbAccessTypes(accessTypes, (List<AccessType>) e.getValue());

						/* add additional information only if available */
						List<String> readStatements = (List<String>) d.getAttribute(DB_ACCESS_OPERATION);
						if (readStatements == null) {
							readStatements = new ArrayList<>();
						}
						addReadStatements(readStatements, (List<AccessType>) e.getValue());
						if (!readStatements.isEmpty()) {
							d.addAttribute(DB_ACCESS_OPERATION, readStatements);
						}
					});
				} else {
					/* add as new dependency */
					final List<DatabaseAccessType> accessTypes = new ArrayList<>();
					addDbAccessTypes(accessTypes, (List<AccessType>) e.getValue());
					dependency.addAttribute(DB_ACCESS_TYPE, accessTypes);

					/* add additional information only if available */
					final List<String> readStatements = new ArrayList<>();
					addReadStatements(readStatements, (List<AccessType>) e.getValue());
					if (!readStatements.isEmpty()) {
						dependency.addAttribute(DB_ACCESS_OPERATION, readStatements);
					}

					entry.addDependency(dependency);
				}
			}
		});
	}

	/**
	 * Collects all workfile access bindings.
	 */
	@SuppressWarnings("unchecked")
	void collectWorkfileBindings() throws DiscoveryException {
		final MultiValuedMap<String, FileAccess> workfiles = workfileCollector.doCollect();

		workfiles.asMap().entrySet().forEach(e -> {
			final String workfileName = e.getKey();
			final ModelArtifact resource = repo.getEntry(entry, workfileName, ResolveTarget.RESOURCE_FILE)
					.orElse(new ModelArtifact().setName(workfileName).setType(ResolveTarget.RESOURCE_FILE).validate());
			final ModelDependency dependency = new ModelDependency().setTarget(resource).setBinding(Binding.EARLY).validate();

			final List<ModelDependency> dependencies = entry.getDependencies().filter(d -> d.getTarget().equals(dependency.getTarget()))
					.collect(Collectors.toList());
			if ( ! dependencies.isEmpty()) {
				/* add to already existing dependencies */
				dependencies.stream().forEach(d -> {
					List<FileAccess> accessTypes = (List<FileAccess>) d.getAttribute(FILE_ACCESS_TYPE);
					if (accessTypes == null) {
						accessTypes = new ArrayList<>();
						d.addAttribute(FILE_ACCESS_TYPE, accessTypes);
					}
					MetricsUtility.mergeList(accessTypes, (List<FileAccess>) e.getValue());
				});
			} else {
				/* add as new dependency */
				dependency.addAttribute(ModelAttributeKey.FILE_ACCESS_TYPE, e.getValue());
				entry.addDependency(dependency);
			}
		});
	}

	private static void addDbAccessTypes(final List<DatabaseAccessType> accessTypes, final List<AccessType> naturalAccessTypes) {
		naturalAccessTypes.forEach(type -> {
			final DatabaseAccessType accessType;
			switch (type) {
				case DELETE:
					accessType = DatabaseAccessType.DELETE;
					break;
				case FIND:
				case GET:
				case HISTOGRAM:
				case READ:
					accessType = DatabaseAccessType.READ;
					break;
				case STORE:
					accessType = DatabaseAccessType.STORE;
					break;
				case UPDATE:
					accessType = DatabaseAccessType.UPDATE;
					break;
				default:
					throw new IllegalStateException("Unsupported type: " + type.name());
			}

			if ( ! accessTypes.contains(accessType)) {
				accessTypes.add(accessType);
			}
		});
	}

	private static void addReadStatements(final List<String> readStatements, final List<AccessType> naturalAccessTypes) {
		naturalAccessTypes.forEach(type -> {
			switch (type) {
				case FIND:
				case GET:
				case HISTOGRAM:
				case READ:
					if ( ! readStatements.contains(type.name())) {
						readStatements.add(type.name());
					}
					break;
				default:
					/* we're only interested in read types */
					break;
			}
		});
	}

	private Stream<ModelDependency> createAvailable(final NaturalDependency dependency) {
		return dependency.getTargets()
				.stream()
				.map(targetObject -> {
					ResolveTarget targetType;
					try {
						targetType = Mapper.mapType(sourceObjectManager, targetObject);
					} catch (final IllegalStateException e) {
						/* we know that we have a dependency to an unresolvable Natural object at least */
						LOG.error(String.format("[%s] %s", dependency.getTargetName(), "Unable to resolve actual type"), e);
						targetType = ResolveTarget.NATURAL;
					}
					/* resolve target with name, type and path (duplicated names allowed) */
					final Optional<ModelArtifact> targetArtifact = resolveTarget(targetObject.getName(), targetType, targetObject.getPath());
					return createBinding(targetArtifact, targetObject.getName(), targetType);
				});
	}

	private Optional<ModelDependency> createUnavailable(final NaturalDependency dependency) {
		String targetName; 
		if (dependency.isStaticBinding()) {
			targetName = dependency.getTargetName();
		} else {
			targetName = fieldValueResolver.resolve(dependency.getTargetName());
			if (targetName == null) {
				/* Creating a ModelError with the location of statement if the dependency target could not be determined */
				entry.addError(new ErrorMarker()
						.setErrorSeverity()
						.setKey(ErrorKey.UNDISCOVERED_DEPENDENCY)
						.setCause(String.format("The value of the call variable '%s' could not be determined and thus the dependency target is undiscovered",
								dependency.getTargetName()))
						.validate());
				return Optional.empty();
			}
		}

		final Optional<ModelArtifact> target = resolveTarget(targetName);
		final ResolveTarget targetType;
		if (target.isPresent()) {
			targetType = target.get().getType();
		} else if (dependency.getTargetType() != null) {
			targetType = Mapper.mapType(dependency);
		} else {
			targetType = ResolveTarget.NATURAL;
		}
		return Optional.of(createBinding(target, targetName, targetType));
	}

	private ModelDependency createAdditional(final String dependency) {
		final Optional<ModelArtifact> target = resolveTarget(dependency);
		final ResolveTarget targetType = target.isPresent() ? target.get().getType() : ResolveTarget.NONE;
		return createBinding(target, dependency, targetType);
	}

	private Optional<ModelArtifact> resolveTarget(final String targetName) {
		final List<ModelArtifact> entries = repo.getEntries(targetName);
		if (entries.isEmpty() && config.getUtilityList().isUtility(targetName)) {
			final ModelArtifact utilityArtifact = new ModelArtifact()
					.setName(targetName)
					.setOrigin(Origin.ENVIRONMENT)
					.setType(ResolveTarget.UTILITY)
					.validate();
			entries.add(utilityArtifact);
		}
		if (entries.size() != 1 && LOG.isErrorEnabled()) {
			final String message = (entries.isEmpty() ? "No entry" : "Multiple entries") + " found for object";
			LOG.error("[{}, {}] {} {}", sourceObject.getName(), sourceObject.getPath(), message, targetName);
		}
		return Optional.ofNullable(entries.size() == 1 ? entries.get(0) : null);
	}

	private Optional<ModelArtifact> resolveTarget(final String targetName, final ResolveTarget targetType, final String path) {
		return repo.getEntry(entry, targetName, path, targetType);
	}

	private static ModelDependency createBinding( final Optional<ModelArtifact> target,
			final String targetName,
			final ResolveTarget targetType ) {

		return new ModelDependency()
				.setTarget( target.orElse( new ModelArtifact()
						.setIdentification(Identification.MISSING)
						.setName(targetName)
						.setType(targetType)
						.validate()) )
				.setBinding(Mapper.mapBinding(targetType))
				.validate();
	}

	private static class Mapper {

		private static Binding mapBinding(final ResolveTarget targetType) {
			switch (targetType) {
				case NATURAL_LDA:
				case NATURAL_IW_LDA:
				case NATURAL_PDA:
				case NATURAL_IW_PDA:
				case NATURAL_GDA:
				case NATURAL_IW_GDA:
				case NATURAL_FUNCTION:
				case NATURAL_COPYCODE:
				case NATURAL_DDM:
					/* fall through intended */
					return Binding.EARLY;
				default:
					return Binding.LATE;
			}
		}

		private static ResolveTarget mapType(final NaturalDependency dependency) {
			final Type type = assertNotNull(dependency.getTargetType());
			switch (type) {
				case PROGRAM:
					return NATURAL_PROGRAM;
				case SUBROUTINE:
					return NATURAL_SUBROUTINE;
				case SUBPROGRAM:
					return NATURAL_SUBPROGRAM;
				case COPYCODE:
					return NATURAL_COPYCODE;
				case CLASS:
					return NATURAL_CLASS;
				case CPM:
					return NATURAL_CPM;
				case ADAPTVIEW:
					return NATURAL_ADAPTVIEW;
				case FUNCTION:
					return NATURAL_FUNCTION;
				case HELP:
					return NATURAL_HELP;
				case ADAPTER:
					return NATURAL_ADAPTER;
				case MAP:
					return NATURAL_MAP;
				case DIALOG:
					return NATURAL_DIALOG;
				case DIALOG_PRIV_RES:
					return NATURAL_DIALOG_PRIV_RES;
				case GDA:
					return NATURAL_GDA;
				case PDA:
					return NATURAL_PDA;
				case LDA:
					return NATURAL_LDA;
				case DDM:
					return NATURAL_DDM;
				case TEXT:
					return NATURAL_TEXT;
				case ERROR_MESSAGE:
					return NATURAL_ERROR_MESSAGE;
				default:
					throw new IllegalStateException("Unexpected Natural object type " + type.toString() + " for object " + dependency.getTargetName());
			}
		}

		private static ResolveTarget mapType(final NaturalSourceObjectManager sourceObjectManager, @Nullable final SourcePojo object) {
			if (object == null) {
				/* we know that we have a dependency to an unresolvable Natural object at least */
				return NATURAL;
			}

			final NaturalProgrammingMode mode = sourceObjectManager.getProgrammingMode(object);
			switch (object.getType()) {
				case PROGRAM:
					return mode == REPORTING ? NATURAL_PROGRAM_REPORTING : NATURAL_PROGRAM;
				case SUBROUTINE:
					return mode == REPORTING ? NATURAL_SUBROUTINE_REPORTING : NATURAL_SUBROUTINE;
				case SUBPROGRAM:
					return mode == REPORTING ? NATURAL_SUBPROGRAM_REPORTING : NATURAL_SUBPROGRAM;
				case COPYCODE:
					return mode == REPORTING ? NATURAL_COPYCODE_REPORTING : NATURAL_COPYCODE;
				case CLASS:
					return NATURAL_CLASS;
				case CPM:
					return NATURAL_CPM;
				case ADAPTVIEW:
					return NATURAL_ADAPTVIEW;
				case FUNCTION:
					return mode == REPORTING ? NATURAL_FUNCTION_REPORTING : NATURAL_FUNCTION;
				case HELP:
					return mode == REPORTING ? NATURAL_HELP_REPORTING : NATURAL_HELP;
				case ADAPTER:
					return NATURAL_ADAPTER;
				case MAP:
					return mode == REPORTING ? NATURAL_MAP_REPORTING : NATURAL_MAP;
				case DIALOG:
					return NATURAL_DIALOG;
				case DIALOG_PRIV_RES:
					return NATURAL_DIALOG_PRIV_RES;
				case GDA:
				case PDA:
				case LDA:
					final String extension = FilenameUtils.getExtension(object.getPath());
					return ResolveTargetHelper.getResolveTarget(extension, NATURAL)
							.orElseThrow(() -> new IllegalStateException(String.format("Unexpected extension '%s' for Natural object type %s for object %s",
									extension, object.getType().toString(), object.getPath())));
				case DDM:
					return NATURAL_DDM;
				case TEXT:
					return NATURAL_TEXT;
				case ERROR_MESSAGE:
					return NATURAL_ERROR_MESSAGE;
				default:
					throw new IllegalStateException("Unexpected Natural object type " + object.getType().toString() + " for object " + object.getPath());
			}
		}

	}

}
