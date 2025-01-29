/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.java;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.model.ModuleType.JAVA_ANNOTATION;
import static innowake.mining.shared.model.ModuleType.JAVA_COMPILATION_UNIT;
import static innowake.mining.shared.model.ModuleType.JAVA_ENUM;
import static innowake.mining.shared.model.ModuleType.JAVA_INTERFACE;
import static innowake.mining.shared.model.ModuleType.JAVA_PACKAGE;
import static innowake.mining.shared.model.ModuleType.JAVA_TYPE;
import static innowake.mining.shared.model.RelationshipType.CALLS;
import static innowake.mining.shared.model.RelationshipType.REFERENCES;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.mining.server.discovery.dawn.metrics.contributors.AstNodeLocationProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.ndt.core.parsing.spi.Document;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.TypeReferenceType;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.AstInputProvider;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.java.JavaAstVisitor.Dependency;
import innowake.mining.server.discovery.dawn.metrics.contributors.java.JavaAstVisitor.Dependency.DependencyType;
import innowake.mining.server.discovery.dawn.metrics.contributors.java.JavaAstVisitor.JavaType;
import innowake.mining.server.discovery.dawn.metrics.contributors.java.JavaAstVisitor.MethodCall;
import innowake.mining.server.discovery.dawn.metrics.contributors.java.JavaAstVisitor.StoredProcedureInvocation;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.metrics.generic.input.InputType;
import innowake.mining.server.discovery.metrics.generic.input.MetricInputProvider;
import innowake.mining.server.discovery.metrics.generic.loc.CustomRegionsLocMetric;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.java.JavaParseResultProvider;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.java.JavaLexerFactory;
import innowake.ndt.core.parsing.java.JavaRegionCategory;
import innowake.ndt.parsing.parser.java.model.JavaCompilationUnit;

/**
 * Contributor for Java source files.
 */
@Component
public class JavaContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(JavaContributor.class);
	private static final String JAVA_PACKAGES_CACHE_KEY = "JavaContributor_JAVA_PACKAGES_CACHE_KEY";
	private static final String PATH_SEPARATOR = "/";
	private static final String DEFAULT_PATTERN = "**/*";
	private static final String JAVA_SOURCE = "src/java";
	private static final String INVALID_SEARCH_ORDER = "Invalid Search Order configuration: Source patterns must not start with path separator '/' for : %s";
	private static final String NO_MATCHING_SEARCH_ORDER = "No Matching search order configuration found for : %s";
	private static final String INVALID_FOLDER = "Java file is located in the wrong folder Or source pattern doesn't match with the path and package. ";
	private static final String NO_CONFIG_FOUND_FOR_JAVA_SOURCE = "File located in %s : %s has no matching search order ";

	@Autowired
	private ParserProviderService parserProvider;

	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private DiscoveryCache discoveryCache;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		/* JSF and JSP files are currently not supported by this contributor. Same for Java pom.xml files which have the type UNKNOWN */
		return sourceObject.getTechnology() == Technology.JAVA && sourceObject.getType() == Type.COMPILATION_UNIT;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final var collectMethods = context.getFeatureMap().get(FeatureId.JAVA_COLLECT_METHOD_CALLS).booleanValue();
		final var parseResultProvider = parserProvider.createJavaParser(context);
		try {
			final var javaModel = parseResultProvider.getParseResult(sourceObject);
			final var root = javaModel.getRoot();
			if (root.isEmpty()) {
				LOG.error(() -> "Error while parsing " + sourceObject.getPath() + ": No root found.");
			} else {
				final var declaration = ((CompilationUnit) ((JavaCompilationUnit) root.get()).getJdtAstNode()).getPackage();
				final var packageName = declaration != null ? declaration.getName().toString() : "";

				if (parseResultProvider.isIncludedPackage(packageName)) {
					/* packages are external modules, need one per package. For now we don't link Java packages with the containing Java Compilation Units */
					if ( ! packageName.isEmpty()) {
						builder.declareExternalModule(packageName, JAVA_PACKAGE);
					}
					final var rootModule = builder.declareRootModule(sourceObject.getName(), JAVA_COMPILATION_UNIT);
					validateSearchOrder(context, sourceObject, packageName, rootModule);

					collectSourceMetrics(rootModule, sourceObject, javaModel);

					final var visitor = new JavaAstVisitor(collectMethods, sourceObject.getContent().toString());
					assertNotNull(((JavaCompilationUnit) root.get()).getJdtAstNode()).accept(visitor);

					collectSubModules(builder, visitor, rootModule);
				} else {
					LOG.debug(() -> String.format("Skipping resource collection for %s as it is filtered by Java package", sourceObject.getName()));
				}
			}
		} catch (final DiscoveryException e) {
			LOG.error(() -> "Error while parsing " + sourceObject.getPath(), e);
			LOG.debug(() -> ExceptionUtils.getFullStackTrace(e));
		} catch (final Exception e) {
			LOG.error("Exception occured while parsing" + sourceObject.getPath(), e);
		} catch (final Throwable e) {
			LOG.error("Unxpected error occured while parsing" + sourceObject.getPath(), e);
		}
	}

	private static void collectSourceMetrics(final ModuleBuilder rootModule, final SourcePojo sourceObject, final AstModel astModel) {
		try {
			final var iLexerFactory = JavaLexerFactory.get();
			final MetricInputProvider inputProvider = new AstInputProvider(sourceObject, iLexerFactory, astModel);
			final var metricsContributor =  new GenericMetricsContributor(inputProvider);
			metricsContributor.enable(new CustomRegionsLocMetric(InputType.TOKEN, JavaRegionCategory.CATEGORY_COMMENT, JavaRegionCategory.CATEGORY_CODE));
			metricsContributor.enable(MetricFactory.get(MetricType.MCCABE_COMPLEXITY));
			rootModule.addAdditionalInfo(GenericMetricsUtil.executeAndGetResults(metricsContributor));
		} catch (final MetricException e) {
			LOG.error("Error while calculating metrics", e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
	}

	private void collectSubModules(final DiscoveryBuilderFromSource builder, final JavaAstVisitor visitor, final ModuleBuilder rootModule) {
		final var javaTypes = visitor.getJavaTypes();
		if (javaTypes.isEmpty()) {
			return;
		}
		final var storedProcedures = visitor.getStoredProcedureInvocations();
		final var dependencies = visitor.getDependencies();
		final var methodCalls = visitor.getMethodCalls();

		for (final var javaType : javaTypes) {
			final var dependencyCandidates = new ArrayList<JavaCandidate>(dependencies.size() + methodCalls.size());
			final var moduleLocation = new ModuleLocation(javaType.getOffset(), javaType.getLength());
			final var subModuleBuilder = builder.declareSubModule(javaType.getFullyQualifiedName(), javaType.getType(), moduleLocation);

			if ( ! storedProcedures.isEmpty()) {
				storedProcedures.stream()
						.filter(storedProcedure -> moduleLocation.contains(storedProcedure.getOffset(), storedProcedure.getLength()))
						.forEach(storedProcedure -> addStoredProcedureDependency(subModuleBuilder, storedProcedure, visitor.getDocument()));
			}
			if ( ! methodCalls.isEmpty() && javaType.getType() == ModuleType.JAVA_METHOD) {
				addMethodCallDependencyCandidates(javaType, methodCalls, dependencyCandidates);
			}

			if ( ! dependencies.isEmpty()) {
				final var name = javaType.getFullyQualifiedName();
				/* ArrayList implements Serializable */
				dependencies.stream()
						/* filter references whose parent type does not match to prevent findings for outer classes when there is a finding for an inner classes */
						.filter(dep -> name.equals(dep.getParentName()) && moduleLocation.contains(dep.getOffset(), dep.getLength()))
						.map(JavaCandidate::of)
						.forEach(dependencyCandidates::add);

				if ( ! dependencyCandidates.isEmpty()) {
					subModuleBuilder.deferAction("collectDependencies", dependencyCandidates);
				}
			}
		}
		if ( ! methodCalls.isEmpty()) {
			/* Add the errors to the rootModule for the method calls which were not resolved */
			final ArrayList<JavaCandidate> methodCallErrors = methodCalls.stream().map(methodCall -> new JavaCandidate(methodCall.getCalleeMethod(),
					methodCall.getPackageName(),	null,
					StringUtils.EMPTY, methodCall.getOffset(), methodCall.getLength(), null, null ))
					.collect(Collectors.toCollection(ArrayList::new));
			rootModule.deferAction("addErrors", methodCallErrors);
		}
		/* Add the errors collected by visitor */
		visitor.getErrors().forEach(error -> rootModule.addError(Severity.ERROR, ErrorKey.UNDISCOVERED_DEPENDENCY, error.e1, error.e2));
	}

	private static void addMethodCallDependencyCandidates(final JavaType javaType,
			final List<MethodCall> methodCalls, final ArrayList<JavaCandidate> dependencyCandidates) {
		final String callerMethodName = javaType.getFullyQualifiedName();
		for (final Iterator<MethodCall> it = methodCalls.iterator(); it.hasNext();) {
			final var methodCall = it.next();
			if (callerMethodName.equals(methodCall.getCallerMethod())) {
				final var dependencyCandidate = new JavaCandidate(methodCall.getCalleeMethod(), methodCall.getPackageName(), DependencyType.REFERENCE,
						StringUtils.EMPTY, methodCall.getOffset(), methodCall.getLength(), ModuleType.JAVA_METHOD, methodCall.getCalleeMethodParameters());
				dependencyCandidates.add(dependencyCandidate);
				it.remove();
			}
		}
	}

	private void validateSearchOrder(final DiscoveryContext context, final SourcePojo sourceObject, final String packageName, final ModuleBuilder rootModule) {
		final String sourcePattern = getSourcePattern(sourceObject, context);
		/* Checks for the case when source Pattern starts with '/'. e.g. '/src/java/WMIN3202/pro3' */
		if (sourcePattern.startsWith(PATH_SEPARATOR)) {
			rootModule.addError(Severity.WARNING, ErrorKey.INVALID_SEARCH_ORDER_CONFIGURATION, String.format(INVALID_SEARCH_ORDER, sourcePattern));
			return;
		}

		if (sourcePattern.isEmpty()) {
			rootModule.addError(Severity.WARNING, ErrorKey.INVALID_SEARCH_ORDER_CONFIGURATION, String.format(NO_MATCHING_SEARCH_ORDER,
					sourceObject.getPath()));
			return;
		}

		if ( ! sourcePattern.isEmpty() && ! packageName.isEmpty()) {
			validatePathAndPackage(sourceObject, packageName, rootModule, sourcePattern);
		}
	}

	private void validatePathAndPackage(final SourcePojo sourceObject, final String packageName, final ModuleBuilder rootModule, final String sourcePattern) {
		final int index = sourcePattern.indexOf("/**/");
		final String path = sourceObject.getPath();
		final String sourcePath = (index != -1) ? sourcePattern.substring(0, index) : sourcePattern;
		final String replacedPackageName = packageName.replace(".", PATH_SEPARATOR);
		final String combinedPath = truncatePathEndingWithSlash((sourcePath.equals(DEFAULT_PATTERN) || sourcePath.equals(JAVA_SOURCE)) ?
				sourcePattern + PATH_SEPARATOR
				+ replacedPackageName : sourcePath + PATH_SEPARATOR + replacedPackageName);
		final String pathWithoutFileName = truncatePathEndingWithSlash(path.substring(0, path.lastIndexOf(PATH_SEPARATOR) + 1));
		/* A java file is located in src/java and has a package but no matching search order was found. No error is added if there is no package present. */
		checkIfConfigurationPresentForJavaSrc(packageName, rootModule, path, sourcePath, pathWithoutFileName);
		/* Checks if the file is in the right package. e.g. Folder is /src/java/de/iw/Test.java but the package in Test.java is de.iw.util */
		checkPackageName(rootModule, sourcePath, combinedPath, pathWithoutFileName, replacedPackageName);
		/* Checks if source pattern does not match with the path and package of the java source file. e.g.
		 * <source pattern="src/java/project1/"/> instead of
		 * <source pattern="src/java/project1/src/main/java/"/> */
		checkCombinedPath(sourceObject, rootModule, path, sourcePath, replacedPackageName);
	}

	private void checkCombinedPath(final SourcePojo sourceObject, final ModuleBuilder rootModule, final String path, final String sourcePath,
			final String replacedPackageName) {
		if (path.contains(replacedPackageName) && path.substring(0, path.indexOf(replacedPackageName))
				.equals(sourcePath.equals(DEFAULT_PATTERN) ? "" : sourcePath)) {
			rootModule.addError(Severity.WARNING, ErrorKey.INVALID_SEARCH_ORDER_CONFIGURATION, String.format(NO_MATCHING_SEARCH_ORDER,
					sourceObject.getPath()));
		}
	}

	private void checkPackageName(final ModuleBuilder rootModule, final String sourcePath, final String combinedPath, final String pathWithoutFileName,
			final String packageName) {
		if (((sourcePath.equals(DEFAULT_PATTERN) || sourcePath.equals(JAVA_SOURCE)) && ( ! packageName.isEmpty()
				&& ! pathWithoutFileName.contains(packageName))) || ( ! combinedPath.equals(pathWithoutFileName) && ! sourcePath.equals(DEFAULT_PATTERN)
						&& ! sourcePath.equals(JAVA_SOURCE))) {
			if (sourcePath.equals(DEFAULT_PATTERN)) {
				rootModule.addError(Severity.WARNING, ErrorKey.INVALID_SEARCH_ORDER_CONFIGURATION, String.format(INVALID_FOLDER + "Actual Package: %s, "
						+ "But was: %s", combinedPath, pathWithoutFileName));
			} else {
				rootModule.addError(Severity.WARNING, ErrorKey.INVALID_SEARCH_ORDER_CONFIGURATION, String.format(INVALID_FOLDER + "Actual: %s, Expected: %s",
						combinedPath, pathWithoutFileName));
			}
		}
	}

	private void checkIfConfigurationPresentForJavaSrc(final String packageName, final ModuleBuilder rootModule, final String path, final String sourcePath,
			final String pathWithoutFileName) {
		if ((pathWithoutFileName.equals(JAVA_SOURCE) && ! sourcePath.equals(pathWithoutFileName) ) && ! packageName.isEmpty()) {
			rootModule.addError(Severity.WARNING, ErrorKey.INVALID_SEARCH_ORDER_CONFIGURATION, String.format(NO_CONFIG_FOUND_FOR_JAVA_SOURCE, JAVA_SOURCE,
					path));
		}
	}

	private String truncatePathEndingWithSlash(final String path) {
		return path.endsWith(PATH_SEPARATOR) ? path.substring(0, path.length() - 1) : path;
	}

	private String getSourcePattern(final SourcePojo sourceObject, final DiscoveryContext context) {
		final Optional<SearchOrder> searchOrder = context.getSearchOrders().findMatchingSearchOrder(PATH_SEPARATOR + sourceObject.getPath());
		return searchOrder.isPresent() ? searchOrder.get().getSourcePattern().orElse(StringUtils.EMPTY) : StringUtils.EMPTY;
	}

	private static void addStoredProcedureDependency(final ModuleBuilder virtualModule, final StoredProcedureInvocation invocation, final Document document) {
		final var storedProcedure = invocation.getStoredProcedure();
		if (storedProcedure.isPresent()) {
			DawnMetricsUtility.declareDependencyToSqlStoredProcedureWithSchema(virtualModule, storedProcedure.get(), ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
					.setLocation(new ModuleLocation(invocation.getOffset(), invocation.getLength()));
		} else {
			final var offset = invocation.getOffset();
			final var length = invocation.getLength();
			final var lineNumbers = AstNodeLocationProvider.getLineNumbers(document, offset, length);
			final var location= new AstNodeLocation(offset, length, offset, length, offset, length, lineNumbers.e1, lineNumbers.e2);
			virtualModule.addError(Severity.WARNING,
									ErrorKey.PARSE_ERROR,
									String.format("Unable to examine name of Stored Procedure for method invocation: %s", invocation.getCode()),
									location);
		}
	}

	@DeferredAction
	public void collectDependencies(final ModuleBuilder moduleBuilder, final ModulePojo module, final DiscoveryContext context,
			final ArrayList<JavaCandidate> dependencies) {
		final var provider = parserProvider.createJavaParser(context);
		final var location = module.getLocation().orElse(null);
		if (location != null) {
			final var name = module.getName();
			try {
				@SuppressWarnings("unchecked")
				final var packages = (Set<String>) discoveryCache.computeValueIfAbsent(context.getJobId(), JAVA_PACKAGES_CACHE_KEY,
						() -> getJavaPackages(context));

				dependencies.forEach(dependency -> {
					if (dependency.moduleType == ModuleType.JAVA_METHOD) {
						declareMethodCallDependency(moduleBuilder, dependency, provider, packages);
					} else {
						/* Create all other valid dependencies */
						declareDependency(moduleBuilder, dependency, location, name, packages, provider);
					}
				});
			} catch (final Exception e) {
				LOG.error("Error while collecting dependencies for Java module: {}, {} ", module.getName(), module.getId(), e);
				moduleBuilder.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
			}
		}
	}

	private static void declareDependency(final ModuleBuilder moduleBuilder, final JavaCandidate dependency, final ModuleLocation location,
	final String name, final Set<String> packages, final JavaParseResultProvider provider) {
		if ( ! filterByDependency(location, name, dependency, packages, provider)) {
			return;
		}
		final var moduleFilter = new ModuleFilter()
				.setNames(dependency.name)
				.setTypes(JAVA_INTERFACE, JAVA_TYPE, JAVA_ANNOTATION, JAVA_ENUM);
		final var dependencyBuilder = moduleBuilder.declareDependency(REFERENCES, moduleFilter)
				.setBinding(Binding.EARLY)
				.setLocation(new ModuleLocation(dependency.offset, dependency.length))
				.createIfMissing(dependency.name, JAVA_TYPE, Identification.MISSING);

		switch (dependency.type) {
			case EXTEND:
				dependencyBuilder.addAttribute(ModelAttributeKey.TYPE_REFERENCE_TYPE, TypeReferenceType.EXTEND);
				break;
			case IMPLEMENT:
				dependencyBuilder.addAttribute(ModelAttributeKey.TYPE_REFERENCE_TYPE, TypeReferenceType.IMPLEMENT);
				break;
			default:
				break;
		}
	}

	private static void declareMethodCallDependency(final ModuleBuilder moduleBuilder, final JavaCandidate dependency, final JavaParseResultProvider provider,
			final Set<String> packages) {
		/* collect as java_method dependencies */
		if ( ! filterByPackage(provider, dependency, packages)) {
			return;
		}
		final var moduleFilter = new ModuleFilter()
				.setNames(dependency.name)
				.setTypes(ModuleType.JAVA_METHOD);
		final var dependencyBuilder = moduleBuilder.declareDependency(CALLS, moduleFilter)
				.setBinding(Binding.LATE)
				.setLocation(new ModuleLocation(dependency.offset, dependency.length));
		if (dependency.parameters != null && assertNotNull(dependency.parameters).length != 0) {
			dependencyBuilder.addAttribute(ModelAttributeKey.PARAMETER_TYPES, Arrays.asList(dependency.parameters));
		}
	}

	@DeferredAction()
	public void addErrors(final ModuleBuilder moduleBuilder, final DiscoveryContext context,
			final ArrayList<JavaCandidate> errors) {
		final var provider = parserProvider.createJavaParser(context);
		try {
			@SuppressWarnings("unchecked")
			final var packages = (Set<String>) discoveryCache.computeValueIfAbsent(context.getJobId(), JAVA_PACKAGES_CACHE_KEY, () -> getJavaPackages(context));

			errors.stream()
					.filter(error -> filterByPackage(provider, error, packages))
								.forEach(error -> moduleBuilder.addError(Severity.ERROR, ErrorKey.UNDISCOVERED_DEPENDENCY,
						String.format("Unable to create dependency for method invocation. Method invocation: %s", error.name)));
		} catch (final  Exception e) {
			moduleBuilder.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, String.format("Error while adding errors to the Java module. %s", e.getMessage()));
		}
	}

	private Set<String> getJavaPackages(final DiscoveryContext context) {
		final var pckges = new HashSet<>(moduleService.findNames(q -> q.ofProject(context.getProjectId()).withTechnology(Technology.JAVA).withType(Type.PACKAGE)));
		/* add default package */
		pckges.add("");
		return pckges;
	}

	private static boolean filterByDependency(final ModuleLocation location, final String name, final JavaCandidate dependency, final Set<String> packages,
												final JavaParseResultProvider provider) {

		/* filter references whose parent type does not match to prevent findings for outer classes when there is a finding for one of its inner classes */
		return name.equals(dependency.parentName) && location.contains(dependency.offset, dependency.length) &&
				/* Keep only compilation units that is not filtered due to the Java include and exclude packages configuration and if no Java include packages 
				 * were specified, are in the already collected java packages of the project */
				filterByPackage(provider, dependency, packages);
	}

	private static boolean filterByPackage(final JavaParseResultProvider provider, final JavaCandidate candidate, final Set<String> packages) {
		return provider.isIncludedPackage(candidate.packageName) && (provider.hasIncludePackages() || packages.contains(candidate.packageName));
	}


	private static class JavaCandidate implements Serializable {

		private final DependencyType type;
		private final String name;
		private final String parentName;
		private final String packageName;
		private final int offset;
		private final int length;

		@Nullable
		private final ModuleType moduleType;
		@Nullable
		private final String[] parameters;

		private JavaCandidate(final String name, final String packageName, @Nullable final DependencyType type, final String parentName, final int offset,
				final int length, @Nullable final ModuleType moduleType, @Nullable final String[] parameters) {
			this.type = type;
			this.name = name;
			this.parentName = parentName;
			this.packageName = packageName;
			this.offset = offset;
			this.length = length;
			this.moduleType = moduleType;
			this.parameters = parameters;
		}

		private static JavaCandidate of(final Dependency dependency) {
			return new JavaCandidate(dependency.getFullyQualifiedName(), dependency.getPackageName(), dependency.getType(),
											  dependency.getParentName(), dependency.getOffset(), dependency.getLength(), null, null);
		}
	}
}
