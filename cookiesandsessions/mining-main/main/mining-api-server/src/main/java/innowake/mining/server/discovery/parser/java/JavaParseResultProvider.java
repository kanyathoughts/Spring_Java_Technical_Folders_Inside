/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.java;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.collections4.set.ListOrderedSet;
import org.apache.commons.io.FilenameUtils;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.dom.ASTParser;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.dawn.metrics.contributors.java.JavaContributor;
import innowake.mining.server.discovery.parser.AbstractCachingParseResultProvider;
import innowake.mining.server.discovery.parser.CancellableParserTask;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.parsing.parser.java.JavaParserAst;
import innowake.ndt.parsing.parser.java.JavaParserConfiguration;
import innowake.ndt.parsing.parser.java.JavaParserConfiguration.JavaParserConfigurationBuilder;
import innowake.ndt.parsing.parser.java.model.JavaModel;

/**
 * Provides Java parser results.
 */
public class JavaParseResultProvider extends AbstractCachingParseResultProvider<JavaModel> {

	/** The error message of the {@link DiscoveryException} when validation of source folder failed */
	static final String ERROR_PARSING_FAILED = "Parsing of source file %s failed for the following reason(s): %s";

	private static final Logger LOG = LoggerFactory.getLogger(Logging.JAVA_PARSER);

	private final String[] includedPackages;
	private final String[] excludedPackages;
	private final Map<String, Boolean> cachedFilterPackages = new HashMap<>();
	private final SearchOrders searchOrders;
	private final String sourcePath;
	private static final Pattern PATTERN_ANY_ALPHABETS_NUMBERS = Pattern.compile(".*[A-Za-z0-9]+.*");

	/**
	 * Constructor.
	 *
	 * @param config The active discovery {@link Config}; not {@code null}
	 * @param worker The {@link TimedWorker} for the execution of the parsing; not {@code null}
	 * @param sourcePath the root folder into which source files are exported; not {@code null}
	 * @param jobId The job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService}
	 * @param searchOrders The {@link SearchOrders} of the project
	 */
	public JavaParseResultProvider(final Config config, final TimedWorker worker, final String sourcePath, final String jobId,
			final ParseResultCacheService parseResultCacheService, final SearchOrders searchOrders) {
		super(worker, config.getParserTimeout(ResolveTarget.JAVA), jobId, parseResultCacheService);
		this.searchOrders = searchOrders;
		this.sourcePath = sourcePath;
		includedPackages = config.getJavaIncludePackages();
		excludedPackages = config.getJavaExcludePackages();
	}

	@Override
	public JavaModel getParseResult(final SourcePojo sourceObject) throws DiscoveryException {
		try {
			return getParseResult(sourceObject, MessageProvider.from(sourceObject, ResolveTarget.JAVA), new CancellableParserTask<>() {

				@Override
				public JavaModel call() throws DiscoveryException {
					final SearchOrderPaths searchOrderPaths = getSearchOrderPaths(sourceObject.getPath());
					final JavaParserConfiguration<SourcePojo> javaConfig = new JavaParserConfigurationBuilder<SourcePojo>()
							.setSourcePaths(searchOrderPaths.getSourcePaths())
							.setAssemblingDataProvider(new JavaDataProvider())
							.build();
					javaConfig.setModuleName(sourceObject.getName());
					javaConfig.setParserProgressMonitor(this);

					return new JavaParserAst<>(javaConfig)
									.parse(sourceObject)
									.orElseThrow(() -> new DiscoveryException("Unable to parse the content in the path " + sourceObject.getPath()));
				}
			});
		} catch (final WorkerCancellationException exception) {
			throw new DiscoveryException(exception.getMessage(), exception);
		}
	}

	/**
	 * Returns whether or not the given {@code packageName} is filtered according to the Java package configuration (include & exclude) and therefore has to be
	 * collected as a resource  or dependency ({@linkplain JavaContributor}).
	 *
	 * @param packageName the package name to check; not {@code null}
	 * @return {@code true} if the package is filtered, else {@code false}.
	 */
	public boolean isIncludedPackage(final String packageName) {
		return cachedFilterPackages.computeIfAbsent(packageName, key -> {
			/* An empty array means that all packages of the source folder should be included in the discovery. When a pattern is specified then only matching
			 * packages and all compilation units, whose package declaration matches with the include pattern, are included by the discovery */
			final boolean includes = includedPackages.length == 0 || Arrays.stream(includedPackages).anyMatch(name -> FilenameUtils.wildcardMatch(key, name));

			/* An empty array means that no packages are excluded in the discovery. When a pattern is specified then all matching packages and all compilation
			 * units, whose package declaration matches with the exclude pattern, are excluded by the discovery. */
			final boolean excludes = excludedPackages.length != 0 && Arrays.stream(excludedPackages).anyMatch(name -> FilenameUtils.wildcardMatch(key, name));

			/* The exclude filter takes precedence means that if a package matches with both the include and exclude filters then it (and all compilation units
			 * of that package) are excluded by the discovery. */
			return Boolean.valueOf(includes && ! excludes);
		}).booleanValue();
	}

	/**
	 * Returns if any include packages were specified in the discovery configuration or not.
	 *
	 * @return {{@code true} if include packages were specified. Otherwise {@code false}
	 */
	public boolean hasIncludePackages() {
		return includedPackages.length != 0;
	}

	/**
	 * Creates a new {@link ASTParser} with the given {@code sourcePaths} 
	 *
	 * @param sourcePaths The source paths; not {@code null}
	 * @return the {@link ASTParser}; not {@code null}
	 */
	public static ASTParser newParser(final String... sourcePaths) {

		final JavaParserConfiguration<SourcePojo> javaConfig = new JavaParserConfigurationBuilder<SourcePojo>()
				.setSourcePaths(sourcePaths)
				.setAssemblingDataProvider(new JavaDataProvider())
				.build();

		final ASTParser jdtParser = ASTParser.newParser(javaConfig.getApiLevel());
		jdtParser.setKind(ASTParser.K_COMPILATION_UNIT);
		jdtParser.setResolveBindings(true);

		/* See innowake.mining.server.discovery.metrics.java.JavaDependencyVisitorTest.testAnnotationInEnum1()
		 * With parser.setBindingsRecovery(true); The type of the @Nullable annotation would be returned as
		 * innowake.mining.server.discovery.metrics.java.annotation.Nullable instead of null */
		jdtParser.setBindingsRecovery(false);

		/* Must set sourcepath entries so JDT does binding resolving and we need the same number of sourcepath entries for the encoding */
		final String[] encodings = new String[sourcePaths.length];
		/* SourceExportTask uses the same Charset for source export */
		Arrays.fill(encodings, "UTF-8");
		jdtParser.setEnvironment(null, sourcePaths, encodings, true);

		/* When parser.setSource(String) is used, you have to specify compiler options explicitly using setCompilerOptions(Map)
		 * as 1.5 code and higher will not be properly parsed without setting the appropriate values for the compiler options */
		final Map<String, String> options = JavaCore.getOptions();
		JavaCore.setComplianceOptions(javaConfig.getComplianceOption(), options);
		jdtParser.setCompilerOptions(options);

		return jdtParser;
	}
	
	private SearchOrderPaths getSearchOrderPaths(final String path) {
		final String[] pathPatterns = searchOrders.resolvePattern(path);
		final SearchOrderPaths searchOrderPaths = new SearchOrderPaths(sourcePath);
		for (final String pathPattern : pathPatterns) {
			final int pos = pathPattern.indexOf('*');
			if (pos != -1) {
				for (int i = pos; i >= 0; i--) {
					final char c = pathPattern.charAt(i);
					if ( ! (c == '/' || c == '\\' || c == '.')) {
						final String fixedPath = pathPattern.substring(0, i);
						if (PATTERN_ANY_ALPHABETS_NUMBERS.matcher(pathPattern.substring(pos + 1)).matches()) {
							LOG.warn(() -> "Invalid target pattern : " + pathPattern + ". Using the pattern : " + fixedPath);
						}
						searchOrderPaths.add(fixedPath, true);
						break;
					}
				}
			} else {
				searchOrderPaths.add(pathPattern, true);
			}
		}

		searchOrderPaths.add("src/java", false);

		return searchOrderPaths;
	}

	private static class SearchOrderPaths {

		private final Set<String> searchPaths = new ListOrderedSet<>();
		private final String sourcePath;

		private SearchOrderPaths(final String sourcePath) {
			this.sourcePath = sourcePath;
		}

		private void add(final String pathPattern, final boolean testIfExists) {
			final Path path = Paths.get(sourcePath, pathPattern).toAbsolutePath();

			if ( ! testIfExists || Files.isDirectory(path)) {
				if ( ! searchPaths.add(path.toString())) {
					LOG.info(() -> "Skipped duplicate target pattern: " + path.toString());
				}
			} else {
				LOG.warn(() -> "Skipped non existing Java Source Directory: " + path.toString());
			}
		}

		private String[] getSourcePaths() {
			return searchPaths.toArray(new String[0]);
		}
	}
}
