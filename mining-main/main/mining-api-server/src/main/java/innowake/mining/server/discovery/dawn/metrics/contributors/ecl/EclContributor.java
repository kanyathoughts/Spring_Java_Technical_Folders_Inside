/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.ecl;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.GenericMetricsUtil;
import innowake.mining.server.discovery.dawn.metrics.contributors.InputProvider;
import innowake.mining.server.discovery.metrics.generic.GenericMetricsContributor;
import innowake.mining.server.discovery.metrics.generic.MetricException;
import innowake.mining.server.discovery.metrics.generic.MetricFactory;
import innowake.mining.server.discovery.metrics.generic.MetricType;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.parsing.parser.ecl.model.EclNode;
import innowake.ndt.parsing.parser.ecl.model.ast.AbstractFileAccessControlStatement;
import innowake.ndt.parsing.parser.ecl.model.ast.CustomControlStatement;
import innowake.ndt.parsing.parser.ecl.model.ast.EclAddStatement;
import innowake.ndt.parsing.parser.ecl.model.ast.EclControlStatement;
import innowake.ndt.parsing.parser.ecl.model.ast.EclFile;
import innowake.ndt.parsing.scanner.ecl.EclLexerFactory;

/**
 * Contributor for ECL files.
 */
@Component
public class EclContributor implements DiscoveryContributorFromSource {
	
	private static final Logger LOG = LoggerFactory.getLogger(EclContributor.class);
	private static final Pattern COBOL_PROGRAM_NAME_PATTERN = Pattern.compile("MAINPROGRAM\\s(\\w+)");

	@Autowired
	private ParserProviderService parserProvider;
	
	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return  sourceObject.getTechnology() == Technology.ECL && sourceObject.getType() == Type.ECL_JOB;
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context,
			final SourcePojo sourceObject) {
		final GenericMetricsContributor metricsContributor = new GenericMetricsContributor(new InputProvider(sourceObject, EclLexerFactory.get()));
		metricsContributor.enable(MetricFactory.get(MetricType.LOC_LOC));
		
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.ECL_JOB);
		
		try {
			rootModule.addAdditionalInfo(GenericMetricsUtil.executeAndGetResults(metricsContributor));
		} catch (final MetricException e) {
			LOG.error("Error while calculating metrics", e);
			rootModule.addError(Severity.ERROR, ErrorKey.METRICS_CALCULATION_ERROR, e.getMessage());
		}
		
		final Optional<AstNode> root;
		try {
			root = parserProvider.createEclParser(context).getParseResult(sourceObject).getRoot();
		} catch (final DiscoveryException e) {
			LOG.error("Error while parsing " + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
			/* unable to parse so nothing more to discover */
			return;
		}
		
		if (root.isPresent()) {
			final EclNode rootNode = (EclNode)root.get();
			collectCobolDependencies(rootModule, rootNode);
			collectUtilityDependencies(builder, rootModule, rootNode);
			collectFileDependencies(builder, rootModule, rootNode);
		}
	}

	/**
	 * Collects dependencies to Cobol program from ECL modules.
	 *
	 * @param rootModule the root module of type {@link ModuleBuilder}
	 * @param rootNode rootNode of ECL Program
	 */
	private void collectCobolDependencies(final ModuleBuilder rootModule, final EclNode rootNode) {
		rootNode.getChildrenDeep(EclAddStatement.class).stream()
				.filter(statement -> COBOL_PROGRAM_NAME_PATTERN.matcher(statement.getData()).find())
				.forEach(statement -> {
					final Matcher matcher = COBOL_PROGRAM_NAME_PATTERN.matcher(statement.getData());
					if (matcher.find()) {
						final String programName = matcher.group(1);
						final ModuleFilter moduleFilter = new ModuleFilter().setNames(programName)
								.setTypes(ModuleType.COBOL_PROGRAM);
						rootModule.declareDependency(RelationshipType.CALLS, moduleFilter)
								  .setBinding(Binding.EARLY)
								  .setLocation(new ModuleLocation(statement.getStartOffset(), statement.getLength()));
					} else {
						LOG.error("Cobol program doesn't exist");
						rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, "Cobol program doesn't exist");
					}
				});
	}

	/**
	 * Collects dependencies for custom control statements from ECL modules as utilities.
	 *
	 * @param builder {@link DiscoveryBuilderFromSource}
	 * @param rootModule the root module of type {@link ModuleBuilder}
	 * @param rootNode rootNode of ECL Program
	 */
	private void collectUtilityDependencies(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule, final EclNode rootNode) {
		final Set<String> utilities = new HashSet<>();
		rootNode.getChildrenDeep(EclControlStatement.class).stream()
				.filter(CustomControlStatement.class::isInstance)
				.map(EclControlStatement::getMnemonic)
				.filter(utilities::add)
				.forEach(utilityName -> {
					final ModuleBuilder externalModule = builder.declareExternalModule(utilityName, ModuleType.UNKNOWN_UTILITY);
					rootModule.declareDependency(RelationshipType.CALLS, externalModule)
							  .setBinding(Binding.EARLY);
				});
	}

	/**
	 * Collects dependencies for file access statements from ECL modules.
	 *
	 * @param builder {@link DiscoveryBuilderFromSource}
	 * @param rootModule the root module of type {@link ModuleBuilder}
	 * @param rootNode rootNode of ECL Program
	 */
	private void collectFileDependencies(final DiscoveryBuilderFromSource builder, final ModuleBuilder rootModule, final EclNode rootNode) {
		final Set<String> files = new HashSet<>();
		rootNode.getChildrenDeep(AbstractFileAccessControlStatement.class)
				/* gets all the file names */
				.forEach(statement -> statement.getEclFiles().stream()
						.map(EclFile::getResolvedFileName)
						.filter(StringUtils::isNotEmpty)
						.filter(files::add)
						.forEach(fileItem -> {
							final ModuleBuilder externalModule = builder.declareExternalModule(fileItem, ModuleType.RESOURCE_FILE);
							rootModule.declareDependency(RelationshipType.ACCESSES, externalModule)
									.setBinding(Binding.EARLY)
									.setLocation(new ModuleLocation(statement.getStartOffset(), statement.getLength()));
						}));
	}
}
