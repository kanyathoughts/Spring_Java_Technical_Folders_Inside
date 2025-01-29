/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.dcl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.server.discovery.metrics.MetricsUtility;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.dcl.DCLParseResultProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.IToken;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.parsing.parser.dcl.model.DclModel;
import innowake.ndt.parsing.parser.dcl.model.ast.CommandProcedure;
import innowake.ndt.parsing.parser.dcl.model.ast.FileSpecification;
import innowake.ndt.parsing.parser.dcl.model.ast.command.BaseCommand;
import innowake.ndt.parsing.parser.dcl.model.ast.command.IncludeProcedureCommand;
import innowake.ndt.parsing.parser.dcl.model.ast.command.RunCommand;

/**
 * Contributor for dawn DCL.
 */
@Component
public class DCLContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(DCLContributor.class);

	private static final String DCL_COMMENT = "$!";

	/**
	 * The splitter is used on these types of commands after parsing:<br>
	 *     "RUN/NODEBUG EXE:CAP_BAL_BRM_08.EXE"<br>
	 * and splits <br>
	 *     "EXE:CAP_BAL_BRM_08.EXE"<br>
	 * into <br>
	 * 	   "EXE" and "CAP_BAL_BRM_08.EXE".
	 */
	private static final Pattern COLON_SPLITTER = Pattern.compile(":");

	/**
	 * Strips away the file ending, e.g. from "CAP_BAL_BRM_08.EXE".
	 */
	private static final Pattern FILE_ENDING_SPLITTER = Pattern.compile("\\.");

	/**
	 * Helper for finding a program name in e.g. '$ ENIP100_RUN :== "$EXE:ENIP100"'.
	 */
	private static final String EXE_MARKER = "$EXE:";

	/**
	 * Fixed keywords for @COM
	 */
	private enum RESERVED_KEYWORD {
		INITIALIZATION, TERMINATION, BATCH_ERROR
	}

	@Autowired
	private ParserProviderService parserProvider;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return (sourceObject.getTechnology() == Technology.VMS && sourceObject.getType() == Type.DCL);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final DCLParseResultProvider dclParseResultProvider = parserProvider.createDclParser(context.getConfig(), context.getTimedWorker(),
				context.getSearchOrders(), context.getJobId());
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.DCL);
		final Optional<DclModel> dclModel;
		try {
			DawnMetricsUtility.collectLinesOfCode(sourceObject, rootModule, DCL_COMMENT);
			dclModel = dclParseResultProvider.getParseResult(sourceObject);
		} catch (final DiscoveryException e) {
			LOG.error("Error while parsing DCL file " + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
			/* unable to parse so nothing more to discover */
			return;
		} catch (final Exception e) {
			LOG.error("Exception occured while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
			return;
		} catch (final Throwable e) {
			LOG.error("Unxpected error occured while parsing" + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.MODULE_ABORT, e.getMessage());
			return;
		}

		if (dclModel.isEmpty() ||  dclModel.get().getRoot().isEmpty()) {
			LOG.error("Root not found for the DCL file " + sourceObject.getPath());
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, "Root not found for the DCL file " + sourceObject.getPath());
			return;
		}

		final CommandProcedure commandProcedure = dclModel.get().getCommandProcedure();
		addFileDependencies(commandProcedure, rootModule, sourceObject.getName(), builder);
		addDCLDependenciesToEntry(commandProcedure, rootModule);
	}

	private static void addFileDependencies(final CommandProcedure commandProcedure, final ModuleBuilder rootModule, final String sourceObjectName,
			final DiscoveryBuilderFromSource builder) {
		/* extract all RUN commands */
		commandProcedure.getChildrenDeep(BaseCommand.class).stream()
			.filter(element -> element.getName().equalsIgnoreCase("DEFINE") || element.getName().equalsIgnoreCase("DEF")).forEach(element -> {
				final List<String> strTokens = element.getTokens().stream().map(t -> t.getText().toString()).collect(Collectors.toList());
				/* We have more than 2 tokens and the second token is not '/', like in DEFINE/NOLOG. */
				if (strTokens.size() > 2 && !strTokens.get(1).equals("/")) {
					String physicalFileName = String.join("", strTokens.subList(2, strTokens.size()));

					/* Remove any quotes. */
					physicalFileName = StringUtils.strip(physicalFileName, "'\"");

					/* Strip away any comment behind the filename */
					final int commentIndex = physicalFileName.indexOf('!');
					physicalFileName = commentIndex == -1 ? physicalFileName : physicalFileName.substring(0, commentIndex);

					final String logicalFileName = strTokens.get(1);
					if ( ! isValidFileDefinition(physicalFileName, logicalFileName)) {
						return;
					}
					
					final ModuleBuilder externalModule = builder.declareExternalModule(physicalFileName, ModuleType.RESOURCE_FILE);

					rootModule
						.declareDependency(RelationshipType.ACCESSES, externalModule)
						.setBinding(Binding.EARLY)
						.addAttribute(ModelAttributeKey.FILE_ALIAS, logicalFileName.toUpperCase())
						.setLocation(new ModuleLocation(element.getStartOffset(), element.getLength()));
				} else {
					LOG.warn("UNKNOWN DEFINE constallation in DCL->File resolving in file: " + sourceObjectName);
				}
			});
	}

	private static boolean isValidFileDefinition(final String physicalFileName, final String logicalFileName) {
		if (physicalFileName.isEmpty() || logicalFileName.isEmpty()) {
			return false;
		}
		/* Check if physicalName is a number */
		if (physicalFileName.chars().allMatch(Character::isDigit)) {
			return false;
		}
				
		return ! logicalFileName.contains("$");
	}

	private static void addDCLDependenciesToEntry(final CommandProcedure commandProcedure, final ModuleBuilder rootModule) {
		/* extract all RUN commands */
		commandProcedure.getChildrenDeep(RunCommand.class)
		  .stream()
		  .map(RunCommand::getFileSpecification)
		  .forEach(element -> extractRunCommands(element, rootModule));

		/* extract all IncludeProcedure commands referencing a file spec */
		commandProcedure.getChildrenDeep(IncludeProcedureCommand.class)
		  .stream()
		  .map(IncludeProcedureCommand::getInclude)
		  .filter(FileSpecification.class::isInstance)
		  .map(FileSpecification.class::cast)
		  .forEach(element -> extractInclProc(element, rootModule));

		extractSecondaryRunCommands(commandProcedure, rootModule);
	}

	private static void extractRunCommands(final FileSpecification fileSpecification, final ModuleBuilder rootModule) {
		final String[] runEntities = COLON_SPLITTER.split(fileSpecification.getFileName());
		if (runEntities.length > 1) {
			final String runEntity = runEntities[1];
			final String runWithoutEnding = FILE_ENDING_SPLITTER.split(runEntity)[0];
			final ModuleFilter moduleFilter = new ModuleFilter().setNames(runWithoutEnding).setTypes(ModuleType.COBOL_PROGRAM, ModuleType.BASIC_PROGRAM);

			rootModule
				.declareDependency(RelationshipType.CALLS, moduleFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
				.setBinding(Binding.EARLY)
				.setLocation(new ModuleLocation(fileSpecification.getStartOffset(), fileSpecification.getLength()));
		}
	}

	private static void extractInclProc(final FileSpecification fileSpecification, final ModuleBuilder rootModule) {
		final String[] inclProcEntities = COLON_SPLITTER.split(fileSpecification.getFileName());
		if (inclProcEntities.length > 1 && "COM".equalsIgnoreCase(inclProcEntities[0])) {
			final String searchCOM = FILE_ENDING_SPLITTER.split(inclProcEntities[1])[0];
			for (final RESERVED_KEYWORD keyword : RESERVED_KEYWORD.values()) {
				if (keyword.name().equalsIgnoreCase(searchCOM)) {
					/* Continue with next element. */
					return;
				}
			}
			final ModuleFilter moduleFilter = new ModuleFilter().setNames(searchCOM).setTypes(ModuleType.DCL);

			rootModule
				.declareDependency(RelationshipType.CALLS, moduleFilter)
				.setBinding(Binding.EARLY)
				.setLocation(new ModuleLocation(fileSpecification.getStartOffset(), fileSpecification.getLength()));
		}
	}

	private static void extractSecondaryRunCommands(final AstNode commandProcedure, final ModuleBuilder rootModule) {
		final List<BaseCommand> baseCommands = commandProcedure.getChildrenDeep(BaseCommand.class);
		final Map<String, String> runMap = new HashMap<>();
		baseCommands.forEach(cmd -> {
			final List<IToken> tokens = cmd.getTokens();
			/* Check if the current token is a run-command. */
			if ( ! tokens.isEmpty() && runMap.containsKey(tokens.get(0).getText().toString())) {
				final String runText = tokens.get(0).getText().toString();

				/* We found a RUN instruction.*/
				final String runProgram = runMap.get(runText);
				final ModuleFilter moduleFilter = new ModuleFilter().setNames(runProgram).setTypes(ModuleType.COBOL_PROGRAM, ModuleType.BASIC_PROGRAM);

				rootModule
					.declareDependency(RelationshipType.CALLS, moduleFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
					.setBinding(Binding.LATE)
					.setLocation(new ModuleLocation(cmd.getStartOffset(), cmd.getLength()));
				/* Only report finding once. */
				runMap.remove(runText);
			}

			/* Analyze each set of command-tokens for '"$EXE:'*/
			tokens.forEach(token -> {
				final String tokenText = MetricsUtility.trimQuotesSpaces(token.getText().toString().trim().toUpperCase());
				if (tokenText.startsWith(EXE_MARKER)) {
					String program = MetricsUtility.trimQuotesSpaces(COLON_SPLITTER.split(tokenText)[1]);
					if (program.contains(".")) {
						program = FILE_ENDING_SPLITTER.split(program)[0];
					}
					runMap.put(tokens.get(0).getText().toString(), program);
				}
			});
		});
	}
}