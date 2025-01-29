/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.csd;
import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.PersistenceException;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Enums;
import com.google.common.io.Files;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.csd.CsdParseResultProvider;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.parsing.parser.csd.CsdConverter;
import innowake.ndt.parsing.parser.csd.CsdParserFactory.CsdParserType;
import innowake.ndt.parsing.parser.csd.model.Csd;
import innowake.ndt.parsing.parser.csd.model.Program;
import innowake.ndt.parsing.parser.csd.model.Transaction;

/**
 * Dawn contributor for CSD files
 */
@Component
public class CSDContributor implements DiscoveryContributorFromSource {

	private static final Logger LOG = LoggerFactory.getLogger(CSDContributor.class);

	@Autowired
	private SourceCachingService sourceService;
	
	@Autowired
	private ParserProviderService parserProvider;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return (sourceObject.getTechnology() == Technology.CSD && sourceObject.getType() == Type.LIST)
				|| (sourceObject.getTechnology() == Technology.CSD && sourceObject.getType() == Type.EXTRACT);
	}

	@Override
	public void contribute(DiscoveryBuilderFromSource builder, DiscoveryContext context, SourcePojo sourceObject) {
		final CsdParserType parserType = sourceObject.getType() == Type.LIST ? CsdParserType.LIST : CsdParserType.EXTRACT;
		final CsdParseResultProvider csdParserResultProvider = parserProvider.createCsdParser(context, parserType);

		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(),
				sourceObject.getType() == Type.LIST ? ModuleType.CSD_LIST : ModuleType.CSD_EXTRACT);

		final Csd csdModel;
		try {
			csdModel = csdParserResultProvider.getParseResult(sourceObject);
		} catch (DiscoveryException e) {
			LOG.error("Error while parsing " + sourceObject.getPath(), e);
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, e.getMessage());
			/* unable to parse so nothing more to discover */
			return;
		}

		/* collect dependency */
		final Set<String> programs = csdModel.getPrograms().stream().map(Program::getId).collect(Collectors.toSet());
		final Set<Transaction> transactions = csdModel.getTransactions().stream().filter(t -> programs.contains(t.getProgram())).collect(Collectors.toSet());

		/* map program name to resolve target type */
		final Map<String, ModuleType> programToResolveTargetType = new HashMap<>();
		csdModel.getPrograms().forEach(p -> programToResolveTargetType.put(p.getId(), getModuleTypeFromLanguageString(p.getLanguage())));

		for (final Transaction transaction : transactions) {
			final ModuleBuilder transactionModule = builder.declareSubModule(transaction.getId(), ModuleType.CSD_TRANSACTION);
			final ModuleType ModuleType = programToResolveTargetType.get(transaction.getProgram());

			final ModuleFilter moduleFilter = new ModuleFilter();
			moduleFilter.setNames(transaction.getProgram());
			/* We skip setting ModuleType if it's not available in the source program */
			if (ModuleType != null) {
				moduleFilter.setTypes(ModuleType);
			}
			transactionModule.declareDependency(RelationshipType.CALLS, moduleFilter, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR).setBinding(Binding.EARLY);

		}
		
		/* Create csd file (by id) that has no resource file (without ds name) */
		csdModel.getFiles().stream().filter(f -> f.getDsname() == null || f.getDsname().isEmpty())
				.forEach(f -> builder.declareSubModule(f.getId(), ModuleType.CSD_FILE));

		/* Collect dependencies for files
		 * Create csd file (by id) and resource file (by full ds name) and connect them */
		csdModel.getFiles().stream().filter(f -> f.getDsname() != null && !f.getDsname().isEmpty())
				.map(f -> new Tuple2<ModuleBuilder, ModuleBuilder>(builder.declareSubModule(f.getId(), ModuleType.CSD_FILE),
						builder.declareExternalModule(f.getDsname(), ModuleType.RESOURCE_FILE)))
				.forEach(t -> t.e1.declareDependency(RelationshipType.REFERENCES, t.e2).setBinding(Binding.EARLY));
		
		/* generate XML file for the CSD */
		if (context.getConfig().isCSDDumpXML()) {
			generateXML(csdModel, sourceObject);
		}

	}

	private static ModuleType getModuleTypeFromLanguageString(final String language) {

		if (StringUtils.isBlank(language)) {
			/* We set ModuleType to null and skip it later if it's not available in the source program */
			return null;
		}

		final ModuleType retTarget;
		switch (language.toUpperCase()) {
			case "COBOL":
				retTarget = ModuleType.COBOL_PROGRAM;
				break;
			case "PLI":
			case "PL1":
				retTarget = ModuleType.PL1_PROGRAM;
				break;
			case "ASSEMBLER":
				retTarget = ModuleType.ASSEMBLER_PROGRAM;
				break;
			default:
				retTarget = Enums.getIfPresent(ModuleType.class, language.toUpperCase()).or(ModuleType.UNKNOWN);
				break;
		}

		return retTarget;
	}

	/**
	 * Generate XML with given CSD.
	 *
	 * @param csd - CSD object to be export
	 * @param sourceObject - the source object of the original CSD file
	 */
	private void generateXML(final Csd csd, final SourcePojo sourceObject) {
		final String file = sourceObject.getPath();
		final String directory = file.substring(0, file.lastIndexOf("/") + 1);
		final String path = directory + Files.getNameWithoutExtension(file) + ".xml";
		
		SourcePojo sourceObjectByPath = null;
		try {
			sourceObjectByPath = sourceService.cachingByProjectPath(sourceObject.getProject().getNid(), path);
		} catch (final PersistenceException e) {
			/* ignore if not found */
		}
		
		final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		CsdConverter.toXML(csd, outputStream);
		
		final BinaryString content = new BinaryString(outputStream.toByteArray());
		if (sourceObjectByPath != null) {
			sourceService.update(new SourcePojoPrototype()
					.withId(sourceObjectByPath.identity())
					.setContent(content));
		} else {
			sourceService.create(new SourcePojoPrototype()
					.setProject(sourceObject.getProject())
					.setName(FilenameUtils.getName(path))
					.setPath(path)
					.setTechnology(Technology.XML)
					.setType(Type.UNKNOWN)
					.setContentRevision(1l)
					.setMetaDataRevision(1l)
					.setContent(content));
		}
	}

}
