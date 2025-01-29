/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors.basic;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.contributors.DawnMetricsUtility;
import innowake.mining.server.discovery.parser.ParserProviderService;
import innowake.mining.server.discovery.parser.basic.BasicParseResultProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.utility.UtilityEntity;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.parsing.parser.basic.BasicModel;
import innowake.ndt.parsing.parser.basic.BasicProgram;
import innowake.ndt.parsing.parser.basic.model.ast.CallStatement;
import innowake.ndt.parsing.parser.basic.model.ast.ExternalFunctionStatement;
import innowake.ndt.parsing.parser.basic.model.ast.FunctionStatement;
import innowake.ndt.parsing.parser.basic.model.ast.IncludeDirective;
import innowake.ndt.parsing.parser.basic.model.ast.IncludeDirective.IncludeType;
import innowake.ndt.parsing.parser.basic.model.ast.OpenStatement;
import innowake.ndt.parsing.parser.basic.model.ast.OpenStatement.OpenStatementFileSpecification;
import innowake.ndt.parsing.parser.basic.model.ast.OpenStatement.OpenType;
import innowake.ndt.parsing.parser.basic.model.ast.OpenStatement.OrganizationType;
import innowake.ndt.parsing.parser.basic.model.ast.ProgramStatement;
import innowake.ndt.parsing.parser.basic.model.ast.SubroutineStatement;

/**
 * Contributor for Basic Language files
 */
@Component
public class BasicContributor implements DiscoveryContributorFromSource {
	
	private static final Logger LOG = LoggerFactory.getLogger(BasicContributor.class);
	private static final String BASIC_COMMENT = "!";
	private static final String DEFAULT_EXTENSION = "BAS";
	private static final Pattern PATTERN_TO_IGNORE = Pattern.compile("<\\w+>");
	private static Pattern COM_DEPENDENCY = Pattern.compile("'@COM:([^']*?)'");
	private static final String INCLUDE = "INCLUDE ";

	@Autowired
	private SourceCachingService sourceService;

	@Autowired
	private ParserProviderService parserProvider;

	@Override
	public boolean accept(final DiscoveryContext context, final SourcePojo sourceObject) {
		return (sourceObject.getTechnology() == Technology.BASIC && sourceObject.getType() == Type.OBJECT);
	}

	@Override
	public void contribute(final DiscoveryBuilderFromSource builder, final DiscoveryContext context, final SourcePojo sourceObject) {
		final BasicParseResultProvider basicParseResultProvider = parserProvider.createBasicParser(context.getConfig(), context.getTimedWorker(),
				context.getJobId());
		final ModuleBuilder rootModule = builder.declareRootModule(sourceObject.getName(), ModuleType.BASIC_OBJECT);

		final BasicModel basicModel;
		try {
			DawnMetricsUtility.collectLinesOfCode(sourceObject, rootModule, BASIC_COMMENT);
			basicModel = basicParseResultProvider.getParseResult(sourceObject);
		} catch (final DiscoveryException e) {
			LOG.error("Error while parsing Basic Object " + sourceObject.getPath(), e);
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
		
		final Optional<AstNode> optionalBasicProgram = basicModel.getRoot();
		if ( ! optionalBasicProgram.isPresent()) {
			LOG.error("Root not found for the basic program " + sourceObject.getPath());
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, "Root not found for the basic program " + sourceObject.getPath());
			return;
		}

		final BasicProgram basicProgram = (BasicProgram) optionalBasicProgram.get();

		collectSubroutine(rootModule, basicProgram, builder);
		collectFunction(rootModule, basicProgram, builder);
		collectProgram(rootModule, basicProgram, builder);
		collectCallStatementDependencies(rootModule, basicProgram, context);
		collectIncludeDirectiveDependencies(rootModule, basicProgram, context);
		collectOpenStatementDependencies(rootModule, basicProgram, builder);
		collectExternalFunctionStatementDependencies(rootModule, basicProgram);
	}
	
	private void collectSubroutine(final ModuleBuilder rootModule, final BasicProgram basicProgram, final DiscoveryBuilderFromSource builder) {
		final List<SubroutineStatement> subroutineStatements = basicProgram.getChildrenDeep(SubroutineStatement.class);
		for (final SubroutineStatement subroutineStatement : subroutineStatements) {
			final ModuleBuilder subroutineModule = builder.declareSubModule(subroutineStatement.getName(), ModuleType.BASIC_SUBROUTINE,
					new ModuleLocation(subroutineStatement.getStartOffset(), subroutineStatement.getLength()));
			
			rootModule.declareDependency(RelationshipType.CALLS, subroutineModule)
				.setBinding(Binding.EARLY)
				.setLocation(new ModuleLocation(subroutineStatement.getStartOffset(), subroutineStatement.getLength()));
		}
	}
	
	private void collectFunction(final ModuleBuilder rootModule, final BasicProgram basicProgram, final DiscoveryBuilderFromSource builder) {
		final List<FunctionStatement> functionStatements = basicProgram.getChildrenDeep(FunctionStatement.class);
		for (final FunctionStatement functionStatement : functionStatements) {
			final ModuleBuilder functionModule = builder.declareSubModule(functionStatement.getFunctionName(), ModuleType.BASIC_FUNCTION,
					new ModuleLocation(functionStatement.getStartOffset(), functionStatement.getLength()));
			
			rootModule.declareDependency(RelationshipType.CALLS, functionModule)
				.setBinding(Binding.EARLY)
				.setLocation(new ModuleLocation(functionStatement.getStartOffset(), functionStatement.getLength()));
		}
	}
	
	private void collectProgram(final ModuleBuilder rootModule, final BasicProgram basicProgram, final DiscoveryBuilderFromSource builder) {
		final List<ProgramStatement> programStatements = basicProgram.getChildrenDeep(ProgramStatement.class);
		for (final ProgramStatement programStatement : programStatements) {
			final ModuleBuilder programModule = builder.declareSubModule(programStatement.getName(), ModuleType.BASIC_PROGRAM,
					new ModuleLocation(programStatement.getStartOffset(), programStatement.getLength()));
			
			rootModule.declareDependency(RelationshipType.CALLS, programModule)
				.setBinding(Binding.EARLY)
				.setLocation(new ModuleLocation(programStatement.getStartOffset(), programStatement.getLength()));
		}
	}
	
	private void collectCallStatementDependencies(final ModuleBuilder rootModule, final BasicProgram basicProgram, final DiscoveryContext context) {
		final List<CallStatement> callStatements = basicProgram.getChildrenDeep(CallStatement.class);
		final Set<String> utilityNames = context.getConfig().getUtilityList().getUtilities(ResolveTarget.BASIC).stream().map(UtilityEntity::getModuleName)
				.map(String::toUpperCase).collect(Collectors.toSet());
		
		getCallStatementDependencies(callStatements, rootModule);
		getCOMDependencies(callStatements, rootModule, utilityNames);
	}
	
	private void getCallStatementDependencies(final List<CallStatement> callStatements, final ModuleBuilder rootModule) {
		for (final CallStatement callStatement : callStatements) {
			final String routine = callStatement.getRoutine();
			final ModuleFilter moduleFilter = new ModuleFilter().setNames(routine).setTypes(ModuleType.BASIC_PROGRAM, ModuleType.BASIC_SUBROUTINE,
					ModuleType.BASIC_FUNCTION, ModuleType.BASIC_OBJECT, ModuleType.COBOL_PROGRAM, ModuleType.C_PROGRAM, ModuleType.VAX_MACRO_ENTRY);

			rootModule
				.declareDependency(RelationshipType.CALLS, moduleFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
				.setBinding(Binding.EARLY)
				.addAttribute(ModelAttributeKey.CALL_TYPE, "CALL " + routine)
				.setLocation(new ModuleLocation(callStatement.getStartOffset(), callStatement.getLength()));
		}
	}

	private void getCOMDependencies(final List<CallStatement> callStatements, final ModuleBuilder rootModule, final Set<String> utilityNames) {
		callStatements.stream()
			.filter(statement -> utilityNames.contains(statement.getRoutine().toUpperCase()))
			.filter(statement -> ! statement.getArguments().isEmpty())
			.map(statement -> statement.getArguments().get(0))
			.map(argument -> {
				final Matcher matcher = COM_DEPENDENCY.matcher(argument);
				if (matcher.find()) {
					return matcher.group(1);
				} else {
					return StringUtils.EMPTY;
				}
			})
			.filter(StringUtils::isNotBlank)
			.map(FilenameUtils::getBaseName)
			.filter(distinctStringCaseInsensitive())
			.forEach(routine ->
				rootModule
					.declareDependency(RelationshipType.CALLS, new ModuleFilter().setNames(routine).setTypes(ModuleType.DCL),
							ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
					.setBinding(Binding.EARLY)
			);
	}
	
	private Predicate<String> distinctStringCaseInsensitive() {
		final Set<String> set = new HashSet<>();
		return string -> set.add(string.toUpperCase());
	}

	private void collectIncludeDirectiveDependencies(final ModuleBuilder rootModule, final BasicProgram basicProgram, final DiscoveryContext context) {
		final List<IncludeDirective> includeDirectives = basicProgram.getChildrenDeep(IncludeDirective.class);
		getFileType(rootModule, filterIncludeDirectives(includeDirectives, IncludeType.FILE), context);
		getLibraryType(rootModule, includeDirectives, context);
		getCdoRecordType(rootModule, filterIncludeDirectives(includeDirectives, IncludeType.CDD));
	}

	private List<IncludeDirective> filterIncludeDirectives(final List<IncludeDirective> includeDirectives, final IncludeType includeType) {
		final Set<String> distinctIncludeeNames = new HashSet<>();
		final List<IncludeDirective> filteredDirectives = new ArrayList<>();
		for (final IncludeDirective includeDirective : includeDirectives) {
			if (includeDirective.getIncludeType().equals(includeType)) {
				String includeeName = includeDirective.getIncludeeName();
				if ( ! PATTERN_TO_IGNORE.matcher(includeeName).find()) {
					if (StringUtils.isBlank(FilenameUtils.getExtension(includeeName))) {
						includeeName = includeeName + "." + DEFAULT_EXTENSION;
					}
					if ( ! distinctIncludeeNames.contains(includeeName)) {
						distinctIncludeeNames.add(includeeName);
						filteredDirectives.add(includeDirective);
					}
				}
			}
		}
		return filteredDirectives;
	}
	
	private void getFileType(final ModuleBuilder rootModule, final List<IncludeDirective> filteredDirectives, final DiscoveryContext context) {
		for (final IncludeDirective includeDirective : filteredDirectives) {
			final String includeName = includeDirective.getIncludeeName();
			final String includeNameWithoutExt = FilenameUtils.getBaseName(includeName);
			final Optional<String> sourceObjectPath = getSourceObjectPath(context.getProjectId(), includeName, includeNameWithoutExt);
			final ModuleFilter moduleFilter = new ModuleFilter().setNames(includeNameWithoutExt);
			sourceObjectPath.ifPresent(moduleFilter::setPaths);

			rootModule
				.declareDependency(RelationshipType.REFERENCES, moduleFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
				.setBinding(Binding.EARLY)
				.addAttribute(ModelAttributeKey.OUTBOUND, INCLUDE + includeName)
				.setLocation(new ModuleLocation(includeDirective.getStartOffset(), includeDirective.getLength()))
				.createIfMissing(includeNameWithoutExt, ModuleType.BASIC_OBJECT, Identification.MISSING, ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
		}
	}

	private void getLibraryType(final ModuleBuilder rootModule, final List<IncludeDirective> includeDirectives, final DiscoveryContext context) {
		for (final IncludeDirective includeDirective : includeDirectives) {
			if (includeDirective.getIncludeType().equals(IncludeType.FROM_LIBRARY)) {
				final String includeName = includeDirective.getIncludeeName();
				final String includeNameWithoutExt = FilenameUtils.getBaseName(includeName);
				final Optional<String> sourceObjectPath = getSourceObjectPath(context.getProjectId(), includeName, includeNameWithoutExt);
				final ModuleFilter moduleFilter = new ModuleFilter().setNames(includeNameWithoutExt);
				sourceObjectPath.ifPresent(moduleFilter::setPaths);

				rootModule
					.declareDependency(RelationshipType.REFERENCES, moduleFilter, ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
					.setBinding(Binding.EARLY)
					.addAttribute(ModelAttributeKey.OUTBOUND, INCLUDE + includeName + ". LIBRARY : " + includeDirective.getLibraryName().orElse(""))
					.setLocation(new ModuleLocation(includeDirective.getStartOffset(), includeDirective.getLength()))
					.createIfMissing(includeNameWithoutExt, ModuleType.BASIC_OBJECT, Identification.MISSING, ResolutionFlag.RESOLVE_CASE_INSENSITIVE);
			}
		}
	}

	private Optional<String> getSourceObjectPath(final EntityId projectId, final String includeName, final String includeNameWithoutExt) {
		List<SourcePojo> sourceObjectList = sourceService.find(q -> q.ofProject(projectId).withName(includeNameWithoutExt)).stream()
				.filter(obj -> ( ! obj.getPath().isEmpty())).collect(Collectors.toList());
		if (sourceObjectList.isEmpty()) {
			sourceObjectList = sourceService.find(q -> q.ofProject(projectId).withName(includeNameWithoutExt.toLowerCase())).stream()
					.filter(obj -> ( ! obj.getPath().isEmpty())).collect(Collectors.toList());
		}

		if ( ! sourceObjectList.isEmpty()) {
			if (sourceObjectList.size() == 1) {
				return Optional.ofNullable(sourceObjectList.iterator().next().getPath());
			} else {
				return sourceObjectList.stream()
						.filter(obj -> FilenameUtils.getName(obj.getPath()).equalsIgnoreCase(includeName)).map(SourcePojo::getPath).findFirst();
			}
		}

		return Optional.empty();
	}

	private void getCdoRecordType(final ModuleBuilder rootModule, final List<IncludeDirective> filteredDirectives) {
		for (final IncludeDirective includeDirective : filteredDirectives) {
			final String includeName = includeDirective.getIncludeeName();
			
			rootModule
				.declareDependency(RelationshipType.REFERENCES, 
						new ModuleFilter().setNames(includeName).setTypes(ModuleType.CDO_RECORD, ModuleType.CDO_FILE), ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
				.setBinding(Binding.EARLY)
				.addAttribute(ModelAttributeKey.OUTBOUND, INCLUDE + includeName)
				.setLocation(new ModuleLocation(includeDirective.getStartOffset(), includeDirective.getLength()))
				.createIfMissing(includeName, ModuleType.CDO_FILE);
		}
	}
	
	private void collectOpenStatementDependencies(final ModuleBuilder rootModule, final BasicProgram basicProgram, final DiscoveryBuilderFromSource builder) {
		final List<OpenStatement> openStatements = basicProgram.getChildrenDeep(OpenStatement.class);
		for (final OpenStatement openStatement : openStatements) {
			final OpenStatementFileSpecification fileSpecification = openStatement.getFileSpecification();
			if (fileSpecification != null) {
				final List<Object> accessTypes = new ArrayList<>();
				if (openStatement.getOpenType() == OpenType.INPUT) {
					accessTypes.add(ModelAttributeValue.FileAccess.READ);
				} else if (openStatement.getOpenType() == OpenType.OUTPUT) {
					accessTypes.add(ModelAttributeValue.FileAccess.WRITE);
				}
				final OrganizationType orgType = openStatement.getOrganizationType() == null ? OrganizationType.SEQUENTIAL
						: openStatement.getOrganizationType();
				accessTypes.add(assertNotNull(orgType).name());
				
				final ModuleBuilder resourceFileModule = builder.declareExternalModule(fileSpecification.getFileName().toUpperCase(), ModuleType.RESOURCE_FILE);
				rootModule.declareDependency(RelationshipType.ACCESSES, resourceFileModule)
					.setBinding(Binding.EARLY)
					.addAttribute(ModelAttributeKey.FILE_ACCESS_OPERATION, "OPEN")
					.addAttribute(ModelAttributeKey.FILE_ACCESS_TYPE, accessTypes)
					.setLocation(new ModuleLocation(openStatement.getStartOffset(), openStatement.getLength()));
			}
		}
	}
	
	private void collectExternalFunctionStatementDependencies(final ModuleBuilder rootModule, final BasicProgram basicProgram) {
		final List<ExternalFunctionStatement> externalFunctionStatements = basicProgram.getChildrenDeep(ExternalFunctionStatement.class);
		externalFunctionStatements.forEach(externalFunctionStatement ->
			externalFunctionStatement.getFunctionNames().forEach(functionName ->
				rootModule
					.declareDependency(RelationshipType.CALLS,
							new ModuleFilter().setNames(functionName).setTypes(ModuleType.C_FUNCTION, ModuleType.BASIC_FUNCTION),
							ResolutionFlag.RESOLVE_CASE_INSENSITIVE)
					.setBinding(Binding.EARLY)
					.addAttribute(ModelAttributeKey.OUTBOUND, "EXTERNAL FUNCTION " + functionName)
					.setLocation(new ModuleLocation(externalFunctionStatement.getStartOffset(), externalFunctionStatement.getLength()))
			)
		);
	}
}