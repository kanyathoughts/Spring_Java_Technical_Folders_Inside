/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.DependencyBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.contributors.ims.ImsMetricsUtil;
import innowake.mining.server.discovery.metrics.DawnOriginResolver;
import innowake.mining.server.discovery.metrics.MetricsUtility;
import innowake.mining.server.discovery.metrics.ims.DawnImsTransitiveMetricsCollector.ImsLanguageSpecificContributor;
import innowake.mining.server.discovery.parser.cobol.CobolParseResultProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleBasePojo;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.ndt.cobol.parser.ast.model.CobolDataField;
import innowake.ndt.cobol.parser.ast.model.CobolExpression;
import innowake.ndt.cobol.parser.ast.model.CobolFieldReference;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.cobol.parser.ast.model.CobolProgram;
import innowake.ndt.cobol.parser.ast.model.CobolReference;
import innowake.ndt.cobol.parser.ast.model.CobolReferenceExpression;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolEntryStmt;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * This will provide all the information regarding to Cobol to create the dependencies transitively with IMS.
 */
public class CobolImsCollector implements ImsLanguageSpecificContributor{
	
	/* name used for IMS entry point, i.e. "ENTRY 'DLITCBL' USING ..." */
	private static final String DLITCBL = "DLITCBL";
	private final DiscoveryBuilder builder;
	private final ModuleBuilder rootModule;
	private final SourcePojo rootSourceObject;
	private final CobolParseResultProvider parseResultProvider;
	private final CobolModel rootCobolModel;
	private final DawnOriginResolver originResolver;
	private final ModuleService moduleService;
	private final SourceCachingService sourceService;

	public CobolImsCollector(final SourcePojo rootSourceObject, final DiscoveryBuilder builder, final ModuleBuilder rootModule,
			final CobolParseResultProvider parseResultProvider, final CobolModel rootCobolModel, final DawnOriginResolver originResolver,
			final ModuleService moduleService, final SourceCachingService sourceService) {
		this.builder = builder;
		this.rootModule = rootModule;
		this.rootSourceObject = rootSourceObject;
		this.parseResultProvider = parseResultProvider;
		this.rootCobolModel = rootCobolModel;
		this.originResolver = originResolver;
		this.moduleService = moduleService;
		this.sourceService = sourceService;
	}
	

	@Nullable
	@Override
	public Tuple2<AstNode, Integer> resolveCallNodeAndPcbNumber(final ModuleLocation callLocation, final Optional<ModuleBasePojo> rootProgram) {
		final List<CobolCallStmt> callStmts = rootCobolModel.getCobolProgram().getChildrenDeep(CobolCallStmt.class);
		CobolCallStmt selectedCall = null;
		/* we have to locate the call statement in the assembled AST, based on the original unassembled dependency location */
		for (final CobolCallStmt callStmt : callStmts) {
			final ModuleLocation originalLocation = originResolver.resolveLocation(callStmt);
			if (originalLocation.getOffset().equals(callLocation.getOffset())) {
				selectedCall = callStmt;
				break;
			}
		}
		if (selectedCall == null) {
			return null;
		}
		final CobolExpression pcbExpression = selectedCall.getUsings().get(1).getExpression();
		CobolDataField pcbField = null;
		if (pcbExpression instanceof CobolReferenceExpression) {
			final CobolReference pcbReference = ((CobolReferenceExpression) pcbExpression).getOp1();
			if (pcbReference instanceof CobolFieldReference) {
				pcbField = ((CobolFieldReference) pcbReference).getField();
			}
		}
		if (pcbField == null) {
			return null;
		}
		final int pcbNumber = rootProgram.isPresent() ? getPcbNumberUsingRootProgram(rootProgram.get(), pcbField.getName())
				: getPcbNumberUsingCurrentProgram(pcbField);
		if (pcbNumber < 0) {
			return null;
		}
		return Tuple2.of(selectedCall, pcbNumber);
	}
	
	private int getPcbNumberUsingRootProgram(final ModuleBasePojo rootProgram, final String pcbFieldName) {
		/* this is not the "correct" way to do it, but a simplification which we currently also apply for PL/1:
		 * instead of actually tracing back the value of the pcbField, we just check whether the root program contains a USING parameter
		 * that has the same name as the pcbField in the current program and assume that this is the correct parameter */
		final innowake.mining.shared.discovery.Tuple2<CobolModel, IAssembling<SourcePojo>> rootProgramParseResult;
		try {
			rootProgramParseResult = parseResultProvider.getParseResult(getSourceObject(rootProgram));
		} catch (final DiscoveryException e) {
			throw new IllegalStateException("Failed to parse" + rootProgram.getPath(), e);
		}

		final List<CobolReference> usings = getUsings(rootProgramParseResult.e1.getCobolProgram());
		final Optional<CobolReference> selectedUsingReference = usings.stream()
				.filter(ref -> {
					if (ref instanceof CobolFieldReference) {
						return ((CobolFieldReference) ref).getField().getName().equals(pcbFieldName);
					}
					return false;
				})
				.findFirst();

		return selectedUsingReference.map(usings::indexOf).orElse(-1);

	}

	private int getPcbNumberUsingCurrentProgram(final CobolDataField pcbField) {
		final List<CobolReference> usings = getUsings(rootCobolModel.getCobolProgram());

		final Optional<CobolReference> selectedUsingReference = usings.stream()
				.filter(ref -> {
					if (ref instanceof CobolFieldReference) {
						return ((CobolFieldReference) ref).getField().equals(pcbField);
					}
					return false;
				})
				.findFirst();

		return selectedUsingReference.map(usings::indexOf).orElse(-1);

	}

	private List<CobolReference> getUsings(final CobolProgram cobolProgram) {
		final var procedureDivision = cobolProgram.getProcedureDivision();
		if (procedureDivision == null) {
			return Collections.emptyList();
		}
		final List<CobolEntryStmt> entryStmts = procedureDivision.getChildrenDeep(CobolEntryStmt.class, entryStmt ->
				MetricsUtility.trimQuotesSpaces(((CobolEntryStmt) entryStmt).getName()).equals(DLITCBL));
		if ( ! entryStmts.isEmpty()) {
			return entryStmts.get(0).getUsings();
		} else {
			return procedureDivision.getUsings();
		}
	}

	@Override
	public void addTransitiveImsDependency(final AstNode callNode, final ModuleRelationshipPojo dependency,
			final Optional<ModelAttributeMap<Object>> attributeMap) {
		final Optional<ModuleBasePojo> dstModuleDetails = dependency.getDstModuleDetails();
		if (dstModuleDetails.isEmpty()) {
			return;
		}
		final var moduleFilter = new ModuleFilter().setModuleIds(dstModuleDetails.get().identity());
		addDependency(callNode, dependency, attributeMap, moduleFilter);
	}
	
	@Override
	public void addTransitiveImsDependency(final AstNode callNode, final ModuleRelationshipPojo reference, final ModuleFilter targetModuleFilter,
			final Optional<ModelAttributeMap<Object>> attributeMap, final ResolutionFlag... resolutionFlags) {
		addDependency(callNode, reference, attributeMap, targetModuleFilter, resolutionFlags);
	}
	
	private void addDependency(final AstNode callNode, final ModuleRelationshipPojo reference, final Optional<ModelAttributeMap<Object>> attributeMap,
			final ModuleFilter moduleFilter, final ResolutionFlag... resolutionFlags) {
		final var sourceModuleBuilder = originResolver.resolve(callNode, builder, rootModule);
		final DependencyBuilder dependency = sourceModuleBuilder.declareDependency(reference.getRelationship(), moduleFilter, resolutionFlags);
		reference.getDependencyBinding().ifPresent(dependency::setBinding);
		reference.getSrcLocation().ifPresent(dependency::setLocation);
		attributeMap.ifPresent(dependency::setAttributes);
	}

	@Override
	public List<ModuleRelationshipPojo> getDependenciesCallingIms() {
		final List<EntityId> moduleIds = moduleService
				.findModuleIds(q -> q.ofProject(rootSourceObject.getProject()).withPath(rootSourceObject.getPath()));
		if (moduleIds.isEmpty()) {
			return Collections.emptyList();
		}
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofProject(rootSourceObject.getProject())
				  .ofSource(moduleIds.iterator().next())
				  .includeModuleDetails(false, true));
		return references.stream()
				.filter(reference -> reference.getDstModuleDetails().isPresent())
				.filter(reference -> ImsMetricsUtil.resolveImsUtility(reference.getDstModuleDetails().get().getName()) != null)
				.collect(Collectors.toList());
	}
	
	private SourcePojo getSourceObject(final ModuleBasePojo module) {
		final Optional<String> optPath = Optional.ofNullable(module.getPath());
		final String path = optPath.orElseThrow(() -> new IllegalStateException("Source file is not present for: " + module.getName()));
		return sourceService.cachingByProjectPath(rootSourceObject.getProjectNid(), path);	
	}

	@Override
	public Technology getTargetTechnology() {
		return Technology.COBOL;
	}
}
