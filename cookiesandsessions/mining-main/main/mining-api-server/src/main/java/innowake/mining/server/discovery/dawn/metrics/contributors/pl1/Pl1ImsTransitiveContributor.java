/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.pl1;

import static innowake.lib.core.lang.Assert.assertNotNull;

import innowake.lib.core.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.contributors.SafeExecUtils;
import innowake.mining.server.discovery.dawn.metrics.contributors.ims.ImsMetricsUtil;
import innowake.mining.server.discovery.metrics.DawnOriginResolver;
import innowake.mining.server.discovery.metrics.ims.DawnImsTransitiveMetricsCollector;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.ModuleBasePojo;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.AstUtil;
import innowake.ndt.core.parsing.ast.visitor.TopDownVisitor;
import innowake.ndt.parsing.parser.dependency.ast.pl1.AbstractPl1ProcedureNode;
import innowake.ndt.pl1parser.ast.model.attribute.BasedAttribute;
import innowake.ndt.pl1parser.ast.model.attribute.MainOption;
import innowake.ndt.pl1parser.ast.model.declaration.BaseDataTypeFormat;
import innowake.ndt.pl1parser.ast.model.declaration.BaseDeclaration;
import innowake.ndt.pl1parser.ast.model.declaration.NamedVariableDeclaration;
import innowake.ndt.pl1parser.ast.model.declaration.PointerVariableFormat;
import innowake.ndt.pl1parser.ast.model.declaration.StructureDeclaration;
import innowake.ndt.pl1parser.ast.model.expression.BaseExpression;
import innowake.ndt.pl1parser.ast.model.expression.ReferenceExpression;
import innowake.ndt.pl1parser.ast.model.reference.BaseCallReference;
import innowake.ndt.pl1parser.ast.model.reference.BaseReference;
import innowake.ndt.pl1parser.ast.model.reference.BasicReference;
import innowake.ndt.pl1parser.ast.model.reference.FieldReference;
import innowake.ndt.pl1parser.ast.model.reference.LocationQualifiedReference;
import innowake.ndt.pl1parser.ast.model.reference.QualifiedReference;
import innowake.ndt.pl1parser.ast.model.reference.Subscript;
import innowake.ndt.pl1parser.ast.model.statement.CallStatement;
import innowake.ndt.pl1parser.ast.model.statement.DeclareStatement;
import innowake.ndt.pl1parser.ast.model.statement.EntryStatement;
import innowake.ndt.pl1parser.ast.model.statement.ProcedureStatement;
import innowake.ndt.pl1parser.ast.model.statement.block.ProcedureBlock;
import org.apache.commons.lang3.ArrayUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Transitive metrics collector to collect dependencies between pl1 and ims programs.
 */
public class Pl1ImsTransitiveContributor implements DawnImsTransitiveMetricsCollector.ImsLanguageSpecificContributor {

	private static final String P$$$ZZ = "P$$$ZZ";
	private final AstModel model;
	private final DiscoveryBuilder.ModuleBuilder rootModule;
	private final DiscoveryBuilder discoveryBuilder;
	private final Pl1DependencyUtility<AbstractPl1ProcedureNode> pl1DependencyUtility;
	private final ModuleLightweightPojo moduleLightweight;
	private final Pl1ParseResultProvider parseResultProvider;
	private final SourceCachingService sourceService;
	private final ModuleService moduleService;
	private final EntityId projectId;
	private final DawnOriginResolver originResolver;

	Pl1ImsTransitiveContributor(final AstModel model, final DiscoveryBuilder.ModuleBuilder rootModule, final DiscoveryBuilder discoveryBuilder,
			final Pl1DependencyUtility<AbstractPl1ProcedureNode> pl1DependencyUtility, final ModuleLightweightPojo moduleLightweight,
			final SourceCachingService sourceService, final ModuleService moduleService, final Pl1ParseResultProvider parseResultProvider,
			final EntityId projectId) {
		this.model = model;
		this.rootModule = rootModule;
		this.discoveryBuilder = discoveryBuilder;
		this.pl1DependencyUtility = pl1DependencyUtility;
		this.moduleLightweight = moduleLightweight;
		this.sourceService = sourceService;
		this.moduleService = moduleService;
		this.parseResultProvider = parseResultProvider;
		this.projectId = projectId;
		this.originResolver = pl1DependencyUtility.getOriginResolver();
	}

	@Override
	public Technology getTargetTechnology() {
		return Technology.PL1;
	}

	@Nullable
	@Override
	public Tuple2<AstNode, Integer> resolveCallNodeAndPcbNumber(final ModuleLocation callLocation, final Optional<ModuleBasePojo> rootProgram) {
		final Optional<AstNode> optRoot = model.getRoot();
		if (optRoot.isEmpty()) {
			return null;
		}

		final AstNode root = optRoot.get();
		final List<CallStatement> callStatements = root.getChildrenDeep(CallStatement.class);
		CallStatement call = null;
		/* we have to locate the call statement in the assembled AST, based on the original unassembled dependency location */
		for (final CallStatement callStatement : callStatements) {
			final ModuleLocation originalLocation = originResolver.resolveLocation(callStatement);
			if (originalLocation.getOffset().equals(callLocation.getOffset())) {
				call = callStatement;
				break;
			}
		}
		if (call == null) {
			return null;
		}

		var reference = call.getReference();
		reference = reference instanceof FieldReference ? ((FieldReference) reference).getReference() : reference;
		final String targetName = reference.getIdentifier();

		/* THIS IS A DIRTY HACK FOR WMIN-4435 AND MUST BE REMOVED WITH WMIN-4441 */
		if (P$$$ZZ.equals(targetName)) {
			final List<BaseExpression> arguments = getCallArguments(reference);
			if (arguments.size() >= 4) {
				final BaseExpression pcbArg = arguments.get(3);
				if (pcbArg instanceof ReferenceExpression) {
					final BaseReference pcbReference = ((FieldReference) ((ReferenceExpression) pcbArg).getReference()).getReference();
					return Tuple2.of(call, resolvePcbNumber(rootProgram, root, call, pcbReference));
				}
			}
		}

		final ImsMetricsUtil.ImsUtility imsUtil = ImsMetricsUtil.resolveImsUtility(targetName);
		if (imsUtil == null) {
			return null;
		}
		if (imsUtil == ImsMetricsUtil.ImsUtility.AIBTDLI) {
			/* we currently don't resolve any AIB mask, therefore returning as unknown PCB. */
			return Tuple2.of(call, -1);
		}

		final List<BaseExpression> arguments = getCallArguments(reference);
		/* the PCB is always the third parameter */
		if (arguments.size() >= 3) {
			final BaseExpression pcbArg = arguments.get(2);
			if (pcbArg instanceof ReferenceExpression) {
				final BaseReference pcbReference = ((FieldReference) ((ReferenceExpression) pcbArg).getReference()).getReference();
				return Tuple2.of(call, resolvePcbNumber(rootProgram, root, call, pcbReference));
			}
		}
		return Tuple2.of(call, -1);
	}

	@Override
	public void addTransitiveImsDependency(final AstNode callNode, final ModuleRelationshipPojo dependency,
			final Optional<ModelAttributeMap<Object>> attributeMap) {
		final Optional<ModuleBasePojo> dstModuleDetails = dependency.getDstModuleDetails();
		if (dstModuleDetails.isEmpty()) {
			return;
		}
		final var moduleFilter = new ModuleFilter().setModuleIds(dstModuleDetails.get().identity());
		addDependency(callNode, dependency, moduleFilter, attributeMap, ResolutionFlag.MERGE_DUPLICATES);
	}

	@Override
	public void addTransitiveImsDependency(final AstNode callNode, final ModuleRelationshipPojo dependency, final ModuleFilter targetModuleFilter,
			final Optional<ModelAttributeMap<Object>> attributeMap, final ResolutionFlag... resolutionFlags) {
		addDependency(callNode, dependency, targetModuleFilter, attributeMap, resolutionFlags);
	}

	private void addDependency(final AstNode callNode, final ModuleRelationshipPojo dependency, final ModuleFilter targetModuleFilter,
			final Optional<ModelAttributeMap<Object>> attributeMap, final ResolutionFlag... resolutionFlags) {
		final var originalSource = originResolver.resolveSourceObject(callNode);

		final var sourceModuleBuilder = originalSource.isPresent() ?
				discoveryBuilder.anchorTo(new ModuleFilter().setPaths(originalSource.get().getPath())) : rootModule;
		
		final Optional<DiscoveryBuilder.ModuleBuilder> anchoredProcedureModule = getAnchoredProcedure(originalSource);
		pl1DependencyUtility.addDependency(sourceModuleBuilder, dependency, attributeMap, targetModuleFilter, resolutionFlags);
		anchoredProcedureModule.ifPresent(subModuleBuilder -> pl1DependencyUtility.addDependency(subModuleBuilder, dependency, attributeMap,
				targetModuleFilter, resolutionFlags));
	}

	private Optional<DiscoveryBuilder.ModuleBuilder> getAnchoredProcedure(final Optional<SourcePojo> originalSource) {
		if (originalSource.isPresent() && (originalSource.get().getType() == Type.COPYBOOK ||
				! originalSource.get().getPath().equals(moduleLightweight.getPath()))) {
			return Optional.empty();
		}

		if (originalSource.isPresent()) {
			final var sourceModuleId = moduleService.findModuleIds(q -> q.ofProject(projectId)
							.withPath(assertNotNull(originalSource.get().getPath())))
					.stream()
					.findFirst();

			return getMainSubroutine(sourceModuleId.orElseThrow(() ->
					new IllegalStateException("moduleId not found for path: " + originalSource.get().getPath())));
		} else  {
			return getMainSubroutine(moduleLightweight.identity());
		}
	}

	Optional<DiscoveryBuilder.ModuleBuilder> getMainSubroutine(final EntityId sourceModuleId) {
		final var mainSubroutine = moduleService.findRelationship(q -> q.ofModuleInDirection(sourceModuleId, RelationshipDirection.OUT)
						.withTypes(Collections.singleton(RelationshipType.CONTAINS)).includeModuleDetails(false, true))
				.stream()
				.map(ModuleRelationshipPojo::getDstModuleDetails)
				.filter(Optional::isPresent)
				.filter(vm -> vm.get().getModuleType() == ModuleType.PL1_SUBROUTINE)
				.map(Optional::get)
				.findFirst();
		return mainSubroutine.map(moduleBasePojo -> discoveryBuilder.anchorTo(new ModuleFilter().setModuleIds(moduleBasePojo.identity()),
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR));
	}


	@Override
	public List<ModuleRelationshipPojo> getDependenciesCallingIms() {
		/* Fetch the dependencies called by both the main program and the PL1 Copybooks and find the utility dependencies in them*/

		final Optional<ModuleLightweightPojo> module = moduleService
				.findAnyModuleLightweight(q -> q.ofProject(projectId).withPath(assertNotNull(moduleLightweight.getPath())));
		if (module.isEmpty()) {
			return Collections.emptyList();
		}
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofProject(projectId)
				.ofSource(module.get().identity())
				.includeModuleDetails(false, true));
		final Predicate<ModuleRelationshipPojo> isValidUtilityDependency =
				reference -> reference.getDstModuleDetails().isPresent() && ImsMetricsUtil.resolveImsUtility(reference.getDstModuleDetails().get().getName()) != null
				/* THIS IS A DIRTY HACK FOR WMIN-4435 AND MUST BE REMOVED WITH WMIN-4441 */
				|| P$$$ZZ.equals(reference.getDstModuleDetails().get().getName());
		final var dependenciesCallingIms = references.stream()
				.filter(isValidUtilityDependency)
				.collect(Collectors.toList());
		final var copyBookDependencies = references.stream()
				.filter(dep -> dep.getDstModuleDetails().isPresent() && (dep.getDstModuleDetails().get().getModuleType() == ModuleType.PL1_COPYBOOK))
				.flatMap(dep -> moduleService.findRelationship(q -> q.ofProject(projectId)
						.ofSource(dep.getDstModuleDetails().get().identity())
						.includeModuleDetails(false, true)).stream())
				.filter(isValidUtilityDependency)
				.collect(Collectors.toList());

		return Stream.of(dependenciesCallingIms, copyBookDependencies).flatMap(List::stream).collect(Collectors.toList());
	}

	@Override
	public List<ModuleRelationshipPojo> getAdditionalDependencies(final ModuleRelationshipPojo callDependency) {
		/* when doing any changes to the call dependency of the main program, we also have to include the dependency that had
		 * been added to the subroutine. therefore we have to search the matching dependency in the subroutine. */
		final var sourceModuleId = moduleLightweight.identity();
		final var mainSubroutine = moduleService.findRelationship(q -> q.ofModuleInDirection(sourceModuleId, RelationshipDirection.OUT)
				.withTypes(Collections.singleton(RelationshipType.CONTAINS)))
				.stream()
				.map(ModuleRelationshipPojo::getDstModuleDetails)
				.filter(Optional::isPresent)
				.filter(vm -> vm.get().getModuleType() == ModuleType.PL1_SUBROUTINE)
				.map(Optional::get)
				.findFirst();

		final Optional<ModuleLocation> optCallLocation = callDependency.getSrcLocation();
		final var optionalTarget = callDependency.getDstModuleDetails();
		if (mainSubroutine.isEmpty() || optCallLocation.isEmpty() || optionalTarget.isEmpty()) {
			return Collections.emptyList();
		}
		final ModuleLocation callLocation = optCallLocation.get();
		final var target = optionalTarget.get();
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofProject(projectId)
				.ofSource(mainSubroutine.get().identity())
				.includeModuleDetails(false, true));

		return references.stream().filter(d -> {
					final Optional<ModuleLocation> optLoc = d.getSrcLocation();
					final var dstTarget = d.getDstModuleDetails();
					if (optLoc.isPresent() && dstTarget.isPresent() && target.equals(dstTarget.get())) {
						final ModuleLocation loc = optLoc.get();
						return loc.getOffset().equals(callLocation.getOffset()) && loc.getLength().equals(callLocation.getLength());
					}
					return false;
				})
				.collect(Collectors.toList());
	}

	private int resolvePcbNumber(final Optional<ModuleBasePojo> rootProgram, final AstNode root, final CallStatement call, final BaseReference pcbReference) {
		if (pcbReference instanceof LocationQualifiedReference) {
			/* check if the left hand side of the pointer qualification is the PCB pointer we're looking for: PCB_PTR->PCB_MASK */
			final BaseReference locatorReference = ((LocationQualifiedReference) pcbReference).getLocatorReference();
			final String locatorReferenceName = resolveFieldReference(locatorReference);
			if (locatorReferenceName != null) {
				final var result = getPcbPointerOrder(rootProgram, root, assertNotNull(getLastQualificationSegment(locatorReferenceName)));
				if (result != -1) {
					return result;
				}
			}
		}

		final String pcbReferenceName = resolveFieldReference(pcbReference);
		if (pcbReferenceName == null) {
			return -1;
		}

		final BaseDeclaration resolvedDeclaration;
		if (pcbReferenceName.contains(".")) {
			resolvedDeclaration = findQualifiedField(call, pcbReferenceName);
		} else {
			resolvedDeclaration = findUnqualifiedField(call, pcbReferenceName, false);
		}
		final String pcbPointerName = getPcbPointerNameFromDeclaration(resolvedDeclaration);
		if (pcbPointerName == null) {
			return -1;
		}

		return getPcbPointerOrder(rootProgram, root, pcbPointerName);
	}

	private int getPcbPointerOrder(final Optional<ModuleBasePojo> rootProgram, final AstNode rootNode, final String pcbPointerName) {
		/* this module containing the CALL statement and the root program calling this one in its call chain may be different.
		 * the root program is the entry point from a job or transaction and only the root program can be used to properly identify the PCB,
		 * based on the pointers in the main procedure declaration. */
		Optional<AstNode> rootProgramRootNode = Optional.empty();
		if (rootProgram.isPresent()) {
			final var sourceObject = getSourceObject(rootProgram.get());
			final var rootProgramModel = getAstModel(sourceObject);
			if (rootProgramModel.isPresent()) {
				rootProgramRootNode = rootProgramModel.get().getRoot();
			}
		}
		final Optional<ProcedureStatement> optMainProc = rootProgramRootNode.orElse(rootNode).getChildrenDeep(ProcedureStatement.class).stream()
				.filter(ps -> ps.getOptions().stream()
						.anyMatch(option -> MainOption.class.isAssignableFrom(option.getClass())))
				.findFirst();
		if (optMainProc.isEmpty()) {
			return -1;
		}

		/* IMS enforces that the order for the pointer parameters in the main procedure declaration is exactly the same as the order of the PCBs in the PSB.
		 * example:
		 * 	MAINPROC: PROC(POINTER_1, POINTER_2) OPTIONS(MAIN)
		 *    CALL PLITDLI (3, FUNC_GU, POINTER_1, DET_SEG_IO_AREA)
		 * POINTER_1 is the pointer receiving the address of the first PCB declared in the PSB and POINTER_2 for the second PCB.
		 * the call statement is then resolved to make use of the first PCB. */
		final ProcedureStatement mainProc = optMainProc.get();
		List<BasicReference> parameters = mainProc.getParameters();
		for (int i = 0; i < parameters.size(); i++) {
			/* we expect that the pointer names used in the CALL statements are exactly the same as in the main procedure declaration.
			 * therefore sub-programs/procedures/functions using different pointer names will not be matched, as we cannot track the pointers throughout
			 * the call hierarchy. */
			if (parameters.get(i).getIdentifier().equals(pcbPointerName)) {
				return i;
			}
		}

		/* check the parameter declarations of any additional entry point into the main procedure. */
		final List<EntryStatement> entryStatements = getChildrenDeepUntil(mainProc, EntryStatement.class, ProcedureBlock.class);
		for (final EntryStatement es : entryStatements) {
			parameters = es.getParameters();
			for (int i = 0; i < parameters.size(); i++) {
				if (parameters.get(i).getIdentifier().equals(pcbPointerName)) {
					return i;
				}
			}
		}

		return -1;
	}

	private List<BaseExpression> getCallArguments(final BaseReference callReference) {
		List<innowake.ndt.pl1parser.ast.model.expression.BaseExpression> arguments = Collections.emptyList();
		if (callReference instanceof BaseCallReference) {
			arguments = ((BaseCallReference) callReference).getArguments();
		} else {
			final Optional<Subscript> optArguments = ((BasicReference) callReference).getSubscript();
			if (optArguments.isPresent()) {
				arguments = optArguments.get().getDimensions();
			}
		}
		return arguments;
	}

	@Nullable
	private BaseDeclaration findQualifiedField(final AstNode node, final String qualifiedName) {
		final String[] parts = qualifiedName.split("\\.");
		/* we first locate the actual group declaration by using the base qualifier of the field */
		final BaseDeclaration baseStructure = findUnqualifiedField(node, parts[0], true);
		if (baseStructure instanceof StructureDeclaration) {
			/* then we check if we can find the remainder of the qualification chain inside this group declaration */
			return findQualifiedFieldInStructure(baseStructure, ArrayUtils.remove(parts, 0));
		}
		return null;
	}

	@Nullable
	private BaseDeclaration findUnqualifiedField(final AstNode node, final String name, final boolean preferGroupAboveField) {
		/* an unqualified field can be directly on level 1 as a plain or factored declaration together with other fields.
		 * PL/I also allows unqualified field references, even though it's declared in a group, as long as it's unique in the current scope.
		 * this must be scoped to the current procedure, as any declaration in nested procedures is not in valid access scope! */
		final Optional<NamedVariableDeclaration> fieldDeclaration = node.getChildren(DeclareStatement.class).stream()
				.map(ds -> ds.getChildrenDeep(NamedVariableDeclaration.class))
				.flatMap(List::stream)
				.filter(nvd -> nvd.getVariableName().equals(name))
				.findFirst();
		/* the field in question could also be a group declaration */
		final Optional<StructureDeclaration> structureDeclaration = node.getChildren(DeclareStatement.class).stream()
				.map(ds -> ds.getChildrenDeep(StructureDeclaration.class))
				.flatMap(List::stream)
				.filter(sd -> sd.getName().equals(name))
				.findFirst();

		final boolean foundField = fieldDeclaration.isPresent();
		final boolean foundGroup = structureDeclaration.isPresent();
		if (foundField) {
			if (foundGroup && preferGroupAboveField) {
				return structureDeclaration.get();
			}
			return fieldDeclaration.get();
		} else if (foundGroup) {
			return structureDeclaration.get();
		}

		/* if we can't locate anything in the current procedure, we have to go up to the parent one with the next valid scope */
		final ProcedureBlock parentProcedure = AstUtil.findParent(node.getParent(), ProcedureBlock.class);
		if (parentProcedure != null) {
			return findUnqualifiedField(parentProcedure, name, preferGroupAboveField);
		}
		return null;
	}

	@Nullable
	private BaseDeclaration findQualifiedFieldInStructure(final AstNode node, final String[] qualifiedParts) {
		if (qualifiedParts.length == 1) {
			/* only one segment left, so it can either be a field or a nested group declaration inside the group structure */
			final Optional<NamedVariableDeclaration> fieldDeclaration = node.getChildrenDeep(NamedVariableDeclaration.class).stream()
					.filter(nvd -> nvd.getVariableName().equals(qualifiedParts[0]))
					.findFirst();
			if (fieldDeclaration.isPresent()) {
				return fieldDeclaration.get();
			}
		}

		/* same as with unqualified fields, the qualifications for nested groups must also not be complete as long as they're unique.
		 * therefore searching deep, in case one segment is skipped. */
		final Optional<StructureDeclaration> structureDeclaration = node.getChildrenDeep(StructureDeclaration.class).stream()
				.filter(sd -> sd.getName().equals(qualifiedParts[0]))
				.findFirst();
		if (structureDeclaration.isPresent()) {
			if (qualifiedParts.length == 1) {
				return structureDeclaration.get();
			}

			/* when there are multiple segments left, then there is still another nested group declaration to be located. */
			return findQualifiedFieldInStructure(structureDeclaration.get(), ArrayUtils.remove(qualifiedParts, 0));
		}
		return null;
	}

	@Nullable
	private String getPcbPointerNameFromDeclaration(@Nullable final BaseDeclaration declaration) {
		if (declaration == null) {
			return null;
		}

		/* only fields have types, but not groups */
		final Optional<BaseDataTypeFormat> typeFormat =
				declaration instanceof NamedVariableDeclaration ? ((NamedVariableDeclaration) declaration).getDataTypeFormat() : Optional.empty();
		final Optional<BasedAttribute> basedAttrib = declaration.getBasedAttribute();

		if (typeFormat.isPresent() && typeFormat.get() instanceof PointerVariableFormat) {
			/* the field is declared as a pointer: DCL MY_FIELD POINTER */
			return ((NamedVariableDeclaration) declaration).getVariableName();
		}
		if (basedAttrib.isPresent()) {
			/* BASED field or structure that may reference a default pointer, that we extract: DCL MY_FIELD BASED(MY_POINTER) */
			final Optional<BaseReference> locatorReference = basedAttrib.get().getLocatorReference();
			if (locatorReference.isPresent()) {
				return getLastQualificationSegment(resolveFieldReference(locatorReference.get()));
			}
		}
		return null;
	}

	@Nullable
	private String resolveFieldReference(final BaseReference fieldReference) {
		final BaseReference reference = fieldReference instanceof FieldReference ? ((FieldReference) fieldReference).getReference() : fieldReference;
		if (reference instanceof QualifiedReference) {
			/* qualified field reference. for example second parameter: CALL PLITDLI (3, DLI_FUNC3.FUNC_GN, DB_PTR_MAST, DET_SEG_IO_AREA) */
			return assembleQualifiedReference((QualifiedReference) reference);
		} else if (reference instanceof LocationQualifiedReference) {
			/* explicit pointer qualification. for example second parameter: CALL PLITDLI (3, FUNC_PTR->FUNC_REPL, DB_PTR_MAST, DET_SEG_IO_AREA)
			 * we're only interested in the field on the right hand side of the pointer qualification */
			return assembleLocationQualifiedReference((LocationQualifiedReference) reference);
		} else {
			/* plain unqualified field. for example second parameter: CALL PLITDLI (3, FUNC_GU, DB_PTR_MAST, DET_SEG_IO_AREA) */
			return reference.getIdentifier();
		}
	}

	@Nullable
	private String assembleLocationQualifiedReference(final LocationQualifiedReference ref) {
		BaseReference basedRef = ref.getBasedReference();
		basedRef = basedRef instanceof FieldReference ? ((FieldReference) basedRef).getReference() : basedRef;
		if (basedRef instanceof LocationQualifiedReference) {
			return assembleLocationQualifiedReference((LocationQualifiedReference) basedRef);
		} else if (basedRef instanceof QualifiedReference) {
			return assembleQualifiedReference((QualifiedReference) basedRef);
		} else {
			return basedRef.getIdentifier();
		}
	}

	@Nullable
	private String assembleQualifiedReference(final QualifiedReference qualifiedReference) {
		final BaseReference locatorReference = qualifiedReference.getLocatorReference();
		final BaseReference basedReference = qualifiedReference.getBasedReference();
		if ( ! ((basedReference instanceof QualifiedReference || basedReference instanceof BasicReference) && locatorReference instanceof BasicReference)) {
			return null;
		}

		final StringBuilder sb = new StringBuilder(50);
		sb.append(((BasicReference) locatorReference).getIdentifier()).append(".");
		if (basedReference instanceof QualifiedReference) {
			sb.append(assembleQualifiedReference((QualifiedReference) basedReference));
		} else {
			sb.append(((BasicReference) basedReference).getIdentifier());
		}
		return sb.toString();
	}

	@Nullable
	private String getLastQualificationSegment(@innowake.lib.core.api.lang.Nullable final String reference) {
		if (reference != null) {
			final int dotIndex = reference.lastIndexOf('.');
			if (dotIndex != -1) {
				return reference.substring(dotIndex + 1);
			}
		}
		return reference;
	}

	/*
	 * Collects nodes top-down of type nodeType, but stops collecting when reaching a node of type limit.
	 */
	@SuppressWarnings("unchecked")
	private <T> List<T> getChildrenDeepUntil(final AstNode startNode, final Class<?> nodeType, final Class<?> limit) {
		final List<T> result = new ArrayList<>();
		new TopDownVisitor<AstNode>(node -> {
			final Class<?> nodeClass = node.getClass();
			if (startNode != node && limit.isAssignableFrom(nodeClass)) {
				return false;
			}
			if (nodeType.isAssignableFrom(nodeClass)) {
				result.add((T) node);
			}
			return true;
		}).visit(startNode);
		return Collections.unmodifiableList(result);
	}

	private SourcePojo getSourceObject(final ModuleBasePojo module) {
		final Optional<String> optPath = Optional.ofNullable(module.getPath());
		final String path = optPath.orElseThrow(() -> new IllegalStateException("Source file is not present for: " + module.getName()));
		return sourceService.cachingByProjectPath(projectId.getNid(), path);
	}

	private Optional<AstModel> getAstModel(final SourcePojo sourceObject) {
		final var parseResult =  SafeExecUtils.safeExecute(parseResultProvider::getParseResult, sourceObject, rootModule);
		try {
			if (parseResult.isPresent()) {
				return Optional.ofNullable(parseResult.get().getProgramModel());
			}
		} catch (final DiscoveryException e) {
			rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, "Unable to get lightweight model for source: " + sourceObject.getPath());
		}
		return Optional.empty();
	}

}
