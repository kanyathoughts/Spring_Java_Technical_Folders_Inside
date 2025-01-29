/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import com.google.common.collect.Sets;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.metrics.AbstractSourceObjectManager;
import innowake.mining.server.discovery.metrics.NullSourceObjectAliasProvider;
import innowake.mining.server.discovery.metrics.SourceObjectAliasProvider;
import innowake.mining.server.discovery.metrics.SourceObjectDependency;
import innowake.mining.server.discovery.parser.natural.AssemblingDataProvider;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider.NaturalParseResult;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.assembling.AssemblerConfiguration;
import innowake.ndt.core.assembling.AssemblingException;
import innowake.ndt.core.assembling.IAssembler;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.IAssemblingStep;
import innowake.ndt.core.assembling.natural.INaturalAssemblingTypes;
import innowake.ndt.core.assembling.natural.NaturalAssembler;
import innowake.ndt.core.meta.IMetaAccessor;
import innowake.ndt.core.meta.IMetaData;
import innowake.ndt.core.natural.CopyCodeCore;
import innowake.ndt.core.parsing.IAst;
import innowake.ndt.core.parsing.IAstNode;
import innowake.ndt.core.parsing.IAstReference;
import innowake.ndt.core.parsing.IParseResult;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.natural.NaturalAst;
import innowake.ndt.core.parsing.natural.NaturalLightweightParsing;
import innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType;
import innowake.ndt.core.parsing.natural.dependency.DependencyNNodeTypeCategory;
import innowake.ndt.core.parsing.natural.dependency.DependencyNParameterReferenceNode;
import innowake.ndt.core.parsing.natural.dependency.DependencyNReferenceNode;
import innowake.ndt.core.parsing.natural.dependency.DependencyNStatementNode;
import innowake.ndt.core.parsing.natural.dependency.DependencyNaturalTokenParser;
import org.apache.commons.lang.StringUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Manager that provides {@link NaturalProgrammingMode} and {@link SourceObjectDependency} for {@code Natural} {@link SourcePojo}s.
 * <p>
 * This class was widely copied from {@code innowake.ndt.natclipse.core.object.internal.NaturalProjectObjectManager}.
 */
public class NaturalSourceObjectManager extends AbstractSourceObjectManager<NaturalDependency, NaturalParseResult> {

	private static final Set<DependencyNNodeType> STATIC_BINDING_TYPES = Sets.newHashSet(
				DependencyNNodeType.INCLUDE,
				DependencyNNodeType.VIEW_OF,
				DependencyNNodeType.PERFORM,
				DependencyNNodeType.LOCAL_USING,
				DependencyNNodeType.GLOBAL_USING,
				DependencyNNodeType.PARAMETER_USING);

	private final Map<Long, NaturalProgrammingMode> sourceObjectToProgrammingMode = new HashMap<>();
	private final NaturalPostProcessingManager postProcessingManager = new NaturalPostProcessingManager();
	private final IAssembler<SourcePojo> assembler;
	private final SourceObjectAliasProvider aliasProvider;
	private final NaturalParseResultProvider naturalParseResultProvider;

	/**
	 * Constructor.
	 *
	 * @param sourceObjectResolver instance of {@link SourceObjectResolver}
	 * @param configProperties instance of {@link GenericConfigProperties}
	 * @param naturalParseResultProvider of {@link NaturalParseResultProvider}
	 */
	public NaturalSourceObjectManager(final SourceObjectResolver sourceObjectResolver, final GenericConfigProperties configProperties,
			final NaturalParseResultProvider naturalParseResultProvider) {
		this(sourceObjectResolver, NullSourceObjectAliasProvider.INSTANCE, configProperties, naturalParseResultProvider);
	}
	
	/**
	 * Constructor.
	 *
	 * @param sourceObjectResolver instance of {@link SourceObjectResolver} used to resolver {@link NaturalDependency}s
	 * @param aliasProvider instance of {@link SourceObjectAliasProvider} used for resolving {@link NaturalDependency}s with alias names
	 * @param configProperties instance of {@link GenericConfigProperties}
	 * @param naturalParseResultProvider {@link NaturalParseResultProvider}
	 */
	public NaturalSourceObjectManager(
			final SourceObjectResolver sourceObjectResolver, 
			final SourceObjectAliasProvider aliasProvider, 
			final GenericConfigProperties configProperties,
			final NaturalParseResultProvider naturalParseResultProvider) {
		
		super(sourceObjectResolver, configProperties);
		assembler = new NaturalAssembler<>(new AssemblingDataProvider(sourceObjectResolver));
		this.aliasProvider = aliasProvider;
		this.naturalParseResultProvider = naturalParseResultProvider;
	}

	@Override
	public List<NaturalDependency> getOutgoingDependencies(final SourcePojo sourceObject) {
		if (mayContainReferences(sourceObject)) {
			return super.getOutgoingDependencies(sourceObject);
		}
		return Collections.emptyList();
	}

	/**
	 * Provides the {@link NaturalProgrammingMode} for the given {@link SourcePojo}.
	 *
	 * @param sourceObject the {@link SourcePojo} to provide the {@link NaturalProgrammingMode} for
	 * @return the {@link NaturalProgrammingMode} for the given {@link SourcePojo}
	 */
	public NaturalProgrammingMode getProgrammingMode(final SourcePojo sourceObject) {
		if ( ! sourceObjectToProgrammingMode.containsKey(sourceObject.getId())) {
			final ITokenPartitioning partitioning;
			try {
				partitioning = getParseResult(sourceObject).getLightweightModel().getTokenPartitioning();
			} catch (final DiscoveryException e) {
				throw new IllegalStateException(e);
			}
			final IMetaAccessor accessor = NaturalMetaDataHelper.createNaturalMetaAccessor(sourceObject.getType(), partitioning);
			accessor.parse();
			final IMetaData metaData = NaturalMetaDataHelper.accessMetaData(accessor.get());
			final NaturalProgrammingMode mode = NaturalMetaDataHelper.getProgrammingMode(metaData);
			sourceObjectToProgrammingMode.put(sourceObject.getId(), mode);
			return mode;
		}
		return sourceObjectToProgrammingMode.get(sourceObject.getId());
	}

	@Override
	protected NaturalParseResult parse(final SourcePojo sourceObject) {
		try {
			return naturalParseResultProvider.getParseResult(sourceObject);
		} catch (final DiscoveryException e) {
			throw new IllegalStateException(e);
		}
	}

	@Override
	protected void calculateDependencies(final SourcePojo sourceObject, final NaturalParseResult parseResult) {
		final IAst<Object> astUnassembled = createAst(sourceObject,
				NaturalLightweightParsing.computeNaturalTokens(suppressParsing(sourceObject) ? "" : sourceObject.getContent().toString()));
		final IAst<Object> astAssembled;
		try {
			astAssembled = createAst(sourceObject, parseResult.getLightweightModel());
		} catch (final DiscoveryException e) {
			throw new IllegalStateException(e);
		}
		parseReferences(sourceObject, astAssembled, astUnassembled);
		postProcess();
	}

	private void parseReferences(final SourcePojo sourceObject, final IAst<Object> astAssembled, final IAst<Object> astUnassembled) {
		final Set<String> subroutineNames = collectSubroutines(astAssembled);

		final IAstNode[] unassembledNodes = DependencyNNodeTypeCategory.REFERENCE.getNodes(astUnassembled);
		final IAstNode[] assembledNodes = DependencyNNodeTypeCategory.REFERENCE.getNodes(astAssembled);

		Arrays.stream(unassembledNodes)
		.filter(NaturalSourceObjectManager::isIncludeWithParameters)
		.forEachOrdered(node -> postProcessingManager.putToAssemble(sourceObject, INaturalAssemblingTypes.ASSEMBLE_COPYCODES));

		/* We collect includes from the unassembled nodes (because includes are removed during assembling) and everything else from assembled nodes.
		 * We need to filter out includes in the assembled nodes, because when assembling recursively
		 * it seems the assembled nodes can still contain includes. */
		final Stream<IAstNode> includesFromUnassembled = Arrays.stream(unassembledNodes).filter(n -> n.getType() == DependencyNNodeType.INCLUDE);
		final Stream<IAstNode> notIncludesFromAssembled;
		if (sourceObject.getType() == Type.COPYCODE) {
			/* we only collect includes from copycodes - everything else is collected only if the copycode is included by another module */
			notIncludesFromAssembled = Stream.empty();
		} else {
			notIncludesFromAssembled = Arrays.stream(assembledNodes).filter(n -> n.getType() != DependencyNNodeType.INCLUDE);
		}

		Stream.concat(includesFromUnassembled, notIncludesFromAssembled)
		.filter(IAstReference.class::isInstance)
		.filter(NaturalSourceObjectManager::isTargetNotEmpty)
		.filter(node -> containsNoPlaceholders(node, sourceObject))
		.filter(node -> isNoInlineSubroutineReference(node, subroutineNames))
		.forEachOrdered(node -> {
			final String targetName = ((IAstReference) node).getTarget();
			final DependencyNNodeType nodeType = (DependencyNNodeType) node.getType();
			boolean isStaticBinding = true;
			if ( ! STATIC_BINDING_TYPES.contains(nodeType) && node instanceof DependencyNReferenceNode) {
				final DependencyNReferenceNode referenceNode = (DependencyNReferenceNode) node;
				if ( ! referenceNode.getArgument().startsWith("'") &&  ! referenceNode.getArgument().startsWith("\"")) {
					isStaticBinding = false;
				}
			}
			final Set<Type> targetTypes = NaturalDependencyTypeManager.INSTANCE.getTargetTypes(nodeType);
			refreshStandardReference(sourceObject, targetName, targetTypes, node.getLine(), isStaticBinding);
		});
	}

	private static boolean isIncludeWithParameters(final IAstNode node) {
		return node.getType() == DependencyNNodeType.INCLUDE && ((DependencyNParameterReferenceNode) node).getNumberOfParameters() > 0;
	}

	private static boolean isTargetNotEmpty(final IAstNode node) {
		return StringUtils.isNotEmpty(((IAstReference) node).getTarget());
	}

	private static boolean containsNoPlaceholders(final IAstNode node, final SourcePojo sourceObject) {
		/* This code might contain a placeholder at reference node, then do not store directly as reference */
		return ! (sourceObject.getType() == Type.COPYCODE && CopyCodeCore.containsPlaceholder(((IAstReference) node).getTarget()));
	}

	private static boolean isNoInlineSubroutineReference(final IAstNode node, final Set<String> subroutineNames) {
		/* skip this reference, it refers to a local subroutine */
		return ! (node.getNodeType().equals(DependencyNNodeType.PERFORM) && subroutineNames.contains(normalizeHashKey(((IAstReference) node).getTarget())));
	}

	private void postProcess() {
		for (final NaturalPostProcessingItem item : postProcessingManager.getItems()) {
			if (item.hasToAssemble()) {
				assembleForConditionalReferences(item.getObject(), item.getConfiguration());
			}
		}
		postProcessingManager.clear();
	}

	private void assembleForConditionalReferences(final SourcePojo object, final AssemblerConfiguration configuration) {
		try {
			final IAssembling<SourcePojo> assembling = assembler.assemble(object, configuration);
			reparseAssembledReferences(object, assembling);
		} catch (final AssemblingException e) {
			LOG.error("Error while assembling object " + object.getPath(), e);
		}
	}

	private void reparseAssembledReferences(final SourcePojo object, final IAssembling<SourcePojo> assembling) {
		final IAssemblingStep<SourcePojo> root = assembling.getRoot();
		for (int i = 0; i < root.getNumberOfChildren(); i++) {
			final IAssemblingStep<SourcePojo> step = root.getChildAt(i);
			for (int j = 0; j < step.getNumberOfChildren(); j++) {
				final IAssemblingStep<?> childStep = step.getChildAt(j);
				if ( ! childStep.isCausedBySubstitution()) {
					continue;
				}
				final SourcePojo target = (SourcePojo) childStep.getSourceObject();
				final SourcePojo bridge = (SourcePojo) childStep.getParent().getSourceObject();
				if (target != null && bridge != null) {
					final int line = childStep.getLineBefore(j);
					refreshConditionalReference(object, bridge.getName(), target.getName(), target.getType(), line);
				}
			}
		}
	}

	private static IAst<Object> createAst(final SourcePojo sourceObject, final IParseResult parseResult) {
		return suppressParsing(sourceObject) ? new NaturalAst() :
			DependencyNaturalTokenParser.INSTANCE.parse(parseResult.getTokenPartitioning());
	}

	private static boolean suppressParsing(final SourcePojo sourceObject) {
		final Type type = sourceObject.getType();
		return type == Type.TEXT || type == Type.DIALOG_PRIV_RES || type == Type.ERROR_MESSAGE;
	}

	private static boolean mayContainReferences(final SourcePojo sourceObject) {
		switch (sourceObject.getType()) {
			case PROGRAM:
			case SUBPROGRAM:
			case SUBROUTINE:
			case FUNCTION:
			case COPYCODE:
			case ADAPTER:
			case HELP:

				/* LDAs & GDAs may contain views, so they contain references.
				 * PDAs can not contain any references */
			case LDA:
			case GDA:

				/* Maps may contain outgoing references to helproutines */
			case MAP:

				/* Dialogs may also contain outgoing references (callnats, open dialogs, ...) */
			case DIALOG:
				return true;

			default:
				return false;
		}
	}

	private void refreshStandardReference(final SourcePojo sourceObject, final String targetName, final Set<Type> targetTypes, final int line,
			final boolean isStaticBinding) {
		final String name = StringUtils.trim(targetName);
		if (acceptStandardTargetName(name)) {
			if (targetTypes.isEmpty()) {
				registerOutgoingDependency(sourceObject, new NaturalDependency(sourceObject, name, sourceObjectResolver, aliasProvider, line, isStaticBinding));
			} else if (targetTypes.size() == 1) {
				final Type targetType = targetTypes.iterator().next();
				registerOutgoingDependency(sourceObject,
						new NaturalDependency(sourceObject, name, targetType, sourceObjectResolver, aliasProvider, line, isStaticBinding));
			} else {
				registerOutgoingDependency(sourceObject,
						new NaturalDependency(sourceObject, name, sourceObjectResolver, aliasProvider, targetTypes, line, isStaticBinding));
			}
		}
	}

	private void refreshConditionalReference(final SourcePojo sourceObject, final String bridgeName, final String targetName, final Type type,
			final int line) {
		final String name = StringUtils.trim(targetName);
		if (acceptStandardTargetName(bridgeName) && acceptStandardTargetName(name)) {
			registerOutgoingDependency(sourceObject, new NaturalDependency(sourceObject, name, type, sourceObjectResolver, aliasProvider, line, true));
		}
	}

	private static boolean acceptStandardTargetName(final String name) {
		return name.length() > 0;
	}

	private static Set<String> collectSubroutines(final IAst<Object> ast) {
		return Arrays.stream(ast.getNodes(DependencyNNodeType.DEFINE_SUBROUTINE))
				.map(node -> ((DependencyNStatementNode) node).getArgument())
				.map(NaturalSourceObjectManager::normalizeHashKey)
				.collect(Collectors.toSet());
	}

	private static String normalizeHashKey(final String key) {
		return key.trim().toLowerCase();
	}
}
