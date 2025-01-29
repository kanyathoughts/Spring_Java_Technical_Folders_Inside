/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static innowake.mining.data.core.api.AstNodeUtils.ARITHMETIC_EXPRESSION;
import static innowake.mining.data.core.api.AstNodeUtils.ASSIGNING_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.BINDING_NODE;
import static innowake.mining.data.core.api.AstNodeUtils.CFG_COLLAPSIBLE_NODE;
import static innowake.mining.data.core.api.AstNodeUtils.CONSTANT_REFERENCE;
import static innowake.mining.data.core.api.AstNodeUtils.ERROR_PROCESSING_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.FIELD_REFERENCE;
import static innowake.mining.data.core.api.AstNodeUtils.setConditionPaths;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.storeast.api.AstNodeToMiningNode;
import innowake.mining.data.core.storeast.api.ModuleProvider;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.ndt.core.assembling.retrace.Inclusion;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.parsing.ILocation;
import innowake.ndt.core.parsing.ast.AdvancedLocationProvider;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.RetracedLocation;
import innowake.ndt.core.parsing.ast.model.ArithmeticExpression;
import innowake.ndt.core.parsing.ast.model.Branch;
import innowake.ndt.core.parsing.ast.model.BranchStatement;
import innowake.ndt.core.parsing.ast.model.BreakStatement;
import innowake.ndt.core.parsing.ast.model.ConstantReference;
import innowake.ndt.core.parsing.ast.model.ContinueStatement;
import innowake.ndt.core.parsing.ast.model.DefaultBranch;
import innowake.ndt.core.parsing.ast.model.Directive;
import innowake.ndt.core.parsing.ast.model.Entry;
import innowake.ndt.core.parsing.ast.model.ErrorProcessingStatement;
import innowake.ndt.core.parsing.ast.model.HaltStatement;
import innowake.ndt.core.parsing.ast.model.Invocable;
import innowake.ndt.core.parsing.ast.model.LoopControlStatement;
import innowake.ndt.core.parsing.ast.model.LoopEventStatement;
import innowake.ndt.core.parsing.ast.model.LoopStatement;
import innowake.ndt.core.parsing.ast.model.ModuleReturnStatement;
import innowake.ndt.core.parsing.ast.model.ReturnStatement;
import innowake.ndt.core.parsing.ast.model.Statement;
import innowake.ndt.core.parsing.ast.model.cfg.CfgCollapsibleNode;
import innowake.ndt.core.parsing.ast.model.statement.ArithmeticStatement;
import innowake.ndt.core.parsing.ast.model.statement.AssigningStatement;
import innowake.ndt.core.parsing.ast.model.statement.BindingNode;
import innowake.ndt.core.parsing.ast.model.statement.CallExternalStatement;
import innowake.ndt.core.parsing.ast.model.statement.CallInternalStatement;
import innowake.ndt.core.parsing.ast.model.statement.CallStatement;
import innowake.ndt.core.parsing.ast.model.statement.DatabaseAccessStatement;
import innowake.ndt.core.parsing.ast.model.statement.FieldDefinition;
import innowake.ndt.core.parsing.ast.model.statement.FieldReference;
import innowake.ndt.core.parsing.ast.model.statement.FileAccessStatement;
import innowake.ndt.core.parsing.ast.model.statement.JumpStatement;
import innowake.ndt.core.parsing.ast.model.statement.UiStatement;
import innowake.ndt.core.parsing.spi.Document;

/**
 * Core implementation for {@link AstNodeToMiningNode}.
 * 
 * @param <T> the concrete type of {@link Retracing}
 */
public class AstNodeToMiningNodeFunctionImpl<T> implements AstNodeToMiningNode {

	private static final Logger LOG = LoggerFactory.getLogger(AstNodeToMiningNodeFunctionImpl.class);
	
	protected final Map<FieldDefinition<?>, StoreAstPrototype> dataFieldToAstNode = new HashMap<>();
	private final List<AstNode> bindingNodes;
	private final Map<AstNode, StoreAstPrototype> parserToStoreNodeMap;
	protected final ModuleProvider<T> moduleProvider; 
	@Nullable
	protected final AdvancedLocationProvider<T> locationProvider;
	protected final RetracingProvider<T> retracingProvider;
	protected final ModulePojo rootModule;
	protected final LabelCreator labelCreator;
	private final String source;
	private final Document document;
	
	/**
	 * Constructor.
	 * 
	 * @param source the assembled source code of the Module to be transformed
	 * @param moduleProvider the module provider
	 * @param locationProvider the location provider
	 * @param retracingProvider the retracing provider
	 * @param document {@link Document} with unassembled content
	 */
	public AstNodeToMiningNodeFunctionImpl(
			final String source, 
			final ModuleProvider<T> moduleProvider, 
			@Nullable
			final AdvancedLocationProvider<T> locationProvider,
			final RetracingProvider<T> retracingProvider,
			final Document document) {
		this.moduleProvider = moduleProvider;
		this.locationProvider = locationProvider;
		this.retracingProvider = retracingProvider;
		this.source = source;
		rootModule = moduleProvider.getRootModule();
		labelCreator = new LabelCreator(source);
		bindingNodes = new ArrayList<>();
		parserToStoreNodeMap = new HashMap<>();
		this.document = document;
	}
	
	@Override
	public StoreAstPrototype apply(final innowake.ndt.core.parsing.ast.AstNode node) {
		final StoreAstPrototype astNode = createAstNode(node);
		parserToStoreNodeMap.put(node, astNode);
		
		/* the parser returns a non-modifiable Map but we need to change it later */
		final var nodeProperties = new HashMap<>(node.createProperties());
		astNode.setProperties(nodeProperties);
		
		setSuperTypesAttributes(node, astNode);
		
		if (node.isRoot()) {
			astNode.addModuleRelationship(rootModule.identity(), AstModuleRelationshipType.ROOT);
		} else if (node instanceof FieldDefinition<?>) {
			final String name = StringUtils.trimToEmpty(((FieldDefinition<?>) node).getFieldName());
			final String declaration = source.substring(node.getStartOffset(), node.getEndOffset() + 1);
			nodeProperties.put("nameOffset", name.isEmpty() ? -1 : declaration.indexOf(name));
			nodeProperties.put("nameLength", name.length());
		}
		
		return astNode;
	}

	private StoreAstPrototype createAstNode(final AstNode node) {
		if (node.getGenericType(BindingNode.class) != null) {
			bindingNodes.add(node);
		}
		RetracedLocation<T> retracedLocation = null;
		ILocation assembledLocation = null;
		ILocation rootRelativeLocation = null;
		final AdvancedLocationProvider<T> advancedLocationProvider = locationProvider;
		if (advancedLocationProvider != null) {
			try {
				retracedLocation = advancedLocationProvider.getRetracedLocation(node);
			} catch (final Exception e) {
				LOG.warn(() -> String.format("storeAst: Unable to get retraced location for node %s. Error was: %s", node, e));
			}
			try {
				assembledLocation = advancedLocationProvider.getAssembledLocation(node);
			} catch (final Exception e) {
				LOG.warn(() -> String.format("storeAst: Unable to get assembled location for node %s. Error was: %s", node, e));
			}
			try {
				rootRelativeLocation = advancedLocationProvider.getRootRelativeLocation(node);
			} catch (final Exception e) {
				LOG.warn(() -> String.format("storeAst: Unable to get root relative location for node %s. Error was: %s", node, e));
			}
		}
		final Inclusion<T> inclusion = retracingProvider.getInclusion(node);
		final String type = node.getAstNodeName();
		final String label = labelCreator.getLabel(node);
		
		if (inclusion == null) {
			return getStoreAstNode(type, label, retracedLocation, assembledLocation, rootRelativeLocation, -1, -1, null, node);
		} else {
			final T callee = inclusion.getCallee();
			final EntityId calleeModuleId;
			if (callee == null) {
				LOG.warn(() -> String.format("storeAst: Unable to resolve inclusion callee module ID %d.", rootModule.getId()));
				calleeModuleId = null;
			} else {
				calleeModuleId = moduleProvider.getModule(callee).identity();
			}
			final int level = inclusion.getLevel();
			final int ordinal = inclusion.getOrdinal();
			return getStoreAstNode(type, label, retracedLocation, assembledLocation, rootRelativeLocation, level, ordinal, calleeModuleId, node);
		}
	}

	private StoreAstPrototype getStoreAstNode(String type, String label, @Nullable RetracedLocation<T> retracedLocation, @Nullable ILocation assembledLocation,
			@Nullable ILocation rootRelativeLocation, int level, int ordinal, @Nullable EntityId calleeModuleId, AstNode node) {
		try {
			final Integer retracedOffset = (retracedLocation != null) ? retracedLocation.getOffset() : node.getStartOffset();
			final Integer retracedLength = (retracedLocation != null) ? retracedLocation.getLength() : node.getLength();
			final Integer assembledOffset = (assembledLocation != null) ? assembledLocation.getOffset() : node.getStartOffset();
			final Integer assembledLength = (assembledLocation != null) ? assembledLocation.getLength() : node.getLength();
			final Integer rootRelativeOffset = (rootRelativeLocation != null) ? rootRelativeLocation.getOffset() : node.getStartOffset();
			final Integer rootRelativeLength = (rootRelativeLocation != null) ? rootRelativeLocation.getLength() : node.getLength();
			final var rootRelativeStartLineNumber = Integer.valueOf(document.getLineNumber(rootRelativeOffset) + 1);
			final int rootRelativeEndOffset = rootRelativeOffset + rootRelativeLength;
			final var rootRelativeEndLineNumber = Integer.valueOf(document.getLineNumber(rootRelativeEndOffset) + 1);
			final var advancedModuleLocation = new AstNodeLocation(retracedOffset, retracedLength, assembledOffset, assembledLength,
					rootRelativeOffset, rootRelativeLength, rootRelativeStartLineNumber, rootRelativeEndLineNumber);
			return new StoreAstPrototype(type, label, rootModule.identity(), advancedModuleLocation, level, ordinal, calleeModuleId);
		} catch (final IllegalStateException e) {
			return new StoreAstPrototype(type, label, rootModule.identity(), new AstNodeLocation(-1, -1, -1, -1, -1, -1, -1, -1), level, ordinal, calleeModuleId);
		}
	}
	
	private void setSuperTypesAttributes(final innowake.ndt.core.parsing.ast.AstNode node, final StoreAstPrototype astNode) {
		if (node.getGenericType(Directive.class) != null) {
			astNode.addSuperType(AstNodeUtils.DIRECTIVE);
		}
		if (node.getGenericType(Invocable.class) != null) {
			astNode.addSuperType(AstNodeUtils.INVOCABLE);
		}
		if (node.getGenericType(Entry.class) != null) {
			astNode.addSuperType(AstNodeUtils.ENTRY);
		}
		if (node.getGenericType(Statement.class) != null) {
			astNode.addSuperType(AstNodeUtils.STATEMENT);
		}
		if (node.getGenericType(BranchStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.BRANCH_STATEMENT);
			setConditionPaths((BranchStatement) node, astNode);
		}
		if (node.getGenericType(Branch.class) != null) {
			astNode.addSuperType(AstNodeUtils.BRANCH);
		}
		if (node.getGenericType(DefaultBranch.class) != null) {
			astNode.addSuperType(AstNodeUtils.DEFAULT_BRANCH);
		}
		if (node.getGenericType(JumpStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.JUMP_STATEMENT);
		}
		if (node.getGenericType(FileAccessStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.FILE_ACCESS_STATEMENT);
		}
		if (node.getGenericType(DatabaseAccessStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.DATABASE_ACCESS_STATEMENT);
		}
		if (node.getGenericType(UiStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.UI_STATEMENT);
		}
		if (node.getGenericType(ArithmeticStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.ARITHMETIC_STATEMENT);
		}
		if (node.getGenericType(LoopStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.LOOP_STATEMENT);
		}
		if (node.getGenericType(LoopControlStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.LOOP_CONTROL_STATEMENT);
			if (node.getGenericType(ContinueStatement.class) != null) {
				astNode.addSuperType(AstNodeUtils.CONTINUE_STATEMENT);
			}
			if (node.getGenericType(BreakStatement.class) != null) {
				astNode.addSuperType(AstNodeUtils.BREAK_STATEMENT);
			}
		}
		if (node.getGenericType(ReturnStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.RETURN_STATEMENT);
		}
		if (node.getGenericType(ModuleReturnStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.MODULE_RETURN_STATEMENT);
		}
		if (node.getGenericType(HaltStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.HALT_STATEMENT);
		}
		if (node.getGenericType(LoopEventStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.LOOP_EVENT_STATEMENT);
		}
		
		if (node.getGenericType(CallStatement.class) != null) {
			astNode.addSuperType(AstNodeUtils.CALL_STATEMENT);
			if (node.getGenericType(CallInternalStatement.class) != null) {
				astNode.addSuperType(AstNodeUtils.CALL_INTERNAL_STATEMENT);
			} else if (node.getGenericType(CallExternalStatement.class) != null) {
				astNode.addSuperType(AstNodeUtils.CALL_EXTERNAL_STATEMENT);
			}
		}
		
		if (node.getGenericType(FieldDefinition.class) != null) {
			astNode.addSuperType(AstNodeUtils.FIELD_DEFINITION);
			final FieldDefinition<?> field = (FieldDefinition<?>) node;
			final var nodeProperties = astNode.properties.getOrSet(HashMap::new);
			AstService.Properties.FIELD_NAME.setIn(nodeProperties, field.getFieldName());
			AstService.Properties.LANGUAGE_TYPE.setIn(nodeProperties, field.getFormat().getLanguageType());
			AstService.Properties.BYTE_LENGTH.setIn(nodeProperties, field.getFormat().getByteLength());
			handle(field, astNode);
		}

		if (node.getGenericType(FieldReference.class) != null) {
			astNode.addSuperType(FIELD_REFERENCE);
			handle((FieldReference<?>) node, astNode);
		} else if (node.getGenericType(ConstantReference.class) != null) {
			astNode.addSuperType(CONSTANT_REFERENCE);
		}
		if (node.getGenericType(AssigningStatement.class) != null) {
			astNode.addSuperType(ASSIGNING_STATEMENT);
		}
		if (node.getGenericType(ArithmeticExpression.class) != null) {
			astNode.addSuperType(ARITHMETIC_EXPRESSION);
		}
		if (node.getGenericType(BindingNode.class) != null) {
			astNode.addSuperType(BINDING_NODE);
		}
		
		if (node.getGenericType(ErrorProcessingStatement.class) != null) {
			astNode.addSuperType(ERROR_PROCESSING_STATEMENT);
		}
		if (node.getGenericType(CfgCollapsibleNode.class) != null) {
			astNode.addSuperType(CFG_COLLAPSIBLE_NODE);
		}
	}
	
	/**
	 * Stores the given {@code node} and {@code astNode} in the {@code dataFieldToAstNode} map of this function.
	 *
	 * @param node the node from parser
	 * @param astNode the {@link StoreAstPrototype}
	 */
	protected void handle(final FieldDefinition<?> node, final StoreAstPrototype astNode) {
		dataFieldToAstNode.put(node, astNode);
	}

	/**
	 * If the given {@link FieldReference} {@code node} has a {@link FieldDefinition} and the {@code dataFieldToAstNode} map of this function has an entry for
	 * it then a REFERS edge is created from the given {@code astNode} to the {@code astNode} of the {@link FieldDefinition}.
	 *
	 * @param node the node from parser
	 * @param astNode the {@link StoreAstPrototype}
	 */
	protected void handle(final FieldReference<?> node, final StoreAstPrototype astNode) {
		node.getFieldDefinition().ifPresent(field -> {
			final StoreAstPrototype dataFieldAstNode = dataFieldToAstNode.get(field);
			if (dataFieldAstNode != null) {
				astNode.addRelationshipTo(dataFieldAstNode, AstRelationshipType.REFERS);
			}
		});
	}

	/**
	 * Returns a Map from AstNodes to StoreAstNodes which map the AstNodes returned after parsing to their representation in the stored AST.
	 * 
	 * @return a map from AstNodes to StoreAstNodes
	 */
	@Override
	public Map<AstNode, StoreAstPrototype> getParserToStoreNodeMap() {
		return parserToStoreNodeMap;
	}

	/**
	 * Returns a list of Binding nodes.
	 *
	 * @return a list of {@linkplain BindingNode Binding Nodes}
	 */
	@Override
	public  List<AstNode> getBindingNodes() {
		return bindingNodes;
	}
}
