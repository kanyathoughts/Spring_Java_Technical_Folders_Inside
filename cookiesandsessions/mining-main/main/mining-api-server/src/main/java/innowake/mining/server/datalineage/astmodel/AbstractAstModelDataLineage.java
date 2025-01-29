/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.astmodel;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.storeast.impl.LabelCreator;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.datalineage.DataFlow;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.DataFlowNodePrototype;
import innowake.mining.shared.model.datalineage.DataLineageResult;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.Statement;
import innowake.ndt.core.parsing.ast.model.statement.FieldDefinition;
import innowake.ndt.core.parsing.ast.model.statement.FieldReference;

/**
 * Base class for data lineage computation using {@link AstModel}.
 */
public abstract class AbstractAstModelDataLineage {

	/**
	 * Describes how a statement moves data between referenced fields. A single {@code FieldAccess} can describe one of the following:
	 * <ul>
	 *     <li>a statement reads a variable but does not derive its output from it. For example, a CALL statement reads the name of the program to call,
	 *     but does not pass that name along further (i.e. does not "output" the name).</li>
	 *     <li>a statement writes a variable but the written variable is not derived from the statement's input</li>
	 *     <li>a statement "moves" data from an input variable to an output variable or sets an output variable based on the value of the input</li>
	 *     <li>a statement writes a constant value to an output variable</li>
	 * </ul>
	 */
	protected static class FieldAccess {

		@Nullable
		private final FieldDefinition<?> input;
		@Nullable
		private final FieldDefinition<?> output;
		@Nullable
		private final String constantValue;

		private FieldAccess(@Nullable final FieldDefinition<?> input, @Nullable final FieldDefinition<?> output, @Nullable final String constantValue) {
			this.input = input;
			this.output = output;
			this.constantValue = constantValue;
		}

		/**
		 * Returns the field that is read by the statement or {@code null}.
		 * @return the field that is read
		 */
		@Nullable
		public FieldDefinition<?> getInput() {
			return input;
		}

		/**
		 * Returns the field that is written by the statement or {@code null}.
		 * @return the field that is written
		 */
		@Nullable
		public FieldDefinition<?> getOutput() {
			return output;
		}

		/**
		 * Returns the constant value that is written to {@link #getOutput()}.
		 * @return the constant value that is written to the output field
		 */
		@Nullable
		public String getConstantValue() {
			return constantValue;
		}

		/**
		 * Indicates that the statement reads the referenced variable but does not pass the value further along.
		 * @param field the field that is read
		 * @return a FieldAccess object
		 */
		protected static FieldAccess reads(final FieldDefinition<?> field) {
			return new FieldAccess(field, null, null);
		}

		/**
		 * Indicates that the statement reads the referenced variable but does not pass the value further along.
		 * @param field the field that is read
		 * @return a FieldAccess object
		 */
		protected static FieldAccess reads(final FieldReference<?> field) {
			return new FieldAccess(field.getFieldDefinition().orElse(null), null, null);
		}

		/**
		 * Indicates that the statement writes the referenced variable but the written value is not derived from any input variable
		 * @param field the field that is written
		 * @return a FieldAccess object
		 */
		protected static FieldAccess writes(final FieldDefinition<?> field) {
			return new FieldAccess(null, field, null);
		}

		/**
		 * Indicates that the statement writes the referenced variable but the written value is not derived from any input variable
		 * @param field the field that is written
		 * @return a FieldAccess object
		 */
		protected static FieldAccess writes(final FieldReference<?> field) {
			return new FieldAccess(null, field.getFieldDefinition().orElse(null), null);
		}

		/**
		 * Indicates that the statement writes the {@code to} variable with a value that is (somehow) derived from the {@code from} variable.
		 * @param from the variable that is used as input (read by the statement)
		 * @param to the variable that is used as output (written by the statement with a variable derived from the input)
		 * @return a FieldAccess object
		 */
		protected static FieldAccess moves(final FieldDefinition<?> from, final FieldDefinition<?> to) {
			return new FieldAccess(from, to, null);
		}

		/**
		 * Indicates that the statement writes the {@code to} variable with a value that is (somehow) derived from the {@code from} variable.
		 * @param from the variable that is used as input (read by) the statement
		 * @param to the variable that is used as output (written by) the statement with a variable derived from the input
		 * @return a FieldAccess object
		 */
		protected static FieldAccess moves(final FieldReference<?> from, final FieldReference<?> to) {
			return new FieldAccess(from.getFieldDefinition().orElse(null), to.getFieldDefinition().orElse(null), null);
		}

		/**
		 * Indicates that the statement writes a constant value to the output field.
		 * @param constantValue a string describing the constant value (e.g. "ZEROES", "LOW-VALUES")
		 * @param to the variable where the constant value is written
		 * @return a FieldAccess object
		 */
		protected static FieldAccess moves(final String constantValue, final FieldDefinition<?> to) {
			return new FieldAccess(null, to, constantValue);
		}

		/**
		 * Indicates that the statement writes a constant value to the output field.
		 * @param constantValue a string describing the constant value (e.g. "ZEROES", "LOW-VALUES")
		 * @param to the variable where the constant value is written
		 * @return a FieldAccess object
		 */
		protected static FieldAccess moves(final String constantValue, final FieldReference<?> to) {
			return new FieldAccess(null, to.getFieldDefinition().orElse(null), constantValue);
		}
	}

	protected final DataLineageContext context;
	protected final AstModel astModel;
	protected final ModuleLightweightPojo module;
	protected final LabelCreator labelCreator;
	private final Map<AstNode, DataFlowNodePrototype> fieldMap;
	private final Map<AstNode, DataFlowNodePrototype> statementMap;
	private final Map<String, DataFlowNodePrototype> constantsMap;

	/**
	 * Constructor for subclass.
	 * @param context contextual information about a data lineage operation.
	 * @param astModel the parsed AstModel of a Module
	 * @param module the Module
	 */
	protected AbstractAstModelDataLineage(final DataLineageContext context, final AstModel astModel, final ModuleLightweightPojo module) {
		this.context = context;
		this.astModel = astModel;
		this.module = module;
		this.labelCreator = new LabelCreator(astModel.getSource());
		this.fieldMap = new HashMap<>();
		this.statementMap = new HashMap<>();
		this.constantsMap = new HashMap<>();
	}

	/**
	 * Execute data lineage computation and return {@link DataLineageResult}.
	 * @return computed {@link DataLineageResult}
	 */
	public DataLineageResult compute() {
		final Optional<AstNode> root = astModel.getRoot();
		if (root.isEmpty()) {
			return DataLineageResult.empty();
		}

		@SuppressWarnings("unchecked")
		final List<FieldDefinition<?>> fieldDefinitions = (List<FieldDefinition<?>>) (List<?>) root.get().getChildrenDeep(FieldDefinition.class);
		final List<Statement> statements = root.get().getChildrenDeep(Statement.class);

		int count = 1;
		for (final FieldDefinition<?> field : fieldDefinitions) {
			context.getProgressMonitor().setStepDescription(
					String.format("Tracing fields in %s (%d/%d)", module.getName(), count, fieldDefinitions.size()));
			processField(field);
			count++;
		}

		count = 1;
		for (final Statement statement: statements) {
			context.getProgressMonitor().setStepDescription(
					String.format("Tracing statements in %s (%d/%d)", module.getName(), count, statements.size()));
			processStatement(statement);
			count++;
		}

		context.getProgressMonitor().setStepDescription("Creating proxy containers on " + module.getName());
		final Collection<DataLineageResult> specialStatementResults = new ArrayList<>();
		for (final Statement statement: statements) {
			context.getProgressMonitor().setStepDescription(
					String.format("Tracing Special statements in %s (%d/%d)", module.getName(), count, statements.size()));
			specialStatementResults.add(handleSpecialStatement(statement));
		}

		context.getProgressMonitor().setStepDescription("Finishing trace of " + module.getName());
		final DataLineageResult afterComputeResult = afterCompute();

		return DataLineageResult
				.ofNodes(fieldMap.values())
				.withStatements(statementMap.values())
				.combinedWith(DataLineageResult.ofNodes(constantsMap.values()))
				.combinedWith(specialStatementResults)
				.combinedWith(afterComputeResult);
	}

	protected void processField(final FieldDefinition<?> field) {
		final DataFlowNodePrototype fieldNode = getDataFlowNodeForField(field);
		getRelatedFields(field).stream()
				.map(relatedField -> getDataFlowNode(relatedField.getAstNode(), labelCreator.getLabel(relatedField.getAstNode())))
				.forEach(relatedField -> fieldNode.addRelatedField(relatedField.getDataFlowId()));
	}

	protected void processStatement(final Statement statement) {
		@SuppressWarnings("unchecked")
		final List<FieldReference<?>> fieldReferences = (List<FieldReference<?>>) (List<?>) statement.getAstNode().getChildrenDeep(FieldReference.class);
		if (fieldReferences.isEmpty()) {
			return;
		}

		final Collection<FieldAccess> fieldAccesses = getFieldAccesses(statement, fieldReferences);

		if (fieldAccesses.isEmpty()) {
			return;
		}

		final DataFlowNodePrototype statementNode = getDataFlowNodeForStatement(statement);
		for (final FieldAccess fieldAccess : fieldAccesses) {
			final FieldDefinition<?> input = fieldAccess.getInput();
			final FieldDefinition<?> output = fieldAccess.getOutput();
			final String constantValue = fieldAccess.getConstantValue();

			if (constantValue != null) {
				final DataFlowNodePrototype constantNode = getDataFlowNodeForConstant(constantValue);
				final DataFlowNodePrototype fieldNode = getDataFlowNodeForField(assertNotNull(output));
				constantNode.addDataFlow(DataFlow.toFieldViaStatement(fieldNode, statementNode));
			} else {
				if (input != null && output != null) {
					final DataFlowNodePrototype inputNode = getDataFlowNodeForField(input);
					final DataFlowNodePrototype outputNode = getDataFlowNodeForField(output);
					inputNode.addDataFlow(DataFlow.toFieldViaStatement(outputNode, statementNode));
				} else if (input != null) {
					final DataFlowNodePrototype inputNode = getDataFlowNodeForField(input);
					inputNode.addDataFlow(DataFlow.toStatement(statementNode));
				} else if (output != null) {
					final DataFlowNodePrototype outputNode = getDataFlowNodeForField(output);
					statementNode.addDataFlow(DataFlow.toField(outputNode));
				}
			}
		}
	}

	protected DataFlowNodePrototype getDataFlowNode(final AstNode astNode, final String name) {
		return fieldMap.computeIfAbsent(astNode, n -> {
			final ModuleLocation moduleLocation = new ModuleLocation(astNode.getStartOffset(), astNode.getLength());
			return new DataFlowNodePrototype(
					DataFlowId.forField(module.getId(), moduleLocation))
					.setName(name)
					.setLocation(moduleLocation);
		});
	}

	protected Optional<DataFlowNodePrototype> getDataFlowNodeFromReference(final FieldReference<?> fieldReference) {
		return fieldReference.getFieldDefinition().map(fieldDefinition ->
				getDataFlowNode(fieldDefinition.getAstNode(), labelCreator.getLabel(fieldDefinition.getAstNode())));
	}

	protected DataFlowNodePrototype getDataFlowNodeForField(final FieldDefinition<?> fieldDefinition) {
		return getDataFlowNode(fieldDefinition.getAstNode(), labelCreator.getLabel(fieldDefinition.getAstNode()));
	}

	protected DataFlowNodePrototype getDataFlowNodeForStatement(final Statement statement) {
		return statementMap.computeIfAbsent(statement.getAstNode(), astNode -> {
			final ModuleLocation moduleLocation = new ModuleLocation(astNode.getStartOffset(), astNode.getLength());
			return new DataFlowNodePrototype(DataFlowId.forStatement(module.getId(), moduleLocation))
					.setName(labelCreator.getLabel(astNode))
					.setLocation(moduleLocation);
		});
	}

	protected DataFlowNodePrototype getDataFlowNodeForConstant(final String constantValue) {
		return constantsMap.computeIfAbsent(constantValue, v -> new DataFlowNodePrototype(DataFlowId.forConstant(module.getId(), constantValue))
				.setName(constantValue));
	}

	/**
	 * Returns the set of fields that are "related to" the given field. "Related" means that if the value of {@code field} is read or modified, then
	 * potentially also the value of any "related field" is read resp. modified. This usually means that the two fields occupy the same or overlapping
	 * memory areas (because they are part of a union, struct, group, redefinition etc.).
	 *
	 * @param field the field for which to determine the related fields
	 * @return the set of related fields
	 */
	protected abstract Set<FieldDefinition<?>> getRelatedFields(final FieldDefinition<?> field);

	/**
	 * Returns the field accesses for the given statement.
	 * <p>
	 * For convenience, the list of field references contained in the statement is passed as well. This is the same list that could be obtained through
	 * {@code statement.getAstNode.getChildrenDeep(FieldReference.class)}.
	 * <p>
	 * Note: if a statement neither reads nor writes a referenced variable, then do not return a {@code FieldAccess} for that reference.
	 *
	 * @param statement the statement to examine
	 * @param fieldReferences a list of field references contained in the statement
	 * @return the field accesses made by the statement
	 */
	protected abstract Collection<FieldAccess> getFieldAccesses(final Statement statement, final List<FieldReference<?>> fieldReferences);

	/**
	 * This method allows to implement special handling for certain statements - typically those that do not only access
	 * local variables but also communicate with the outside world.
	 * <p>
	 * This method is invoked for each statement found in the program. Statements that do not require special handling can simply be ignored.
	 * @param statement the statement to handle
	 * @return computed {@link DataLineageResult}
	 */
	protected abstract DataLineageResult handleSpecialStatement(final Statement statement);

	/**
	 * This method allows to perform a certain operation once after all statements have been processed. It can be used to process generic properties
	 * of the program such as the entry point.
	 * @return computed {@link DataLineageResult}
	 */
	protected DataLineageResult afterCompute() {
		/* does nothing by default */
		return DataLineageResult.empty();
	}
}

