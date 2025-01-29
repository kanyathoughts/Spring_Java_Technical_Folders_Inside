/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.metrics.easytrieve;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;

import easytrieve.EasytrieveParser.AssignmentStatementContext;
import easytrieve.EasytrieveParser.CallStatementContext;
import easytrieve.EasytrieveParser.DeclareStatementContext;
import easytrieve.EasytrieveParser.DefineStatementContext;
import easytrieve.EasytrieveParser.DisplayFileParamContext;
import easytrieve.EasytrieveParser.FileExitParamContext;
import easytrieve.EasytrieveParser.LinkProgramNameContext;
import easytrieve.EasytrieveParser.MoveFormatStatement1Context;
import easytrieve.EasytrieveParser.MstartStatementContext;
import easytrieve.EasytrieveParser.TransferFieldParamContext;
import easytrieve.EasytrieveParser.WriteFromParamContext;
import easytrieve.EasytrieveParser.WriteStatementContext;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;

/**
 *
 * A listener that gathers metrics about the Easytrieve program.
 */
public class EasytrieveDiscoveryMetricsListener extends EasytrieveMetricsListener {

	private class FieldReferenceNode {

		private final Set<FieldReferenceNode> children = new LinkedHashSet<>();
		private final String name;
		private final boolean isField;

		public FieldReferenceNode(final String name, final boolean isField) {
			this.name = name;
			this.isField = isField;
		}

		public Set<FieldReferenceNode> getReferences() {
			return children;
		}

		public String getName() {
			return name;
		}

		public void addReference(final FieldReferenceNode node) {
			/* add new link if not already in set */
			children.add(node);
		}

		@Override
		public String toString() {
			return name;
		}

	}

	private final List<String> inlineMacros = new LinkedList<>();
	private final Map<String, FieldReferenceNode> fieldNameToFieldReferenceTree = new LinkedHashMap<>();
	private final Set<DependencyDefinitionPojo> unvalidatedDependencies = new LinkedHashSet<>();
	private final Set<DependencyDefinitionPojo> unresolvedFieldReferences = new LinkedHashSet<>();
	private final List<ErrorMarker> otherErrors = new LinkedList<>();
	private final MultiValuedMap<String, String> assignmentValues = new ArrayListValuedHashMap<>();
	private final Set<String> unvalidatedDependencyKeys = new LinkedHashSet<>();
	private final Set<String> unresolvedDependencyKeys = new LinkedHashSet<>();
	private final Config config;

	public EasytrieveDiscoveryMetricsListener(final BufferedTokenStream tokens, final Config config) {
		super(tokens);
		this.config = config;
	}

	public Set<DependencyDefinitionPojo> getUnvalidatedDependencies() {
		return unvalidatedDependencies;
	}

	public List<ErrorMarker> getErrors() {
		return otherErrors;
	}

	public List<String> getInlineMacros() {
		return inlineMacros;
	}

	public MultiValuedMap<String, String> getAssignmentValues() {
		return assignmentValues;
	}

	/**
	 * 
	 * NOTE: Must call after parsing entire parse tree.<br>
	 * Will check all field references against any possible values we gathered through assignment, define, similar
	 * statements. If a field can be resolved it will be added to the dependency target.
	 *
	 */
	public void resolveAllFieldToDependencyReferences() {
		final var unresolvedField = unresolvedFieldReferences.iterator();
		while (unresolvedField.hasNext()) {
			boolean isResolved = false;
			final var filter = unresolvedField.next().getModuleFilters().iterator().next();
			final var filterName = filter.getNames().iterator().next();
			final var allPossibleValues = resolveFieldReference(filterName);
			final ModuleType type = filter.getTypes().iterator().next();
			for (final FieldReferenceNode node : allPossibleValues) {
				if (node.isField) {
					otherErrors.add(new ErrorMarker().setCause(String.format("Unable to resolve field %s referenced in a statement.", node.name))
							.setWarningSeverity().setKey(ErrorKey.PARSE_ERROR).validate());
				}
				final var name = getStringValueFromStringLiteral(node.getName());
				addDependency(name, type, Binding.EARLY, unvalidatedDependencies, unvalidatedDependencyKeys);
				/* remove from unresolved list */
				isResolved = true;
			}
			if (isResolved) {
				unresolvedField.remove();
			}
		}
	}

	@Override
	public void exitAssignmentStatement(@Nullable final AssignmentStatementContext ctx) {
		super.exitAssignmentStatement(ctx);
		if (ctx == null)
			return;
		/* set recieving field name */
		final var fieldName = ctx.recieveField().getText();

		/* set field value */
		final Set<FieldReferenceNode> fieldValues = new HashSet<>();
		if (ctx.assignmentParam().field() != null) {
			final var value = ctx.assignmentParam().field().getText();
			fieldValues.add(new FieldReferenceNode(value, true));
		} else if (ctx.assignmentParam().stringLiteral() != null) {
			final var value = ctx.assignmentParam().stringLiteral().getText();
			fieldValues.add(new FieldReferenceNode(value, false));
		} else {
			/* must be arithmetic, don't track */
		}
		addUnreferencedFieldNameAndValuesToMap(fieldName, fieldValues);
	}

	@Override
	public void exitDefineStatement(@Nullable final DefineStatementContext ctx) {
		super.exitDefineStatement(ctx);
		if (ctx == null)
			return;
		/* set field (should we set qualifier or not?) */
		final var fieldName = ctx.defineFieldName().field().getText();
		/* set field values */
		final Set<FieldReferenceNode> fieldValues = ctx.defineCharacteristics().stream().filter(c -> c.defineValue() != null)
				.flatMap(c -> c.defineValue().stream()).filter(v -> v.stringLiteral() != null)
				.map(v -> new FieldReferenceNode(v.stringLiteral().getText(), false)).collect(Collectors.toSet());

		addUnreferencedFieldNameAndValuesToMap(fieldName, fieldValues);
	}

	@Override
	public void exitMoveFormatStatement1(@Nullable final MoveFormatStatement1Context ctx) {
		/* note: don't care abour format 2 because can't save literals from this */
		super.exitMoveFormatStatement1(ctx);

		if (ctx == null)
			return;

		/* get field name */
		final var fieldName = ctx.moveRecieveParam().recieveField().getText();

		/* get field value(s) */
		final Set<FieldReferenceNode> fieldValues = new HashSet<>();
		if (ctx.moveSendParam().assignmentParam().field() != null) {
			final var value = ctx.moveSendParam().assignmentParam().field().getText();
			fieldValues.add(new FieldReferenceNode(value, true));
		} else if (ctx.moveSendParam().assignmentParam().stringLiteral() != null) {
			final var value = ctx.moveSendParam().assignmentParam().stringLiteral().getText();
			fieldValues.add(new FieldReferenceNode(value, false));
		} else {
			/* must be arithmetic, don't track */
		}

		addUnreferencedFieldNameAndValuesToMap(fieldName, fieldValues);
	}

	@Override
	public void exitCallStatement(final @Nullable CallStatementContext ctx) {
		super.exitCallStatement(ctx);
		if (ctx == null)
			return;

		/* NOTE: we could just handle this by overwriting the enterProgramName method but then
		 * we'd miss an opportunity to get the call information. Since we've saved attributes in
		 * the past, we will likely want to do the same thing here */
		if (ctx.programName() != null) {
			final var progName = ctx.programName().getText();
			final var moduleType = config.getUtilityList().isUtility(progName) ? ModuleType.UNKNOWN_UTILITY : ModuleType.UNKNOWN_PROGRAM;
			addDependency(progName, moduleType, Binding.EARLY, unvalidatedDependencies, unvalidatedDependencyKeys);
		}
	}

	@Override
	public void exitLinkProgramName(final @Nullable LinkProgramNameContext ctx) {
		super.exitLinkProgramName(ctx);
		if (ctx == null)
			return;
		if (ctx.programNameLiteral() != null) {
			final var progName = getStringValueFromStringLiteral(ctx.programNameLiteral().getText());
			addDependency(progName, ModuleType.UNKNOWN_PROGRAM, Binding.EARLY, unvalidatedDependencies, unvalidatedDependencyKeys);
		} else if (ctx.programField() != null) {
			final var progName = ctx.programField().getText();
			addDependency(progName, ModuleType.UNKNOWN_PROGRAM, Binding.EARLY, unresolvedFieldReferences, unresolvedDependencyKeys);
		}
	}

	@Override
	public void exitTransferFieldParam(final @Nullable TransferFieldParamContext ctx) {
		super.exitTransferFieldParam(ctx);
		if (ctx == null)
			return;
		if (ctx.programNameLiteral() != null) {
			final var progName = getStringValueFromStringLiteral(ctx.programNameLiteral().getText());
			addDependency(progName, ModuleType.UNKNOWN_PROGRAM, Binding.EARLY, unvalidatedDependencies, unvalidatedDependencyKeys);
		}
		if (ctx.programField() != null) {
			final var progName = ctx.programField().getText();
			addDependency(progName, ModuleType.UNKNOWN_PROGRAM, Binding.EARLY, unresolvedFieldReferences, unresolvedDependencyKeys);
		}
	}

	@Override
	public void exitDeclareStatement(final @Nullable DeclareStatementContext ctx) {
		super.exitDeclareStatement(ctx);
		if (ctx == null)
			return;
		if (ctx.declareProgramParam() != null) {
			final var progName = getStringValueFromStringLiteral(ctx.field().getText());
			addDependency(progName, ModuleType.UNKNOWN_PROGRAM, Binding.EARLY, unvalidatedDependencies, unvalidatedDependencyKeys);
		}
	}

	@Override
	public void exitFileExitParam(final @Nullable FileExitParamContext ctx) {
		super.exitFileExitParam(ctx);
		if (ctx == null)
			return;
		final var progName = ctx.programNameParam().programName().getText();
		addDependency(progName, ModuleType.UNKNOWN_PROGRAM, Binding.EARLY, unvalidatedDependencies, unvalidatedDependencyKeys);
	}

	@Override
	public void exitDisplayFileParam(final @Nullable DisplayFileParamContext ctx) {
		super.exitDisplayFileParam(ctx);
		if (ctx == null)
			return;
		if (ctx.fileNameParam() != null) {
			final var fileName = ctx.fileNameParam().fileName().getText();
			addDependency(fileName, ModuleType.RESOURCE_FILE, Binding.EARLY, unvalidatedDependencies, unvalidatedDependencyKeys);
		}
	}

	@Override
	public void exitWriteStatement(final @Nullable WriteStatementContext ctx) {
		super.exitWriteStatement(ctx);
		if (ctx == null)
			return;
		final var fileName = ctx.fileName().getText();
		addDependency(fileName, ModuleType.RESOURCE_FILE, Binding.EARLY, unvalidatedDependencies, unvalidatedDependencyKeys);
	}

	@Override
	public void exitWriteFromParam(final @Nullable WriteFromParamContext ctx) {
		super.exitWriteFromParam(ctx);
		if (ctx == null)
			return;
		if (ctx.fileName() != null) {
			final var fileName = ctx.fileName().getText();
			addDependency(fileName, ModuleType.RESOURCE_FILE, Binding.EARLY, unvalidatedDependencies, unvalidatedDependencyKeys);
		} else if (ctx.field() != null) {
			final var recordName = ctx.field().getText();
			addDependency(recordName, ModuleType.RESOURCE_RECORD, Binding.EARLY, unvalidatedDependencies, unvalidatedDependencyKeys);
		}
	}

	@Override
	public void exitMstartStatement(final @Nullable MstartStatementContext ctx) {
		super.exitMstartStatement(ctx);
		if (ctx != null) {
			this.inlineMacros.add(ctx.inlineMacroName().getText().trim());
		}
	}

	@Override
	public void enterAssignmentStatement(final @Nullable AssignmentStatementContext ctx) {
		super.enterAssignmentStatement(ctx);
		if (ctx == null || ctx.assignmentParam() == null)
			return;
		assignmentValues.put(ctx.getStart().getText(), ctx.assignmentParam().getText().replace("\'", " ").trim());
	}

	private String getStringValueFromStringLiteral(final String stringLiteral) {
		return stringLiteral.replace("'", "");
	}

	private void addDependency(final String name, final ModuleType moduleType, final Binding binding, 
			final Set<DependencyDefinitionPojo> dependencies, final Set<String> dependencyDefinitionKeys) {
		final var origin = (moduleType.getType() == Type.UTILITY) ? Origin.ENVIRONMENT : null;
		final var filter = new ModuleFilter().setNames(name).setTypes(moduleType).setOrigin(origin);
		final var technology = filter.getTypes().iterator().next().getTechnology();
		final var type = filter.getTypes().iterator().next().getType();
		
		final var relationship = RelationshipType.from(technology, type);
		final var uniqueKey = String.format("%s%s%s%s%s", name, moduleType, binding, relationship, origin);
		if (dependencyDefinitionKeys.add(uniqueKey)) {
			final var dependencyDefinition = new DependencyDefinitionPojo(UUID.randomUUID(), binding, List.of(filter), relationship);
			dependencies.add(dependencyDefinition);
		}
	}

	private void addUnreferencedFieldNameAndValuesToMap(final String fieldName, final Set<FieldReferenceNode> nodeValues) {
		if (nodeValues.isEmpty()) {
			return;
		}

		/* if field name is already in map */
		if (fieldNameToFieldReferenceTree.containsKey(fieldName)) {
			/* add new value to set of values for this field */
			nodeValues.forEach(node -> fieldNameToFieldReferenceTree.get(fieldName).addReference(node));
		} else {
			/* create new set and add with field as new entry into map */
			final var fieldReference = new FieldReferenceNode(fieldName, true);
			nodeValues.forEach(fieldReference::addReference);
			fieldNameToFieldReferenceTree.put(fieldReference.name, fieldReference);
		}
	}

	private Set<FieldReferenceNode> resolveFieldReference(final String fieldName) {

		/* get field in map */
		final var fieldNode = fieldNameToFieldReferenceTree.get(fieldName);

		/* if not in map or not a tree, return field as set */
		if (fieldNode == null || fieldNode.getReferences().isEmpty()) {
			final Set<FieldReferenceNode> setOfOne = new HashSet<>();
			setOfOne.add(new FieldReferenceNode(fieldName, true));
			return setOfOne;
		}

		/* if in map, get set of values for this field */
		final Set<FieldReferenceNode> references = fieldNode.getReferences();

		/* create list of all fields we have not visited */
		final Set<FieldReferenceNode> unvisited = new LinkedHashSet<>(references);
		Iterator<FieldReferenceNode> unvisitedIter = unvisited.iterator();

		/* list for all fields that don't have children */
		final Set<FieldReferenceNode> retFields = new LinkedHashSet<>();

		/* until we visit all fields...*/
		while (unvisitedIter.hasNext()) {
			/* for each field we have not visited:
			 * get field in map if there and add all referenced fields to both sets (unless equal to parent field name), 
			 * essentially "flattening" set of fields */

			final var child = unvisitedIter.next();

			/* we've visited this so remove */
			unvisitedIter.remove();

			if (fieldNameToFieldReferenceTree.containsKey(child.getName())) {
				final var resolvedChild = fieldNameToFieldReferenceTree.get(child.getName());
				/* add all this child's references (just the first level) */
				final var childRefs = resolvedChild.getReferences().stream().filter(r -> !r.getName().equals(fieldName)).collect(Collectors.toSet());

				if ( ! childRefs.isEmpty()) {
					references.addAll(childRefs);
					unvisited.addAll(childRefs);

					/* reset iterator */
					unvisitedIter = unvisited.iterator();
				}
			} else {
				retFields.add(child);
			}
		}

		/* return all references that don't have children */
		return retFields;
	}
}
