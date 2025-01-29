/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.pl1;

import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.CALL_TYPE;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.DB_ACCESS_OPERATION;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.DB_ACCESS_TYPE;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeKey.STATEMENT;
import static innowake.mining.data.model.discovery.attribute.ModelAttributeValue.CallType.IMS_DB;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.attribute.ModelAttributeComparator;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.contributors.ims.ImsMetricsUtil;
import innowake.mining.server.discovery.metrics.DawnOriginResolver;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.AstUtil;
import innowake.ndt.pl1parser.ast.model.Pl1Model;
import innowake.ndt.pl1parser.ast.model.declaration.BaseDeclaration;
import innowake.ndt.pl1parser.ast.model.declaration.NamedVariableDeclaration;
import innowake.ndt.pl1parser.ast.model.declaration.StructureDeclaration;
import innowake.ndt.pl1parser.ast.model.expression.BaseExpression;
import innowake.ndt.pl1parser.ast.model.expression.ConstantExpression;
import innowake.ndt.pl1parser.ast.model.expression.ReferenceExpression;
import innowake.ndt.pl1parser.ast.model.reference.BaseCallReference;
import innowake.ndt.pl1parser.ast.model.reference.BaseReference;
import innowake.ndt.pl1parser.ast.model.reference.BasicReference;
import innowake.ndt.pl1parser.ast.model.reference.FieldReference;
import innowake.ndt.pl1parser.ast.model.reference.LocationQualifiedReference;
import innowake.ndt.pl1parser.ast.model.reference.QualifiedReference;
import innowake.ndt.pl1parser.ast.model.reference.Subscript;
import innowake.ndt.pl1parser.ast.model.statement.AssignmentStatement;
import innowake.ndt.pl1parser.ast.model.statement.CallStatement;
import innowake.ndt.pl1parser.ast.model.statement.DeclareStatement;
import innowake.ndt.pl1parser.ast.model.statement.block.ProcedureBlock;
import org.apache.commons.collections4.MultiValuedMap;
import org.apache.commons.collections4.multimap.HashSetValuedHashMap;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * This will provide all the information regarding IMS Utilities to Cobol to create the dependencies transitively with IMS.
 */
public class Pl1ImsCollector {

	final DiscoveryBuilder.ModuleBuilder moduleBuilder;
	final DiscoveryBuilder discoveryBuilder;
	final Pl1Model model;
	final DawnOriginResolver originResolver;
	final Config config;

	final MultiValuedMap<String, Tuple2<ModuleLocation,ModelAttributeMap<Object>>> imsUtilities;

	Pl1ImsCollector(final DiscoveryBuilder.ModuleBuilder moduleBuilder, final DiscoveryBuilder discoveryBuilder, final Pl1Model model,
			final DawnOriginResolver originResolver,
			final Config config) {
		this.moduleBuilder = moduleBuilder;
		this.discoveryBuilder = discoveryBuilder;
		this.model = model;
		this.originResolver = originResolver;
		this.config = config;
		imsUtilities = new HashSetValuedHashMap<>();
	}

	/**
	 * Performs the collection of PL/I IMS metrics.
	 */
	void collectMetrics() {
		final var optRoot = model.getRoot();
		if (optRoot.isEmpty()) {
			return;
		}

		final var root = optRoot.get();
		root.getChildrenDeep(CallStatement.class).forEach(call -> {

			BaseReference reference = call.getReference();
			reference = reference instanceof FieldReference ? ((FieldReference) reference).getReference() : reference;

			final String targetName = reference.getIdentifier();
			if ( ! AbstractPl1Contributor.isUtility(targetName, config.getUtilityList())) {
				return;
			}

			/* THIS IS A DIRTY HACK FOR WMIN-4435 AND MUST BE REMOVED WITH WMIN-4441 */
			if ("P$$$ZZ".equals(targetName)) {
				final var arguments = getCallArguments(reference);
				if (arguments.size() >= 3) {
					final String functionName = resolveFunctionName(call, arguments.get(2));
					addImsCallDependency(targetName, call, functionName);
				}
				return;
			}

			/* we're only interested in calls to IMS utilities that are valid from within PL/I */
			final var imsUtil = ImsMetricsUtil.resolveImsUtility(targetName);
			if (imsUtil != ImsMetricsUtil.ImsUtility.AIBTDLI && imsUtil != ImsMetricsUtil.ImsUtility.CEETDLI && imsUtil != ImsMetricsUtil.ImsUtility.PLITDLI) {
				return;
			}
			final var arguments = getCallArguments(reference);
			/* there must be at least two parameters to be treated as a valid call, providing the argument counter followed by the actual function */
			if (arguments.size() >= 2) {
				final String functionName = resolveFunctionName(call, arguments.get(1));
				addImsCallDependency(targetName, call, functionName);
			}
		});
	}

	private List<BaseExpression> getCallArguments(final BaseReference callReference) {
		List<BaseExpression> arguments = Collections.emptyList();
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
	private String resolveFunctionName(final CallStatement call, final BaseExpression functionArg) {
		if (functionArg instanceof ConstantExpression) {
			/* function is a character constant: CALL PLITDLI (3, 'GU', DB_PTR_MAST, DET_SEG_IO_AREA) */
			final String functionName = ((ConstantExpression) functionArg).toString().trim();
			return functionName.substring(1, functionName.length() - 1).trim();
		} else if (functionArg instanceof ReferenceExpression) {
			final BaseReference functionReference = ((FieldReference) ((ReferenceExpression) functionArg).getReference()).getReference();
			final String resolvedFunctionReference = resolveFieldReference(functionReference);
			if (resolvedFunctionReference == null) {
				return null;
			}

			if (resolvedFunctionReference.contains(".")) {
				return resolveImsFunctionFromQualifiedField(call.getStartOffset(), call, resolvedFunctionReference);
			}
			return resolveImsFunctionFromField(call.getStartOffset(), call, resolvedFunctionReference);
		}
		return null;
	}

	@Nullable
	private String resolveImsFunctionFromField(final int callStartOffset, final AstNode node, final String name) {
		final ProcedureBlock parentProcedure = AstUtil.findParent(node.getParent(), ProcedureBlock.class);
		if (parentProcedure == null) {
			return null;
		}

		/* we do first check if we can find a field declaration matching the given unqualified field name, walking the AST upwards from the current
		 * procedure the call statement is located in, until reaching the main procedure. */
		final BaseDeclaration resolvedField = findUnqualifiedField(node, name, false);
		String value = null;
		/* if the field declaration either provides a default INIT or constant VALUE, we use that as the IMS function. if no value could be located,
		 * we try to search if we can find any assignment to the field in question by also walking the AST upwards from the current procedure. */
		if (resolvedField != null) {
			value = resolveImsFunctionFromInitValue(resolvedField);
		}
		if (value == null) {
			value = resolveImsFunctionFromAssignment(callStartOffset, parentProcedure, name);
		}
		return value;
	}

	@Nullable
	private String resolveImsFunctionFromInitValue(final BaseDeclaration declaration) {
		final var optInitVal = declaration.getInitializedValue();
		if (optInitVal.isEmpty()) {
			return null;
		}

		final var initItems = optInitVal.get().getInitializedItems();
		if (initItems.isEmpty()) {
			return null;
		}

		/* we currently only use the first element, even though it might be an array with multiple init values, as we don't resolve array accesses */
		final var optItem = initItems.get(0).getItem();
		if (optItem.isEmpty()) {
			return null;
		}

		final var item = optItem.get();
		if (item instanceof ConstantExpression) {
			final String value = ((ConstantExpression) item).toString().trim();
			return value.substring(1, value.length() - 1).trim();
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
	private String resolveImsFunctionFromQualifiedField(final int callStartOffset, final AstNode node, final String qualifiedName) {
		final BaseDeclaration resolvedQualifiedField = findQualifiedField(node, qualifiedName);
		if (resolvedQualifiedField != null) {
			final String initValue = resolveImsFunctionFromInitValue(resolvedQualifiedField);
			if (initValue != null) {
				return initValue;
			}
		}

		/* if there's no field providing an initial value or no field could be found at all, we try to locate an assignment to the qualified field */
		final ProcedureBlock parentProcedure = AstUtil.findParent(node.getParent(), ProcedureBlock.class);
		if (parentProcedure != null) {
			return resolveImsFunctionFromAssignment(callStartOffset, parentProcedure, qualifiedName);
		}

		return null;
	}

	@Nullable
	private String resolveImsFunctionFromAssignment(final int callStartOffset, final AstNode node, final String fieldName) {
		/* get all assignments scoped to the current procedure that are positioned before the actual call statement */
		final var assignments = node.getChildren(AssignmentStatement.class).stream()
				.filter(as -> as.getEndOffset() < callStartOffset)
				.collect(Collectors.toList());
		for (final AssignmentStatement assignment : assignments) {
			for (BaseReference ref : assignment.getReferences()) {
				final String assignmentFieldName = getAssignmentFieldName(ref);

				/* we expect that the field reference used in the assignment is exactly the same as for the call argument.
				 * meaning if the field is unqualified in the assignment statement, but qualified in the call argument, that it will not be matched.
				 * this would otherwise require us to analyze access scopes throughout all procedures to determine if both fields could be the same. */
				if (fieldName.equals(assignmentFieldName)) {
					final BaseExpression valueExpression = assignment.getExpression();
					if (valueExpression instanceof ConstantExpression) {
						final String value = ((ConstantExpression) valueExpression).toString().trim();
						return value.substring(1, value.length() - 1).trim();
					}
				}
			}
		}

		/* try the parent procedure if nothing could be located in the current one */
		final ProcedureBlock parentProcedure = AstUtil.findParent(node.getParent(), ProcedureBlock.class);
		if (parentProcedure != null) {
			return resolveImsFunctionFromAssignment(callStartOffset, parentProcedure, fieldName);
		}
		return null;
	}

	@Nullable
	private String getAssignmentFieldName(BaseReference ref) {
		ref = ref instanceof FieldReference ? ((FieldReference) ref).getReference() : ref;
		if (ref instanceof QualifiedReference) {
			return assembleQualifiedReference((QualifiedReference) ref);
		} else if (ref instanceof LocationQualifiedReference) {
			return assembleLocationQualifiedReference((LocationQualifiedReference) ref);
		}
		return ref.getIdentifier();
	}

	@Nullable
	private String assembleQualifiedReference(final QualifiedReference qualifiedReference) {
		final var locatorReference = qualifiedReference.getLocatorReference();
		final var basedReference = qualifiedReference.getBasedReference();
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
	private BaseDeclaration findUnqualifiedField(final AstNode node, final String name, final boolean preferGroupAboveField) {
		/* an unqualified field can be directly on level 1 as a plain or factored declaration together with other fields.
		 * PL/I also allows unqualified field references, even though it's declared in a group, as long as it's unique in the current scope.
		 * this must be scoped to the current procedure, as any declaration in nested procedures is not in valid access scope! */
		final var fieldDeclaration = node.getChildren(DeclareStatement.class).stream()
				.map(ds -> ds.getChildrenDeep(NamedVariableDeclaration.class))
				.flatMap(List::stream)
				.filter(nvd -> nvd.getVariableName().equals(name))
				.findFirst();
		/* the field in question could also be a group declaration */
		final var structureDeclaration = node.getChildren(DeclareStatement.class).stream()
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
	private BaseDeclaration findQualifiedField(final AstNode node, final String qualifiedName) {
		final String[] parts = qualifiedName.split("\\.");
		/* we first locate the actual group declaration by using the base qualifier of the field */
		final var baseStructure = findUnqualifiedField(node, parts[0], true);
		if (baseStructure instanceof StructureDeclaration) {
			/* then we check if we can find the remainder of the qualification chain inside this group declaration */
			return findQualifiedFieldInStructure(baseStructure, ArrayUtils.remove(parts, 0));
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

	private void addImsCallDependency(final String utilName, final CallStatement call, @Nullable final String function) {
		final ModelAttributeMap<Object> attributeMap = new ModelAttributeMap<>(ModelAttributeComparator.getKeyComparator());
		final var originalLocation = originResolver.resolveLocation(call);

		/* explicitly not using the original location here, as we want the statement as contained in the full assembled program */
		final String statement = StringUtils.normalizeSpace(model.getSource().substring(call.getStartOffset(), call.getEndOffset())).trim();
		if (function != null) {
			attributeMap.put(DB_ACCESS_TYPE, mapDbAccessType(function));
			attributeMap.put(DB_ACCESS_OPERATION, function);
		} else {
			final var originalSource = originResolver.resolve(call, discoveryBuilder, moduleBuilder);
			/* we currently don't have access to the actual line number. therefore adding the whole statement */
			originalSource.addError(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY, "Unable to identify IMS function for statement: " + statement);
		}
		attributeMap.put(STATEMENT, statement);
		attributeMap.put(CALL_TYPE, IMS_DB);
		imsUtilities.put(utilName, new Tuple2<>(originalLocation, attributeMap));
	}

	public static DatabaseAccessType mapDbAccessType(final String function) {
		switch (function) {
			case "GU":
			case "GHU":
			case "GN":
			case "GHN":
			case "GNP":
			case "GHNP":
				return DatabaseAccessType.READ;
			case "ISRT":
				return DatabaseAccessType.STORE;
			case "DLET":
				return DatabaseAccessType.DELETE;
			case "REPL":
				return DatabaseAccessType.UPDATE;
			default:
				return DatabaseAccessType.OTHER;
		}
	}

	public MultiValuedMap<String, Tuple2<ModuleLocation, ModelAttributeMap<Object>>> getImsUtilities() {
		return imsUtilities;
	}
}
