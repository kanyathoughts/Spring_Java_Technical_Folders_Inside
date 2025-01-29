/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.contributors.resolver.cobol;

import static innowake.mining.server.discovery.metrics.MetricsUtility.trimQuotesSpaces;
import static java.util.Collections.emptyList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.mining.server.discovery.dawn.metrics.contributors.AstNodeLocationProvider;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;

import innowake.lib.calcite.sql.parser.SqlParseException;
import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.metrics.exec.ExecSqlCollector;
import innowake.mining.server.discovery.metrics.exec.ExecStatementUtility;
import innowake.mining.server.discovery.parser.sql.SimpleSqlCollector;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.cobol.parser.ast.model.CobolConstantReference;
import innowake.ndt.cobol.parser.ast.model.CobolDataField;
import innowake.ndt.cobol.parser.ast.model.CobolExpression;
import innowake.ndt.cobol.parser.ast.model.CobolFieldReference;
import innowake.ndt.cobol.parser.ast.model.CobolReference;
import innowake.ndt.cobol.parser.ast.model.CobolReferenceExpression;
import innowake.ndt.cobol.parser.ast.statement.CobolCallStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolMoveStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSetStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolSingleSetContainer;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsCreateTransaction;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsDeleteQTd;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsDeleteQTs;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsFile;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReadQTd;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReadQTs;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsReturn;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsStatement;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsWriteQTd;
import innowake.ndt.cobol.parser.ast.statement.exec.cics.ExecCicsWriteQTs;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.exec.ExecNode;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlCall;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareSchema;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlDeclareTable;
import innowake.ndt.core.parsing.ast.model.exec.sql.ExecSqlLockTable;

/**
 * Default implementation for Cobol reference resolver.
 */
public class DefaultCobolReferenceResolver implements CobolReferenceResolver {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);
	private static final int LEVEL_88 = 88;

	private final List<CobolMoveStmt> moves;
	private final List<CobolDataField> data;
	private final List<CobolSetStmt> sets;
	private Map<String, List<Object>> referencedTables = new HashMap<>();
	private final SourcePojo sourceObject;
	private final AstNodeLocationProvider<SourcePojo> astNodeLocationProvider;

 
	public DefaultCobolReferenceResolver(final List<CobolMoveStmt> moves, final List<CobolDataField> data, final List<CobolSetStmt> sets,
			final SourcePojo sourceObject, final AstNodeLocationProvider<SourcePojo> astNodeLocationProvider) {
		this.moves = moves;
		this.data = data;
		this.sets = sets;
		this.sourceObject = sourceObject;
		this.astNodeLocationProvider = astNodeLocationProvider;
	}

    @Override
    public List<String> resolve(@Nullable final AstNode reference) {
    	final List<String> result = new ArrayList<>(); 
        if (reference instanceof CobolConstantReference) {
            final String name = resolveName((CobolConstantReference) reference);
            result.add(name);
     
        } else if (reference instanceof CobolFieldReference) {
            result.addAll(resolveNames((CobolFieldReference) reference, null));
             
        }
        return result;
    }	
    
    /**
	 *
	 * Resolve EXEC SQL statement, adding all referenced tables.
	 *
	 * @param sqlNode the exec sql statement node
	 * @param rootModule is {@link ModuleBuilder} to add an error
	 * @return list of references (in this case, tables in SQL statement)
	 */
	@SuppressWarnings("unchecked")
	public <T> List<String> resolveExecSQL(final T sqlNode, final ModuleBuilder rootModule, final boolean isDebugging) {
		final boolean isInstanceOfExecSqlDeclareSchema = sqlNode instanceof ExecSqlDeclareSchema;
		
		if (sqlNode instanceof ExecSqlLockTable || sqlNode instanceof ExecSqlDeclareTable || isInstanceOfExecSqlDeclareSchema) {
			final var sqlCollector = new ExecSqlCollector();
			sqlCollector.handleExecSql((ExecNode<?>) sqlNode);
			referencedTables = (Map<String, List<Object>>) (Map<?, ?>) sqlCollector.getReferencedTables();
		} else if (sqlNode instanceof ExecSqlCall) {
			/* declare type as SQL_STORED_PROCEDURE */
			return resolve(((ExecSqlCall<?>) sqlNode).getProcedureName())
					.stream()
					.collect(Collectors.toList());
		} else {
			final String contents = ExecStatementUtility.getExecStatement((ExecNode<?>) sqlNode, isDebugging);
			try {
				referencedTables = (Map<String, List<Object>>) (Map<?, ?>) SimpleSqlCollector.getSqlTables(contents);
			} catch (final SqlParseException e) {
				final String errorMessage = SimpleSqlCollector.getErrorMessage(e, contents);
				rootModule.addError(Severity.ERROR, ErrorKey.PARSE_ERROR, errorMessage, astNodeLocationProvider.
						getAstNodeLocation((ExecNode<?>) sqlNode));
			} 
		}
		
		return referencedTables.keySet().stream()
				.collect(Collectors.toList());
	}
	
	/**
	 * Resolves reference entries from a Cobol expression.
	 * 
	 * @param module {@link ModuleBuilder}
	 * @param expression a Cobol expression
	 * @return the resolved references
	 */
	public List<String> resolve(final ModuleBuilder module, final CobolExpression expression) {
		if (expression instanceof CobolReferenceExpression) {
			return resolve(module, ((CobolReferenceExpression) expression).getOp1());
		}
		return emptyList();
	}
	
	/**
	 * Resolves reference entries from a Cobol reference.
	 *
	 * @param module {@link ModuleBuilder}
	 * @param reference a Cobol reference
	 * @return the resolved references
	 */
	public List<String> resolve(final ModuleBuilder module, final CobolReference reference) {
		if (reference instanceof CobolFieldReference) {
			return resolveNames((CobolFieldReference) reference, module).stream()
			.filter(StringUtils::isNotBlank)
			.collect(Collectors.toList());
		} else if (reference instanceof CobolConstantReference) {
			final String name = resolveName((CobolConstantReference) reference);
			if (StringUtils.isNotBlank(name)) {
				return Collections.singletonList(name);
			}
		}
		return Collections.emptyList();
	}

	private static boolean contains(final CobolExpression toField, final String uniqueId) {
		if (toField instanceof CobolReferenceExpression) {
			final CobolReferenceExpression referenceExpression = (CobolReferenceExpression) toField;
			final CobolReference reference = referenceExpression.getOp1();
			if (reference instanceof CobolFieldReference) {
				final CobolFieldReference candidate = (CobolFieldReference) reference;
				return getUniqueId(candidate).endsWith(uniqueId);
			}
		}
		return false;
	}
	
	private static String getUniqueId(final CobolFieldReference reference) {
		final CobolDataField field = reference.getField();

		if (field == null) {
			return reference.getUnresolvedFieldName();
		}
		return getUniqueId(field);
	}
	
	private static String getUniqueId(final CobolDataField field) {
		final var name = new StringBuilder(field.getName() != null ? field.getName() : StringUtils.EMPTY);
		AstNode parent = field.getParent();

		while (parent instanceof CobolDataField) {
			name.append(".").append(((CobolDataField) parent).getName());
			parent = parent.getParent();
		}
		return name.toString();
	}
	
	/**
	 * Returns the reference tables and their attributes.
	 * 
	 * @return the reference tables and their attributes.
	 */
	public Map<String, List<Object>> getReferencedTables() {
		return referencedTables;
	}

	/**
	 * 
	 * Resolve the queue CICS statement and return queue name. 
	 *
	 * @param queueCicsStatement The general ExecCics statement that is an instance of one of the ExecCics QTd/QTs statements.
	 * @return resolved queue name
	 */
	public String resolveQueue(final ExecCicsStatement queueCicsStatement) {
		return getQueueName(queueCicsStatement);
	}

	/**
	 * Retrieves a queue name from an arbitrary ExecCicsStatement with the assumption that the statement is 
	 * an instance of one of the Exec Cics QTd/QTs statements.
	 *
	 * @param queueCicsStatement The general ExecCicsStatement that is an instance of one of the ExecCics QTd/QTs statements.
	 * @return The queue name from the ExecCicsStatement if it exists, otherwise an empty string is returned.
	 */
	private static String getQueueName(@Nullable final ExecCicsStatement queueCicsStatement) {

		final CobolExpression queue = getQueue(queueCicsStatement);

		if (queue != null && StringUtils.isNotBlank(queue.toString())) {
			return getQueueName(queue);
		} else {
			if (LOG.isErrorEnabled()) {
				LOG.error("Unable to resolve queue name for statement {}", queueCicsStatement);
			}
			return "";
		}
	}
	
	private static String getQueueName(final CobolExpression queue) {
		var queueName = queue.toString();

		/* Try to resolve the actual value of the queue name if possible, instead of just the variable name */
		if (queue instanceof CobolReferenceExpression) {
			final CobolReference op1 = ((CobolReferenceExpression) queue).getOp1();
			if (op1 instanceof CobolFieldReference) {
				final CobolDataField dataField = ((CobolFieldReference) op1).getField();
				if (dataField != null) {
					final List<Object> values = dataField.getValues();
					if (CollectionUtils.isNotEmpty(values)) {
						queueName = values.get(0).toString();
					} else if (StringUtils.isNotBlank(dataField.getName())) {
						queueName = dataField.getName().trim();
					}
				}
			}
		}

		return trimQuotesSpaces(queueName);
	}
	
	@Nullable
	private static CobolExpression getQueue(final ExecCicsStatement queueCicsStatement) {
		CobolExpression queue = null;
		if (queueCicsStatement instanceof ExecCicsDeleteQTd) {
			queue = ((ExecCicsDeleteQTd) queueCicsStatement).getQueue();
		} else if (queueCicsStatement instanceof ExecCicsReadQTd) {
			queue = ((ExecCicsReadQTd) queueCicsStatement).getQueue();
		} else if (queueCicsStatement instanceof ExecCicsWriteQTd) {
			queue = ((ExecCicsWriteQTd) queueCicsStatement).getQueue();
		} else if (queueCicsStatement instanceof ExecCicsDeleteQTs) {
			queue = ((ExecCicsDeleteQTs) queueCicsStatement).getQueue();
		} else if (queueCicsStatement instanceof ExecCicsReadQTs) {
			queue = ((ExecCicsReadQTs) queueCicsStatement).getQueue();
		} else if (queueCicsStatement instanceof ExecCicsWriteQTs) {
			queue = ((ExecCicsWriteQTs) queueCicsStatement).getQueue();
		}
		return queue;
	}
	
	/*
	 * THIS IS A TEMPORARY FIX FOR THE TICKET WMIN-2761, complete solution will be
	 * possible when the Data Lineage feature is available.
	 * 
	 * finds and adds the nearest upstream MOVE when fieldReference is type of a call statement,
	 * else will add all the matching upstream MOVEs.
	 */
	private void findRelevantUpstreamMoves(final CobolFieldReference fieldReference, final String rootVariableName, final List<String> result,
			final boolean firstCall) {
		/* Resolve the value of the field reference from working storage section
		 if it is the first MOVE statement */
		if ( ! firstCall && isRootMove(fieldReference)) {
			result.addAll(resolveRefNames(fieldReference));
			return;
		}

		for (int i = moves.size() - 1; i >= 0; i--) {
			final CobolMoveStmt move = moves.get(i);
			if ( ! (fieldReference.getParent() instanceof CobolCallStmt) || move.getEndOffset() < fieldReference.getStartOffset()) {
				final List<CobolExpression> moveToList = move.getToList();
				for (final CobolExpression moveTo : moveToList) {
					final String uniqueId = getUniqueId(fieldReference);
					if (contains(moveTo, uniqueId)) {
						final CobolExpression from = move.getFrom();
						/*
						 * This check is to avoid the infinite loop of resolving variables when statements are like below 
						 * 
						 *  MOVE X  TO Y.
						 *  MOVE Y TO X.
						 * 
						 *  EXEC CICS XCTL 
						 *  PROGRAM(X)
						 *  COMMAREA(WORK-MAP)
						 *  LENGTH(2000)
						 *  END-EXEC.
						 *  
						 *  X can be called through any caller method.
						 */
						if (getVariableNameFromCobolExpression(from).equals(rootVariableName)) {
							return;
						}
						final List<String> resolvedReferenceNames = resolveReferenceType(fieldReference, rootVariableName, from, firstCall, true);
						result.addAll(resolvedReferenceNames);

						if ( ! firstCall && ! resolvedReferenceNames.isEmpty()) {
							return;
						}
						if (fieldReference.getParent() instanceof CobolCallStmt) {
							return;
						}
					}
				}
			}
		}
	}
	
	private boolean isRootMove(final CobolFieldReference fieldReference) {
		final boolean foundMatchingTo = moves.stream()
				.flatMap(move -> move.getToList().stream())
				.filter(move -> move.getEndOffset() < fieldReference.getStartOffset())
				.anyMatch(to -> contains(to, getUniqueId(fieldReference)));
		return ! foundMatchingTo;
	}
	
	/*
	 * THIS IS A TEMPORARY FIX FOR THE TICKET WMIN-2761, complete solution will be
	 * possible when the Data Lineage feature is available.
	 * 
	 * finds and adds the nearest upstream SET when fieldReference is type of a call statement
	 * else will add all the matching upstream SETs.
	 */
	private void findRelevantUpstreamSets(final CobolFieldReference fieldReference, final String rootVariableName, final List<String> result,
			final boolean firstCall) {
		/* Resolve the value of the field reference from working storage section
		 if it is the first SET statement */
		if ( ! firstCall && isRootSet(fieldReference)) {
			result.addAll(resolveRefNames(fieldReference));
			return;
		}

		for (int i = sets.size() - 1; i >= 0; i--) {
			final CobolSetStmt set = sets.get(i);
			if ( ! (fieldReference.getParent() instanceof CobolCallStmt) || set.getEndOffset() < fieldReference.getStartOffset()) {
				final List<CobolSingleSetContainer> singleSets = set.getSingleSets();
				for (final CobolSingleSetContainer singleSet : singleSets) {
					final List<CobolExpression> froms = singleSet.getFroms();
					for (final CobolExpression from : froms) {
						final String uniqueId = getUniqueId(fieldReference);
						if (contains(from, uniqueId)) {
							final CobolExpression to = singleSet.getTo();
							if (to != null) {
								/*
								 * This check is to avoid the infinite loop of resolving variables when statements are like below 
								 * 
								 *  SET Y  TO X.
								 *  SET X TO Y.
								 * 
								 *  EXEC CICS XCTL 
								 *  PROGRAM(X)
								 *  COMMAREA(WORK-MAP)
								 *  LENGTH(2000)
								 *  END-EXEC.
								 *  
								 *  X can be called through any caller method.
								 */
								if (getVariableNameFromCobolExpression(to).equals(rootVariableName)) {
									return;
								}
								final List<String> resolvedReferenceNames = resolveReferenceType(fieldReference, rootVariableName, to, firstCall, false);
								result.addAll(resolvedReferenceNames);

								if ( ! firstCall && ! resolvedReferenceNames.isEmpty()) {
									return;
								}
							}
							if (fieldReference.getParent() instanceof CobolCallStmt) {
								return;
							}
						}
					}
				}
			}
		}
	}
	
	/**
	 * Returns the name of variable in source code from {@link CobolExpression}.
	 * Example: 
	 * 05  TEST PIC  X(08) VALUE 'ISET'.
	 * TEST will be returned from above variable.
	 *
	 * @param from {@link CobolExpression}
	 * @return the name of variable in source code from {@link CobolExpression}
	 */
	private String getVariableNameFromCobolExpression(final CobolExpression cobolExpression) {
		if ( ! (cobolExpression instanceof CobolReferenceExpression)) {
			return StringUtils.EMPTY;
		}

		final CobolReferenceExpression referenceExpression = (CobolReferenceExpression) cobolExpression;
		final CobolReference reference = referenceExpression.getOp1();

		if ( ! (reference instanceof CobolFieldReference)) {
			return StringUtils.EMPTY;
		}

		final var cobolFieldReference = ((CobolFieldReference) reference);
		return cobolFieldReference.getUnresolvedFieldName();
	}
	
	private boolean isRootSet(final CobolFieldReference fieldReference) {
		final boolean foundMatchingFrom = sets.stream()
				.flatMap(set -> set.getSingleSets().stream())
				.flatMap(singleSet -> singleSet.getFroms().stream())
				.filter(set -> set.getEndOffset() < fieldReference.getStartOffset())
				.anyMatch(from -> contains(from, getUniqueId(fieldReference)));
		return ! foundMatchingFrom;
	}
	
	private List<String> resolveReferenceType(final CobolFieldReference fieldReference, final String rootVariableName, final CobolExpression from,
			final boolean isFirstCall, final boolean isMove) {
		if (fieldReference.getParent() instanceof CobolCallStmt) {
			if (from.getStartOffset() < fieldReference.getStartOffset()) {
				return resolveReferenceName(fieldReference, rootVariableName, from, isFirstCall, isMove);
			}
		} else {
			return resolveReferenceName(fieldReference, rootVariableName, from, isFirstCall, isMove);
		}
		return emptyList();
	}

	private List<String> resolveReferenceName(@Nullable final CobolFieldReference fieldReference, final String rootVariableName, final CobolExpression from,
			final boolean isFirstCall, final boolean isMove) {
		if (from instanceof CobolReferenceExpression) {
			final CobolReferenceExpression referenceExpression = (CobolReferenceExpression) from;
			final CobolReference reference = referenceExpression.getOp1();

			if (reference instanceof CobolConstantReference) {
				final CobolConstantReference constant = (CobolConstantReference) reference;
				return Collections.singletonList(resolveName(constant));
			} else if (reference instanceof CobolFieldReference) {
				final List<String> names = new ArrayList<>();
				final CobolFieldReference cobolFieldReference = ((CobolFieldReference) reference);
				/*
				 * We need to add the initial value of variable as a resolved value, since there is a chance of having a variable without assigning value 
				 * with MOVE or SET statement from the place where we call.
				 */
				if (isFirstCall && fieldReference != null) {
					names.addAll(resolveRefNames(fieldReference));
					LOG.warn(String.format("The dependency from %s to programs like %s are not guaranteed", this.sourceObject.getPath(),
							String.join(",", names)));
				}
				if (isMove) {
					findRelevantUpstreamMoves(cobolFieldReference, rootVariableName, names, false);
				} else {
					findRelevantUpstreamSets(cobolFieldReference, rootVariableName, names, false);
				}
				return names;
			}
		}
		return emptyList();
	}
	
	private static Set<String> getUpstreamNames(@Nullable final CobolDataField dataField) {
		if (dataField == null) {
			return Collections.emptySet();
		}

		final Set<String> result = new HashSet<>();
		CobolDataField node = dataField;
		result.add(getUniqueId(node));
		while (node.getParent() instanceof CobolDataField) {
			node = (CobolDataField) node.getParent();
			result.add(getUniqueId(node));
		}

		return result;
	}
	
	private List<String> resolveNames(@Nullable final CobolDataField dataField) {
		if (dataField == null) {
			return emptyList();
		}

		final List<String> result = new ArrayList<>();
		final List<Object> values = dataField.getValues();

		if (values.isEmpty()) {
			/* Find the initialized values by field reference variable */
			final List<CobolDataField> cobolDataFields = dataField.getParent().getChildrenDeep(CobolFieldReference.class).stream()
					.filter(fieldReference -> fieldReference.getField() != null)
					.map(fieldReference -> fieldReference.getField().getChildrenDeep(CobolDataField.class)).flatMap(List::stream).collect(Collectors.toList());

			final List<Object> referenceValues = cobolDataFields.stream().map(CobolDataField::getValues).filter(dataFieldValues -> ! dataFieldValues.isEmpty())
					.flatMap(List::stream).collect(Collectors.toList());

			values.addAll(referenceValues);
		}
		for (final Object object : values) {
			if (object instanceof CobolConstantReference) {
				final CobolConstantReference constant = (CobolConstantReference) object;
				if (startsWithQuotes(constant)) {
					result.add(resolveName(constant));
				}
			}
		}
		return result;
	}
	
	private List<String> resolveRefNames(final CobolFieldReference cobolFieldReference) {
		if (cobolFieldReference.getField() != null) {
			return cobolFieldReference.getField().getValues().stream().filter(CobolConstantReference.class::isInstance).map(CobolConstantReference.class::cast)
					.filter(this::startsWithQuotes).map(constant -> {
						final String sub1 = cobolFieldReference.getSubstring1() != null ? cobolFieldReference.getSubstring1().toString() : StringUtils.EMPTY;
						final String sub2 = cobolFieldReference.getSubstring2() != null ? cobolFieldReference.getSubstring2().toString() : StringUtils.EMPTY;
						if (StringUtils.isNotBlank(sub1) && StringUtils.isNotBlank(sub2)) {
							final var startOffset = Integer.parseInt(sub1);
							final var offsetLength = Integer.parseInt(sub2);
							return resolveName(constant).substring(startOffset - 1, startOffset + offsetLength - 1);
						} else {
							return resolveName(constant);
						}
					}).collect(Collectors.toList());
		}
		return emptyList();
	}
	
	private boolean startsWithQuotes(final CobolConstantReference constant) {
		final var constantString = constant.getValue().toString();
		return constantString.startsWith("\"") || constantString.startsWith("\'");
	}

	private String resolveName(final CobolConstantReference constant) {
		return trimQuotesSpaces(constant.getValue().toString());
	}
	
	private List<String> resolveNames(final CobolFieldReference fieldReference, @Nullable final ModuleBuilder module) {

		/* Find field values */
		final List<String> result = new ArrayList<>(resolveRefNames(fieldReference));

		final String variableName = fieldReference.getUnresolvedFieldName();
		findRelevantUpstreamMoves(fieldReference, variableName, result, true);

		findRelevantUpstreamSets(fieldReference, variableName, result, true);

		/* Find the initial value of variable into field */
		if (result.isEmpty()) {
			resolveCobolDataFieldDependencies(fieldReference, result, module);
		}
		return result;
	}

	/*
	 * This method is to resolve the values of cobol data field variables
	 * 
	 * Example :
	 * 01 WS-PROGRAM-ID.
	 * 		05 WS-PROGRAM-ENV  PIC X(03) VALUE 'DLV'.
	 * 		05 WS-PROD-TEST    PIC X(01) VALUE '001'.
	 * 			88 DEFAULT-QUAL VALUE '001'.
	 * 			88 SET-PUT-FLAG VALUE 'PUT'.
	 * 
	 * 01 WS-PROGRAMA-ID.
	 * 		05 WS-PROGRAM-ENV  PIC X(03) VALUE 'ADLV'.
	 * 
	 * PROCEDURE DIVISION.
	 * CALL WS-PROGRAM-ID.
	 * CALL WS-PROGRAMA-ID.
	 * 
	 * Here in the above example we will resolve the variable '01 WS-PROGRAM-ID' to 'DLV001' or 'DLVPUT' and for '01 WS-PROGRAMA-ID' to 'ADLV'
	 */
	private void resolveCobolDataFieldDependencies(final CobolFieldReference fieldReference, final List<String> result, final ModuleBuilder module) {
		String finalResolveName = StringUtils.EMPTY;
		final List<String> dynamicDependencies = new ArrayList<>();
		for (final CobolDataField cobolDataField : data) {
			if (getUpstreamNames(cobolDataField).contains(getUniqueId(fieldReference))) {
				finalResolveName = resolveCobolDataField(result, module, finalResolveName, dynamicDependencies, cobolDataField);
			}
		}

		if (dynamicDependencies.size() > 1 && module != null) {
			final var errorMessage = String.format("Dependency with %s might be incorrect because of dynamic calls.", String.join(", ", dynamicDependencies));
			module.addError(Severity.WARNING, ErrorKey.DEPENDENCY_RESOLUTION_ERROR, errorMessage,
					astNodeLocationProvider.getAstNodeLocation(fieldReference));
		}

		if ( ! finalResolveName.isEmpty()) {
			result.add(finalResolveName);
		}
	}

	private String resolveCobolDataField(final List<String> result, @Nullable final ModuleBuilder module, String finalResolveName,
			final List<String> dynamicDependencies, @Nullable final CobolDataField cobolDataField) {
		final List<String> resolveNames = resolveNames(cobolDataField);
		if ( ! resolveNames.isEmpty()) {
			if (module != null) {
				if (cobolDataField != null && cobolDataField.getLevel() != LEVEL_88) {
					finalResolveName = finalResolveName.concat(String.join("", resolveNames));
				} else {
					for (final String resolveName : resolveNames) {
						final String levelResolveName = finalResolveName.concat(String.join("", resolveName));
						dynamicDependencies.add(levelResolveName);
						result.add(levelResolveName);
					}
				}
			} else {
				result.addAll(resolveNames);
			}
		}
		return finalResolveName;
	}

	private void findRelevantUpstreamMovesForTransID(final int execCicsReturnStartOffset, final String defaultTransId, final List<String> result) {
		for (int i = moves.size() - 1; i >= 0; i--) {
			final CobolMoveStmt move = moves.get(i);
			if (move.getEndOffset() < execCicsReturnStartOffset) {
				final List<CobolExpression> moveToList = move.getToList();
				for (final CobolExpression moveTo : moveToList) {
					if (contains(moveTo, defaultTransId)) {
						final CobolExpression from = move.getFrom();
						final List<String> resolvedReferenceNames = resolveReferenceName(null, defaultTransId, from, false, true);
						result.addAll(resolvedReferenceNames);

						if ( ! resolvedReferenceNames.isEmpty()) {
							return;
						}
					}
				}
			}
		}
	}
	
	/**
	 * Expects a CICS Return node which it then evaluates for any trans-id and checks if such an id can be found
	 * (this ID would be taken from a .csd file).
	 * If found it returns the transaction id.
	 *
	 * @param node ExecCicsReturn type as used in EXEC CICS RETURN ... TRANSID()
	 * @param rootModule the root {@link ModuleBuilder}
	 * @return the list of resolved trans-id.
	 */
	public Optional<String> resolveReturnTransID(final ExecCicsReturn node, final ModuleBuilder rootModule) {
		final List<String> result = new ArrayList<>();
		if (node.getTransId() == null || StringUtils.isEmpty(node.getTransId().toString())) {
			return Optional.empty();
		}
		/* strip quotes */

		String name = node.getTransId().toString().trim();
		name = trimQuotesSpaces(name);
		findRelevantUpstreamMovesForTransID(node.getStartOffset(), name, result);
		if (result.isEmpty()) {
			rootModule.addError(Severity.WARNING, ErrorKey.DEPENDENCY_RESOLUTION_ERROR, String.format("Unable to determine name of trans id for %s", name),
					astNodeLocationProvider.getAstNodeLocation(node));
		} else {
			name = result.get(0);
		}
		return Optional.of(name);
	}
	
	/**
	 * Expects a CICS Return node which it then evaluates for any trans-id.
	 * If found it returns the transaction otherwise return empty value
	 * @param transaction ExecCicsCreateTransaction type as used in EXEC CICS CREATE TRANSACTION statement
	 * @return the transaction id.
	 */
	public Optional<String> resolveReturnTransID(final ExecCicsCreateTransaction transaction) {

		final CobolExpression expression = transaction.getTransId();
		if(expression == null || StringUtils.isEmpty(expression.toString())) {
			return Optional.empty();
		}
		/* strip quotes */	
		String name = expression.toString().trim();
		name = trimQuotesSpaces(name);
		return Optional.of(name);
	}
	
	/**
	 * Expects a CICS file which it then evaluates for matching name(s) in the module repository.
	 * If found it adds this to the list of reference entries with link to GUID, if not found adds with -1 for GUID.
	 *
	 * @param file ExecCicsFile type as used in EXEC CICS READ/WRITE
	 * @return the model artifact found or created that matches this file...
	 */
	public String resolve(final ExecCicsFile file) {
		return resolve(((CobolReferenceExpression) file.getFile()).getOp1()).stream().findFirst().orElse(file.getFile().toString());
	}
}
