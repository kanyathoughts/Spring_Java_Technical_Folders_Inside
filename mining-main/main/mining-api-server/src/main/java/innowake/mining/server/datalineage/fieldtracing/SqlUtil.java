/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage.fieldtracing;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.lib.calcite.sql.SqlColumnOperand;
import innowake.lib.calcite.sql.SqlCursor;
import innowake.lib.calcite.sql.SqlFetchCursor;
import innowake.lib.calcite.sql.SqlHostVariable;
import innowake.lib.calcite.sql.SqlIdentifier;
import innowake.lib.calcite.sql.SqlIntoOperand;
import innowake.lib.calcite.sql.SqlNode;
import innowake.lib.calcite.sql.SqlNodeBinding;
import innowake.lib.calcite.sql.SqlSelect;
import innowake.lib.calcite.sql.SqlSelectOperand;
import innowake.lib.calcite.sql.SqlSourceOperand;
import innowake.lib.calcite.sql.SqlUpdate;
import innowake.lib.calcite.sql.SqlValueOperand;
import innowake.lib.calcite.sql.SqlWhereOperand;
import innowake.lib.calcite.sql.parser.SqlParseException;
import innowake.lib.calcite.sql.parser.SqlParser;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.visitor.TopDownVisitor;

/**
 * Utility class containing methods to handle SQL statements.
 */
public class SqlUtil {
	
	/**
	 * Class representing the result of analyzing an SQL statement.
	 */
	public static class SqlInfo {
		private Map<String, List<String>> columnMap;
		private Set<String> writtenTo;
		private Set<String> readFrom;
		
		/**
		 * @return Map mapping host variables to DB columns
		 */
		public Map<String, List<String>> getColumnMap() {
			return columnMap;
		}
		
		/**
		 * @param columnMap Map mapping host variables to DB columns
		 */
		public void setColumnMap(final Map<String, List<String>> columnMap) {
			this.columnMap = columnMap;
		}

		/**
		 * @return set of host variables the statement writes to
		 */
		public Set<String> getWrittenTo() {
			return writtenTo;
		}

		/**
		 * @param writtenTo set of host variables the statement writes to
		 */
		public void setWrittenTo(final Set<String> writtenTo) {
			this.writtenTo = writtenTo;
		}

		/**
		 * @return set of host variables the statement reads from
		 */
		public Set<String> getReadFrom() {
			return readFrom;
		}

		/**
		 * @param readFrom set of host variables the statement reads from
		 */
		public void setReadFrom(final Set<String> readFrom) {
			this.readFrom = readFrom;
		}
	}
	
	private SqlUtil() {
		/* Hides implicit constructor */
	}
	
	/**
	 * Analyzes SQL statement and provides information on:
	 * <li>which host variables are read from</li>
	 * <li>which host variables are written to</li>
	 * <li>which host variables map to which DB columns</li>
	 *
	 * @param sql SQL statement
	 * @return {@linkplain SqlUtil.SqlInfo} instance containing the information mentioned above
	 */
	public static SqlInfo getSqlInfo(final String sql) {
		final SqlInfo info = new SqlInfo();
		SqlNode parsedStmt;
		try {
			parsedStmt = SqlParser.create(sql).parseStmt();
		} catch (final SqlParseException e) {
			throw new IllegalArgumentException("SqlParser failed to parse sql: " + sql, e);
		}
		info.setColumnMap(getColumnMap(parsedStmt));
		final List<SqlHostVariable> childrenDeep = parsedStmt.getChildrenDeep(SqlHostVariable.class);
		info.setWrittenTo(childrenDeep.stream().filter(SqlUtil::isWrittenTo).map(SqlHostVariable::getName).collect(Collectors.toSet()));
		info.setReadFrom(childrenDeep.stream().filter(SqlUtil::isReadFrom).map(SqlHostVariable::getName).collect(Collectors.toSet()));
		
		return info;
	}

	/**
	 * Parses an SQL String using {@linkplain SqlParser} and builds a column map based on the parse result.
	 *
	 * @param parsedStmt SQL statement
	 * @return Column map mapping host variables to DB columns that are associated with it
	 * @throws IllegalArgumentException when the {@linkplain SqlParser} fails to parse the statement
	 */
	private static Map<String, List<String>> getColumnMap(final SqlNode parsedStmt) {
		final Map<String, List<String>> result = new HashMap<>();
		final boolean isBound = parsedStmt.getClass().equals(SqlSelect.class) || parsedStmt.getClass().equals(SqlUpdate.class) ||
				parsedStmt.getClass().equals(SqlCursor.class) || parsedStmt.getClass().equals(SqlFetchCursor.class);
		final List<SqlHostVariable> childrenConnectedWithColumns = getHostVariableChildren(parsedStmt);
		
		if (isBound) {
			handleBound(result, parsedStmt, childrenConnectedWithColumns);
		} else {
			handleUnbound(result, parsedStmt, childrenConnectedWithColumns);
		}
		
		return result;
	}
	
	private static boolean isWrittenTo(final AstNode hostVar) {
		if (hostVar.isRoot()) {
			return false;
		}
		final AstNode parent = hostVar.getNullableParent();
		if (parent == null) {
			return false;
		}
		if (parent instanceof SqlIntoOperand) {
			return true;
		}
		return isWrittenTo(parent);
	}
	
	private static boolean isReadFrom(final AstNode hostVar) {
		if (hostVar.isRoot()) {
			return false;
		}
		final AstNode parent = hostVar.getNullableParent();
		if (parent == null) {
			return false;
		}
		if (parent instanceof SqlWhereOperand || parent instanceof SqlSourceOperand || parent instanceof SqlValueOperand) {
			return true;
		}
		return isReadFrom(parent);
	}

	private static List<SqlHostVariable> getHostVariableChildren(final AstNode astNode) {
		final List<SqlHostVariable> result = new ArrayList<>();
		new TopDownVisitor<AstNode>(node -> {
			if (node instanceof SqlWhereOperand) {
				return false;
			}
			if (node instanceof SqlHostVariable) {
				result.add((SqlHostVariable) node);
			}
			return true;
		}).visit(astNode);
		return result;
	}

	private static void handleBound(final Map<String, List<String>> result, final SqlNode parseStmt, final List<SqlHostVariable> childrenDeep) {
		for (final SqlHostVariable hostVar : childrenDeep) {
			final SqlNode parent = findParent(hostVar);
			if (parent != null) {
				final SqlNodeBinding op = (SqlNodeBinding) parent;
				final Map<String,List<AstNode>> bindings = op.getBindings();
				final List<AstNode> list = new ArrayList<>();
				bindings.values().forEach(list::addAll);
				final List<String> columnNames = obtainColumnNames(list, parseStmt);
				result.put(hostVar.getName(), columnNames);
			}
		}
	}
	
	private static void handleUnbound(final Map<String, List<String>> result, final SqlNode parseStmt, final List<SqlHostVariable> childrenDeep) {
		final List<SqlColumnOperand> columnOperands = parseStmt.getChildren(SqlColumnOperand.class);
		if (columnOperands.size() < childrenDeep.size()) {
			throw new IllegalStateException("The statement refers to more host variables than columns");
		}
		for (int i = 0; i < childrenDeep.size(); i++) {
			final SqlHostVariable hostVar = childrenDeep.get(i);
			final SqlColumnOperand columnOp = columnOperands.get(i);
			final List<SqlIdentifier> children = columnOp.getChildren(SqlIdentifier.class);
			
			List<String> list = result.get(hostVar.getName());
			if (list == null) {
				list = new ArrayList<>();
				result.put(hostVar.getName(), list);
			}
			list.addAll(children.stream().map(c -> c.names.get(0)).collect(Collectors.toList()));
		}
	}

	/**
	 * Extracts a list of column names from a list of {@linkplain AstNode}
	 *
	 * @param list the AstNodes
	 * @param parseStmt statement node used to check the type of statement
	 * @return List of Strings representing the column names
	 */
	private static List<String> obtainColumnNames(final List<AstNode> list, final SqlNode parseStmt) {
		if (parseStmt instanceof SqlSelect || parseStmt instanceof SqlCursor) {
			return list.stream()
					.filter(SqlSelectOperand.class::isInstance)
					.map(SqlSelectOperand.class::cast)
					.map(node -> ((SqlIdentifier) node.getSelectOperand()).names.get(0))
					.collect(Collectors.toList());
		} else {
			return list.stream()
					.filter(SqlColumnOperand.class::isInstance)
					.map(SqlColumnOperand.class::cast)
					.map(node -> node.getChildren().get(0))
					.filter(SqlIdentifier.class::isInstance)
					.map(SqlIdentifier.class::cast)
					.map(node -> node.names.get(0))
					.collect(Collectors.toList());
		}
		
	}
	
	private static SqlNode findParent(final AstNode node) {
		final AstNode parent = node.getNullableParent();
		if (parent == null) {
			return null;
		}
		if (parent instanceof SqlNodeBinding && ! (parent instanceof SqlWhereOperand)) {
			final SqlNodeBinding op = (SqlNodeBinding) parent;
			final Map<String, List<AstNode>> bindings = op.getBindings();
			if ( ! bindings.isEmpty()) {
				return op;
			}
		}
		return findParent(parent);
	}
}
