/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.api.query.clause;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import innowake.spring.data.orientdb.repository.query.clauses.*;
import org.apache.commons.lang.StringUtils;
import com.github.raymanrt.orientqb.query.Clause;
import com.github.raymanrt.orientqb.query.Operator;
import com.github.raymanrt.orientqb.query.Projection;
import com.github.raymanrt.orientqb.query.ProjectionFunction;
import com.github.raymanrt.orientqb.util.Joiner;
import com.google.common.collect.Lists;
import innowake.mining.shared.springdata.EdgeDirection;
import innowake.spring.data.orientdb.commons.exception.UnsupportedQueryTypeException;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinition;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinitionMapper;

/**
 * Wrapper class for {@link Clause}.
 */
public final class OrientClause {

	private static final String UNSUPPORTED_VALUE_TYPE = "Unsupported value assigned for type ";
	private final Clause clause;

	/**
	 * Instantiates orient clause.
	 * 
	 * @param clause the clause
	 */
	private OrientClause(final Clause clause) {
		this.clause = clause;
	}

	/**
	 * Creates a orient clause.
	 * <p>
	 * Does not work with primitive members use {@link #clause(String, OrientOperator, Object)} instead.
	 * 
	 * @param domainClass the entity class
	 * @param field the field name
	 * @param operator operation to be performed
	 * @param value the value of the field
	 * 
	 * @return an orient clause.
	 */
	public static OrientClause clause(final Class<?> domainClass, final String field, final OrientOperator operator, final Object value) {
		final Class<?> fieldType = getFieldType(domainClass, field);
		if (value.getClass().isAssignableFrom(fieldType)) {
			return new OrientClause(Clause.clause(field, operator.getOperator(), value));
		}
		throw new UnsupportedQueryTypeException("Unsupported value assigned " + fieldType);
	}

	/**
	 * Creates a orient clause.
	 * 
	 * @param field the field name
	 * @param operator operation to be performed
	 * @param value the value of the field
	 * @return an orient clause
	 */
	public static OrientClause clause(final String field, final OrientOperator operator, final Object value) {
		return new OrientClause(Clause.clause(field, operator.getOperator(), value));
	}

	/**
	 * Creates an orient clause that uses the Lucene full text index
	 *
	 * @param index the name of the full text index to use
	 * @param value the value to be searched
	 * @return an orient clause
	 */
	public static OrientClause luceneClause(final String index, final String value) {
		return new OrientClause(new SearchIndexClause(index, value));
	}

	/**
	 * Creates a orient {@code AND} clause.
	 * 
	 * @param clauses array of clauses
	 *
	 * @return an orient clause.
	 */
	public static OrientClause and(final OrientClause... clauses) {
		final Clause[] orientClause = getOrientClause(clauses);
		/*
		 * The default CompositeClause adds brackets around AND clauses which causes that Orient doesn't use indexes anymore
		 */
		return new OrientClause(new OrientCompositeClause(Joiner.andJoiner, orientClause));
	}

	/**
	 * Creates a not clause.
	 *
	 * @param clause a orient clause
	 * @return an orient clause
	 */
	public static OrientClause not(final OrientClause clause) {
		return new OrientClause(Clause.not(clause.getClause()));
	}

	/**
	 * Creates a orient {@code OR} clause.
	 * 
	 * @param clauses array of clauses
	 *
	 * @return an orient clause
	 */
	public static OrientClause or(final OrientClause... clauses) {
		final Clause[] orientClause = getOrientClause(clauses);
		return new OrientClause(Clause.or(orientClause));
	}

	/**
	 * Creates an edge clause.
	 *
	 * @param edgeFieldName name of the edge.
	 * @param direction direction of edge
	 * @param clause the sub clause
	 * @return an edge clause
	 */
	public static OrientClause edgeClause(final String edgeFieldName, final EdgeDirection direction, final OrientClause clause) {
		return new OrientClause(new EdgeClause(edgeFieldName, direction, clause.getClause()));
	}

	/**
	 * Creates a clause to query on embedded map field.
	 *
	 * @param domainClass the entity class
	 * @param fieldName the name of the map field
	 * @param key the key value
	 * @param value the value
	 * @return orient clause for map
	 */
	public static OrientClause embeddedMapClasue(final Class<?> domainClass, final String fieldName, final String key, final Object value) {
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(domainClass);
		final Optional<Field> fieldByName;
		final Optional<String> javaFieldNameFromDBFieldName = classDefinition.getJavaFieldNameFromDBFieldName(fieldName);
		if (javaFieldNameFromDBFieldName.isPresent()) {
			fieldByName = classDefinition.getFieldByName(javaFieldNameFromDBFieldName.get());
		} else {
			fieldByName = classDefinition.getFieldByName(fieldName);
		}
		if (fieldByName.isPresent()) {
			final Class<?> fieldType = (Class<?>) ((ParameterizedType) fieldByName.get().getGenericType()).getActualTypeArguments()[1];
			if (value.getClass().isAssignableFrom(fieldType)) {
				return new OrientClause(new EmbeddedMapClause(fieldName, key, value));
			}
			throw new UnsupportedQueryTypeException("Unsupported value assigned for embedded map of type " + fieldType);
		}
		throw new UnsupportedQueryTypeException("Field " + fieldName + " is not part of the given domain class " + domainClass);
	}

	/**
	 * Creates a clause to query on embedded set field.
	 *
	 * @param domainClass the entity class
	 * @param fieldName the name of the embedded set field
	 * @param value the value
	 * @return orient clause for embedded set
	 */
	public static OrientClause embeddedSetClause(final Class<?> domainClass, final String fieldName, final Object value) {
		final Class<?> fieldType = getFieldType(domainClass, fieldName);
		if (value.getClass().isAssignableFrom(fieldType)) {
			return new OrientClause(new EmbeddedSetClause(value, Operator.IN, fieldName));
		}
		throw new UnsupportedQueryTypeException("Unsupported value assigned for embedded set of type " + fieldType);
	}

	/**
	 * Creates a CONTAINS clause on related collection entities field
	 *
	 * @param domainClass the entity class
	 * @param collectionField the collection entity field
	 * @param subClause clause from the related entity
	 * @return a CONTAINS clause
	 */
	public static OrientClause containsClause(final Class<?> domainClass, final String collectionField, final OrientClause subClause) {
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(domainClass);
		final Optional<Field> fieldByName = classDefinition.getFieldByName(collectionField);
		if (fieldByName.isPresent()) {
			final Clause subClauseWithBracket = new ClauseWithParenthesis(subClause.getClause());
			return new OrientClause(Clause.clause(collectionField, Operator.CONTAINS, subClauseWithBracket));
		}
		throw new UnsupportedQueryTypeException("Field " + collectionField + " is not part of the given domain class " + domainClass);
	}

	/**
	 * Creates a IN clause to compare a collection of values.
	 *
	 * @param domainClass the entity class
	 * @param fieldName the name of the field
	 * @param values the list of values to be compared
	 * @return orient IN clause
	 */
	@SuppressWarnings("unchecked")
	public static OrientClause inClause(final Class<?> domainClass, final String fieldName, final List<?> values) {
		final Class<?> fieldType = getFieldType(domainClass, fieldName);
		if ( ! values.isEmpty() && values.get(0).getClass().isAssignableFrom(fieldType)) {
			return new OrientClause(new ClauseWithInOperator(fieldName, (List<Object>) values));
		}
		throw new UnsupportedQueryTypeException("Unsupported list value assigned for field " + fieldName);
	}

	/**
	 * Creates a IN clause to compare a collection of values.
	 *
	 * @param domainClass the entity class
	 * @param fieldName the name of the field
	 * @param values the values to be compared
	 * @return orient IN clause
	 */
	public static OrientClause inClause(final Class<?> domainClass, final String fieldName, final Object[] values) {
		final Class<?> fieldType = getFieldType(domainClass, fieldName);
		if (values.getClass().getComponentType().isAssignableFrom(fieldType)) {
			return new OrientClause(new ClauseWithInOperator(fieldName, Arrays.asList(values)));
		}
		throw new UnsupportedQueryTypeException(UNSUPPORTED_VALUE_TYPE + fieldType);
	}

	/**
	 * Creates a IN clause to compare a collection of values.
	 *
	 * @param fieldName the name of the field
	 * @param values the list of values to be compared
	 * @return orient IN clause
	 */
	@SuppressWarnings("unchecked")
	public static OrientClause inClause(final String fieldName, final List<?> values) {
		return new OrientClause(new ClauseWithInOperator(fieldName, (List<Object>) values));
	}

	/**
	 * Creates a IN clause to compare a collection of values.
	 *
	 * @param fieldName the name of the field
	 * @param values the values to be compared
	 * @return orient IN clause
	 */
	public static OrientClause inClause(final String fieldName, final Object[] values) {
		return new OrientClause(new ClauseWithInOperator(fieldName, Arrays.asList(values)));
	}

	/**
	 * Creates a {@code CONTAINSANY} clause that checks whether the field contains any of the values.
	 * 
	 * @param fieldName name of the field
	 * @param values list of values to be compared
	 * @return orient CONTAINSANY clause
	 */
	@SuppressWarnings("unchecked")
	public static OrientClause containsAnyClause(final String fieldName, final List<?> values) {
		return new OrientClause(new ClauseWithContainsAnyOperator(fieldName, (List<Object>) values));
	}

	/**
	 * Creates a Not IN clause to compare a collection of values.
	 *
	 * @param domainClass the entity class
	 * @param fieldName the name of the field
	 * @param values the list of values to be compared
	 * @return orient Not IN clause
	 */
	@SuppressWarnings("unchecked")
	public static OrientClause notInClause(final Class<?> domainClass, final String fieldName, final List<?> values) {
		final Class<?> fieldType = getFieldType(domainClass, fieldName);
		if ( ! values.isEmpty() && values.get(0).getClass().isAssignableFrom(fieldType)) {
			return new OrientClause(new ClauseWithNotInOperator(fieldName, (List<Object>) values));
		}
		throw new UnsupportedQueryTypeException("Unsupported list value assigned for field " + fieldName);
	}

	/**
	 * Creates a Not IN clause to compare a collection of values.
	 *
	 * @param domainClass the entity class
	 * @param fieldName the name of the field
	 * @param values the values to be compared
	 * @return orient NOT IN clause
	 */
	public static OrientClause notInClause(final Class<?> domainClass, final String fieldName, final Object[] values) {
		final Class<?> fieldType = getFieldType(domainClass, fieldName);
		if (values.getClass().getComponentType().isAssignableFrom(fieldType)) {
			return new OrientClause(new ClauseWithNotInOperator(fieldName, Arrays.asList(values)));
		}
		throw new UnsupportedQueryTypeException(UNSUPPORTED_VALUE_TYPE + fieldType);
	}

	/**
	 * Creates a Not IN clause to compare a collection of values.
	 *
	 * @param fieldName the name of the field
	 * @param values the list of values to be compared
	 * @return orient Not IN clause
	 */
	@SuppressWarnings("unchecked")
	public static OrientClause notInClause(final String fieldName, final List<?> values) {
		return new OrientClause(new ClauseWithNotInOperator(fieldName, (List<Object>) values));
	}

	/**
	 * Creates a Not IN clause to compare a collection of values.
	 *
	 * @param fieldName the name of the field
	 * @param values the values to be compared
	 * @return orient NOT IN clause
	 */
	public static OrientClause notInClause(final String fieldName, final Object[] values) {
		return new OrientClause(new ClauseWithNotInOperator(fieldName, Arrays.asList(values)));
	}

	/**
	 * Creates a SIZE clause to find the fields by evaluating the number of edge references compared to the given value. This would result in a Orient Clause
	 * equivalent to <b>&lt;fieldName&gt;.size() &lt;operator&gt; &lt;value&gt;</b>.
	 *
	 * @param fieldName the name of the field
	 * @param operator operation to be performed
	 * @param value the value to compare
	 * @return orient SIZE clause
	 */
	public static OrientClause sizeClause(final String fieldName, final OrientOperator operator, final Long value) {
		return new OrientClause(Clause.clause(Projection.projection(fieldName).asList().size(), operator.getOperator(), value));
	}


	/**
	 * 
	 * Creates a lower case clause to support lower case projection.
	 * 
	 * @param domainClass the entity class
	 * @param field the name of the field
	 * @param operator operation to be performed
	 * @param value the value to compare
	 * @return {@link OrientClause} lowerCase clause
	 */
	public static OrientClause toLowerCaseClause(final Class<?> domainClass, final String field, final OrientOperator operator, final Object value) {
		final Class<?> fieldType = getFieldType(domainClass, field);
		if (value.getClass().isAssignableFrom(fieldType)) {
			return new OrientClause(Clause.clause(Projection.projection(field).toLowerCase(), operator.getOperator(), value));
		}
		throw new UnsupportedQueryTypeException(UNSUPPORTED_VALUE_TYPE + fieldType);
	}

	/**
	 * 
	 * Creates a lower case clause to support lower case projection.
	 * 
	 * @param field the name of the field
	 * @param operator operation to be performed
	 * @param value the value to compare
	 * @return {@link OrientClause} lowerCase clause
	 */
	public static OrientClause toLowerCaseClause(final String field, final OrientOperator operator, final Object value) {
		return new OrientClause(Clause.clause(Projection.projection(field).toLowerCase(), operator.getOperator(), value));
	}

	/**
	 * Creates an orient clause that uses the {@code .length()} projection
	 *
	 * @param field the name of the field
	 * @param operator operation to be performed on the length
	 * @param value the value to compare
	 * @return {@link OrientClause} length clause
	 */
	public static final OrientClause lengthClause(final String field, final OrientOperator operator, final Object value) {
		return new OrientClause(Clause.clause(Projection.projection(field).length(), operator.getOperator(), value));
	}

	/**
	 * Returns a clause that checks properties on adjacent vertices by following edges of the given type.
	 * 
	 * @param indirections and indirections to follow in order to reach the edge
	 * @param edgeDirection the direction of the edge that we should follow
	 * @param edgeName the name of the edge that we should follow
	 * @param fieldName the name of the field on the adjacent vertex that is examined
	 * @param operator the operator that is applied to the field
	 * @param value the right-hand side of the operator
	 * @return the clause
	 */
	public static OrientClause hasAdjacentVertexWithPropertyClause(final Optional<String> indirections, final EdgeDirection edgeDirection,
			final String edgeName, final String fieldName, final OrientOperator operator, final Object value) {
		if (edgeDirection == EdgeDirection.BOTH) {
			throw new IllegalArgumentException("EdgeDirection BOTH is not applicable for the adjacent vertex clause. Please pick either OUT or IN.");
		}
		Projection edgeProjection = edgeDirection == EdgeDirection.IN ? ProjectionFunction.in(edgeName) : ProjectionFunction.out(edgeName);
		if (indirections.isPresent()) {
			edgeProjection = Projection.projection(indirections.get()).dot(edgeProjection);
		}

		return new OrientClause(Clause.clause(edgeProjection, OrientOperator.CONTAINS.getOperator(),
				new ClauseWithParenthesis(Clause.clause(Projection.projection(fieldName), operator.getOperator(), value))));
	}

	/**
	 * Returns the clause.
	 *
	 * @return instance of {@link Clause}
	 */
	public Clause getClause() {
		return clause;
	}

	private static Clause[] getOrientClause(final OrientClause... clauses) {
		final Clause[] orientClause = new Clause[clauses.length];
		for (int i = 0; i < clauses.length; i++) {
			orientClause[i] = clauses[i].getClause();
		}
		return orientClause;
	}

	private static Class<?> getFieldType(final Class<?> domainClass, final String fieldName) {
		final String[] fieldNames = StringUtils.split(fieldName, ".");
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(domainClass);
		Optional<Field> field;
		final String fields = fieldNames[0];
		final Optional<String> javaFieldNameFromDBFieldName = classDefinition.getJavaFieldNameFromDBFieldName(fields);
		if (javaFieldNameFromDBFieldName.isPresent()) {
			field = classDefinition.getFieldByName(javaFieldNameFromDBFieldName.get());
		} else {
			field = classDefinition.getFieldByName(fields);
		}
		if ( ! field.isPresent()) {
			throw new UnsupportedQueryTypeException(UNSUPPORTED_VALUE_TYPE + field);
		}
		Field fieldValue = field.get();
		Class<?> fieldType = fieldValue.getType();
		if (Collection.class.isAssignableFrom(fieldType)) {
			final ParameterizedType type = (ParameterizedType) fieldValue.getGenericType();
			fieldType = (Class<?>) type.getActualTypeArguments()[0];
		} else if (fieldType.isEnum()) {
			return String.class;
		}
		if (fieldNames.length > 1) {
			return getFieldType(fieldType, StringUtils.join(fieldNames, ".", 1, fieldNames.length));
		} else {
			return fieldType;
		}
	}

	public static class PathSegment {
		final Optional<EdgeDirection> edgeDirection;
		final String edgeFieldName;

		private PathSegment(final String edgeFieldName) {
			this.edgeFieldName = edgeFieldName;
			edgeDirection = Optional.empty();
		}

		private PathSegment(final EdgeDirection edgeDirection, final String edgeFieldName) {
			this.edgeFieldName = edgeFieldName;
			this.edgeDirection = Optional.of(edgeDirection);
		}

		public static PathSegment field(final String edgeFieldName) {
			return new PathSegment(edgeFieldName);
		}

		public static PathSegment edge(final EdgeDirection edgeDirection, final String edgeFieldName) {
			return new PathSegment(edgeDirection, edgeFieldName);
		}
	}

	/**
	 * Makes a clause which checks whether an "adjacent vertex" (i.e. a vertex that is connected to the current one via an edge) has a certain property.
	 *
	 * @param path list of path segments to follow; the path segments are joined with the '.' (dot) projection and each segment may use an OUT() or IN()
	 *        operator to follow an outgoing resp. incoming edge
	 * @param clause the clause to apply to the vertex that was reached after following the path
	 * @return Orient Adjacent vertex Clause which has a certain property.
	 */
	public static OrientClause hasAdjacentVertexWithPropertyClause(final List<PathSegment> path, final Clause clause) {
		final Projection pathProjection = adjacentVertexProjection(path);

		return new OrientClause(Clause.clause(pathProjection, OrientOperator.CONTAINS.getOperator(), new ClauseWithParenthesis(clause)));
	}

	public static OrientClause hasAdjacentVertexWithFullTextSearchClause(final List<PathSegment> path, final String indexName, final Object value) {
		final Projection pathProjection = adjacentVertexProjection(path);

		return new OrientClause(Clause.clause(pathProjection, OrientOperator.CONTAINS.getOperator(),
				new ClauseWithParenthesis(luceneClause(indexName, value.toString()).getClause())));
	}

	private static Projection adjacentVertexProjection(final List<PathSegment> path) {
		final List<Projection> pathProjections = path.stream().map(pathSegment -> {
			if (pathSegment.edgeDirection.isPresent()) {
				if (pathSegment.edgeDirection.get() == EdgeDirection.IN) {
					return ProjectionFunction.in(pathSegment.edgeFieldName);
				} else if (pathSegment.edgeDirection.get() == EdgeDirection.OUT) {
					return ProjectionFunction.out(pathSegment.edgeFieldName);
				} else {
					throw new IllegalArgumentException("EdgeDirection BOTH is not applicable for the adjacent vertex clause. Please pick either OUT or IN.");
				}
			} else {
				return Projection.projection(pathSegment.edgeFieldName);
			}
		}).collect(Collectors.toList());

		final Projection pathProjection;
		if (pathProjections.isEmpty()) {
			throw new IllegalArgumentException("Path cannot be empty");
		} else if (pathProjections.size() == 1) {
			pathProjection = pathProjections.get(0);
		} else {
			pathProjection = Lists.reverse(pathProjections).stream().reduce((a, b) -> b.dot(a))
					.orElseThrow(() -> new IllegalStateException("reduced pathProjection is not present even though Path is not empty"));
		}

		return pathProjection;
	}

	/**
	 * Creates a clause with custom SQL. The provided SQL must be valid and safe, i.e. contain no user-provided data.
	 *
	 * @param clause the SQL for the custom clause
	 * @return a custom OrientClause
	 */
	public static OrientClause custom(final String clause) {
		return new OrientClause(new CustomClause(clause));
	}
}
