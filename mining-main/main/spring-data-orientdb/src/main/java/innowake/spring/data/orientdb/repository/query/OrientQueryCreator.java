/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import static com.github.raymanrt.orientqb.query.Clause.clause;
import static com.github.raymanrt.orientqb.query.Clause.not;
import static innowake.lib.core.lang.Assert.assertNotNull;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Optional;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mapping.PropertyPath;
import org.springframework.data.repository.query.ParameterAccessor;
import org.springframework.data.repository.query.parser.AbstractQueryCreator;
import org.springframework.data.repository.query.parser.Part;
import org.springframework.data.repository.query.parser.PartTree;
import org.springframework.lang.Nullable;
import org.springframework.util.ReflectionUtils;

import com.github.raymanrt.orientqb.query.Clause;
import com.github.raymanrt.orientqb.query.Operator;
import com.github.raymanrt.orientqb.query.Query;
import com.gitub.raymanrt.orientqb.delete.Delete;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.springdata.annotations.EdgeQuery;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.spring.data.orientdb.commons.exception.UnsupportedQueryTypeException;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinition;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinitionMapper;
import innowake.spring.data.orientdb.ogm.mapping.util.RelationshipUtils;
import innowake.spring.data.orientdb.repository.query.clauses.ClauseWithInOperator;
import innowake.spring.data.orientdb.repository.query.clauses.ClauseWithParenthesis;
import innowake.spring.data.orientdb.repository.query.clauses.EdgeClause;
import innowake.spring.data.orientdb.repository.query.clauses.EmbeddedMapClause;
import innowake.spring.data.orientdb.repository.query.clauses.EmbeddedSetClause;

/**
 * Creates the orientDB query using {@code OrientQB} libraries and {@link Part} information.
 */
public class OrientQueryCreator extends AbstractQueryCreator<String, Clause> {

	private static final Logger LOGGER = LoggerFactory.getLogger(OrientQueryCreator.class);

	private static final String SORT_VAR = "$sort";

	private final PartTree tree;
	private final ParameterAccessor accessor;
	private final OrientQueryMethod method;
	private final String entityName;
	private final Class<?> entityClass;

	/**
	 * Instantiates {@code OrientQueryCreator} object.
	 * 
	 * @param tree instance of {@link PartTree}
	 * @param method to derive the query from it's name
	 * @param accessor values to be passed to the query
	 */
	public OrientQueryCreator(final PartTree tree, final OrientQueryMethod method, final ParameterAccessor accessor) {
		super(tree, accessor);
		this.tree = tree;
		this.accessor = accessor;
		this.method = method;
		entityClass = method.getEntityInformation().getJavaType();
		entityName = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(entityClass).getEntityName();
	}

	@Override
	protected Clause create(final Part part, final Iterator<Object> iterator) {
		return buildQueryClause(part, iterator);
	}

	@Override
	protected Clause and(final Part part, final Clause base, final Iterator<Object> iterator) {
		return Clause.and(base, buildQueryClause(part, iterator));
	}

	@Override
	protected Clause or(final Clause base, final Clause criteria) {
		return Clause.or(base, criteria);
	}

	@Override
	protected String complete(@Nullable final Clause criteria, final Sort sort) {
		if (isDeleteQuery()) {
			final Delete delete = new Delete();
			return delete.from(getSubject()).where(criteria).toString();
		} else if (isCountQuery()) {
			final Query countQuery = new Query();
			return countQuery.select("count(*)").from(entityName).where(criteria).toString();
		}
		return buildQueryWithPaginationAndSorting(criteria, sort);
	}

	/**
	 * Determines if a count query is to be built.
	 *
	 * @return true if it's a count query
	 */
	protected boolean isCountQuery() {
		return tree.isCountProjection();
	}

	/**
	 * Determines if a delete query needs to be built.
	 *
	 * @return true if it's a delete query
	 */
	private boolean isDeleteQuery() {
		return tree.isDelete();
	}
	
	/**
	 * Gets subject string to operate on.
	 *
	 * @return subject string to operate on
	 */
	private String getSubject() {
		return entityClass.getAnnotation(Entity.class).isDocumentClass() ? "FROM " + entityName : "VERTEX " + entityName;
	}

	private Clause buildQueryClause(final Part part, final Iterator<Object> iterator) {
		final PropertyPath propertyPath = part.getProperty();
		final Clause clause;
		if (propertyPath.hasNext()) {
			clause = createNestedQueryClause(part, iterator);
		} else if (propertyPath.getTypeInformation().isMap() && iterator.hasNext()) {
			final String key = (String) iterator.next();
			if (iterator.hasNext()) {
				clause = new EmbeddedMapClause(propertyPath.getSegment(), key, iterator.next());
			} else {
				throw new UnsupportedQueryTypeException("Insufficient number of arugments");
			}
		} else if (propertyPath.getTypeInformation().isCollectionLike() && iterator.hasNext()) {
			clause = new EmbeddedSetClause(iterator.next(), Operator.IN, propertyPath.getSegment());
		} else {
			clause = createClause(part, iterator, propertyPath.getLeafProperty().getSegment());
		}
		LOGGER.debug(() -> "Criteria :: " + clause.toString());
		return clause;

	}

	private Clause createNestedQueryClause(final Part part, final Iterator<Object> iterator) {
		final PropertyPath propertyPath = part.getProperty();
		@innowake.lib.core.api.lang.Nullable final EdgeQuery edgeQuery = method.getQueryMethod().getAnnotation(EdgeQuery.class);
		final Clause clause;
		if (edgeQuery != null) {
			final Field field = ReflectionUtils.findField(propertyPath.getOwningType().getType(), propertyPath.getSegment());
			final String edgeName = RelationshipUtils.getGraphRelationshipName(assertNotNull(field));
			final Clause subClause;
			if (propertyPath.isCollection() && iterator.hasNext()) {
				subClause = clause(propertyPath.getLeafProperty().getSegment(), Operator.CONTAINS, iterator.next());
			} else {
				subClause = createClause(part, iterator, propertyPath.getLeafProperty().getSegment());
			}
			clause = new EdgeClause(edgeName, edgeQuery.direction(), subClause);
		} else if (propertyPath.getTypeInformation().isCollectionLike()) {
			final Clause subClause = new ClauseWithParenthesis(createClause(part, iterator, propertyPath.getLeafProperty().getSegment()));
			clause = clause(propertyPath.getSegment(), Operator.CONTAINS, subClause);
		} else {
			clause = createClause(part, iterator, getDotPathFromPropertyPath(propertyPath, entityClass));
		}
		return clause;
	}
	
	private String getDotPathFromPropertyPath(final PropertyPath propertyPath, final Class<?> type) {
		final String segment = propertyPath.getSegment();
		final Optional<String> field;
		try {
			final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(type);
			field = classDefinition.getDbFieldNameFromJavaFieldName(segment);
		} catch (Exception e) {
			throw new UnsupportedQueryTypeException("Unsupported value assigned", e);
		}
		final String propertyValue = field.isPresent() ? field.get() : segment;
		if (propertyPath.hasNext()) {
			return propertyValue + "." + getDotPathFromPropertyPath(Assert.assertNotNull(propertyPath.next()), propertyPath.getType());
		}
		return propertyValue;
	}

	private String buildQueryWithPaginationAndSorting(@Nullable final Clause criteria, final Sort sort) {
		final OrientQuery query = new OrientQuery();
		final Pageable pageable = accessor.getPageable();
		query.from(entityName).where(criteria);
		if (pageable.isPaged()) {
			((OrientQuery) query.limit(pageable.getPageSize())).offset(pageable.getOffset());
		}
		query.select("*");
		int sortIndex = 0;
		for (final Sort.Order sortOrder : sort) {
			final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(entityClass);
			final Optional<String> dbProperty = classDefinition.getDbFieldNameFromJavaFieldName(sortOrder.getProperty());
			String propertyName = dbProperty.isPresent() ? dbProperty.get() : sortOrder.getProperty();
			if (sortOrder.isIgnoreCase()) {
				propertyName += ".toLowerCase()";
			}
			final String propertyNameFinal = propertyName;
			query.let("sort" + sortIndex, () -> propertyNameFinal);
			if (sortOrder.isAscending()) {
				query.orderBy(SORT_VAR + sortIndex);
			} else {
				query.orderByDesc(SORT_VAR + sortIndex);
			}
			query.select(SORT_VAR + sortIndex);
			sortIndex++;
		}
		LOGGER.debug(() -> "Query :: " + query.toString());
		return query.toString();
	}

	private Clause createClause(final Part part, final Iterator<Object> iterator, final String property) {
		final ClassDefinition classDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(entityClass);
		final Optional<String> dbProperty = classDefinition.getDbFieldNameFromJavaFieldName(property);
		final String propertyName = dbProperty.isPresent() ? dbProperty.get() : property;
		final Part.Type type = part.getType();
		final PropertyPath propertyPath = part.getProperty();
		final Object[] propertyValues;
		final String propertyQueried;
		if (iterator.hasNext()) {
			if (Enum.class.isAssignableFrom(propertyPath.getLeafType())) {
				propertyValues = (type == Part.Type.IN) ? ((Collection<?>) iterator.next()).stream().map(s -> ((Enum<?>) s).name()).toArray(Object[]::new)
											: new Object[] {((Enum<?>) iterator.next()).name()};
				propertyQueried = propertyName + ".name";
			} else {
				propertyQueried = propertyName;
				propertyValues = (type == Part.Type.IN) ? ((Collection<?>) iterator.next()).stream().toArray(Object[]::new) : new Object[] {iterator.next()};
			}
		} else {
			return createClauseWithoutParams(propertyName, type);
		}
		switch (type) {
			case AFTER:
			case GREATER_THAN:
				return clause(propertyQueried, Operator.GT, propertyValues[0]);
			case GREATER_THAN_EQUAL:
				return clause(propertyQueried, Operator.GE, propertyValues[0]);
			case BEFORE:
			case LESS_THAN:
				return clause(propertyQueried, Operator.LT, propertyValues[0]);
			case LESS_THAN_EQUAL:
				return clause(propertyQueried, Operator.LE, propertyValues[0]);
			case REGEX:
				return clause(propertyQueried, Operator.MATCHES, propertyValues[0]);
			case LIKE:
				return clause(propertyQueried, Operator.LIKE, propertyValues[0]);
			case NOT_LIKE:
				return not(clause(propertyQueried, Operator.LIKE, propertyValues[0]));
			case CONTAINING:
				return clause(propertyQueried, Operator.CONTAINS, propertyValues[0]);
			case IN:
				return new ClauseWithInOperator(propertyQueried, Arrays.asList(propertyValues));
			case SIMPLE_PROPERTY:
				return clause(propertyQueried, Operator.EQ, propertyValues[0]);
			case NEGATING_SIMPLE_PROPERTY:
				return clause(propertyQueried, Operator.NE, propertyValues[0]);
			default:
				throw new UnsupportedQueryTypeException("Unsupported query type " + type);
		}
	}

	private Clause createClauseWithoutParams(final String propertyName, final Part.Type type) {
		switch (type) {
			case TRUE:
				return clause(propertyName, Operator.EQ, Boolean.TRUE);
			case FALSE:
				return clause(propertyName, Operator.EQ, Boolean.FALSE);
			case IS_NULL:
				return clause(propertyName, Operator.NULL, StringUtils.EMPTY);
			case IS_NOT_NULL:
				return clause(propertyName, Operator.NOT_NULL, StringUtils.EMPTY);
			default:
				throw new UnsupportedQueryTypeException("Unsupported query type " + type);
		}
	}

}
