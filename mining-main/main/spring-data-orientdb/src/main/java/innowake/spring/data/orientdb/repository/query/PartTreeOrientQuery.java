/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import org.springframework.data.repository.query.ParameterAccessor;
import org.springframework.data.repository.query.Parameters;
import org.springframework.data.repository.query.ParametersParameterAccessor;
import org.springframework.data.repository.query.parser.PartTree;

import innowake.spring.data.orientdb.commons.core.OrientOperations;

/**
 * A {@link AbstractOrientQuery} implementation based on a {@link PartTree}.
 * Used to build named queries.
 */
public class PartTreeOrientQuery extends AbstractOrientQuery {

	private final Class<?> domainClass;
	private final PartTree tree;
	private final Parameters<?, ?> parameters;
	private final OrientQueryMethod method;

	/**
	 * Instantiates a new {@link PartTreeOrientQuery} from given {@link OrientQueryMethod} and {@link OrientOperations}.
	 *
	 * @param method the query method
	 * @param operations the orient object template
	 */
	public PartTreeOrientQuery(final OrientQueryMethod method, final OrientOperations<?> operations) {
		super(method, operations);

		this.method = method;
		domainClass = method.getEntityInformation().getJavaType();
		tree = new PartTree(method.getName(), domainClass);
		parameters = method.getParameters();
	}

	@Override
	protected boolean isCountQuery() {
		return tree.isCountProjection();
	}

	@Override
	protected boolean isDeleteQuery() {
		return tree.isDelete();
	}

	@Override
	protected String toSql(final Object[] values) {
		final ParameterAccessor accessor = new ParametersParameterAccessor(parameters, values);
		final OrientQueryCreator creator = new OrientQueryCreator(tree, method, accessor);
		return creator.createQuery();
	}

	@Override
	protected String toSqlCount(final Object[] values) {
		final ParameterAccessor accessor = new ParametersParameterAccessor(parameters, values);
		final OrientCountQueryCreator creator = new OrientCountQueryCreator(tree, method, accessor);
		return creator.createQuery();
	}

}
