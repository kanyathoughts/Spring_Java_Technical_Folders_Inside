/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.repository.query;

import org.springframework.data.repository.query.ParameterAccessor;
import org.springframework.data.repository.query.parser.PartTree;

/**
 * Class to build a count query.
 */
public class OrientCountQueryCreator extends OrientQueryCreator {

	/**
	 * Instantiates {@code OrientCountQueryCreator} object.
	 * 
	 * @param tree instance of {@link PartTree}
	 * @param method to derive the query from it's name
	 * @param parameters values to be passed to the query
	 */
	public OrientCountQueryCreator(final PartTree tree, final OrientQueryMethod method, final ParameterAccessor parameters) {
		super(tree, method, parameters);
	}

	@Override
	public final boolean isCountQuery() {
		return true;
	}
}
