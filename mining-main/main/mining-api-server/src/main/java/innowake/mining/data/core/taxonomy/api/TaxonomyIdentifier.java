/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.api;

import java.util.List;

import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;

/**
 * Identification of technical Taxonomy information.
 * 
 * @param <T> the type on which the analysis takes place.
 */
public interface TaxonomyIdentifier<T> {
	
	/**
	 * Identifies {@linkplain TaxonomyPojo Taxonomies} on a given object.
	 *
	 * @param object the object on which the analysis should be done on
	 * @return a list of identified {@linkplain TaxonomyPojoPrototype Taxonomies}; not {@code null}
	 */
	List<Tuple2<Name, TypeName>> identify(T object);

}
