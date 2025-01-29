/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.impl;

import static innowake.mining.shared.model.ReferenceAttributes.DB_ACCESS_TYPES;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.DELETE;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.READ;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.STORE;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.UPDATE;
import static innowake.mining.shared.model.Type.MAINPROGRAM;
import static innowake.mining.shared.model.Type.PROGRAM;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.data.core.taxonomy.api.DependencyEdge;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.mining.shared.model.Type;
import org.springframework.stereotype.Component;

/**
 * Taxonomy identifier for identifying database access taxonomies.
 */
@Component
public class DatabaseAccessTaxonomy implements TaxonomyIdentifier<DependencyModule> {

	private static final Map<String, Tuple2<Name, TypeName>> PROPERTY_TO_TAXONOMY = new HashMap<>();
	private static final Tuple2<Name, TypeName> READ_TAXONOMY;
	private static final Tuple2<Name, TypeName> STORE_TAXONOMY;
	private static final Tuple2<Name, TypeName> UPDATE_TAXONOMY;
	private static final Tuple2<Name, TypeName> DELETE_TAXONOMY;
	
	static {
		READ_TAXONOMY = TechnicalTaxonomies.tuple(READ, TypeName.DB_ACCESS);
		STORE_TAXONOMY = TechnicalTaxonomies.tuple(STORE, TypeName.DB_ACCESS);
		UPDATE_TAXONOMY = TechnicalTaxonomies.tuple(UPDATE, TypeName.DB_ACCESS);
		DELETE_TAXONOMY = TechnicalTaxonomies.tuple(DELETE, TypeName.DB_ACCESS);

		PROPERTY_TO_TAXONOMY.put(DatabaseAccessType.READ.name(), READ_TAXONOMY);
		PROPERTY_TO_TAXONOMY.put(DatabaseAccessType.STORE.name(), STORE_TAXONOMY);
		PROPERTY_TO_TAXONOMY.put(DatabaseAccessType.UPDATE.name(), UPDATE_TAXONOMY);
		PROPERTY_TO_TAXONOMY.put(DatabaseAccessType.DELETE.name(), DELETE_TAXONOMY);
	}

	@Override
	public List<Tuple2<Name, TypeName>> identify(final DependencyModule object) {
		final Type type = object.getType();
		if (PROGRAM != type && MAINPROGRAM != type) {
			return Collections.emptyList();
		}
		
		return object.getOutgoings().stream()
				.filter(edge -> edge.getRelationship() == RelationshipType.ACCESSES)
				.map(DependencyEdge::getProperties)
				.flatMap(props -> {
					final Object value = props.get(DB_ACCESS_TYPES.getReferenceAttributeExcelName());
					return value instanceof Collection ? ((Collection<?>) value).stream() : Stream.of(value);
				})
				.filter(Objects::nonNull)
				.map(Object::toString)
				.flatMap(this::getTaxonomies)
				.distinct()
				.collect(Collectors.toList());
	}
	
	private Stream<Tuple2<Name, TypeName>> getTaxonomies(final String property) {
		return Arrays.stream(property.split(","))
				.filter(PROPERTY_TO_TAXONOMY::containsKey)
				.map(PROPERTY_TO_TAXONOMY::get);
	}

}
