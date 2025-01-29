/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.impl;

import static innowake.mining.shared.model.TechnicalTaxonomies.Name.UI;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.PROGRAM_TYPE;

import java.util.ArrayList;
import java.util.List;

import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import org.springframework.stereotype.Component;

/**
 * Taxonomy identifier for identifying UI programs types.
 */
@Component
public class UiProgramTaxonomyIdentifier implements TaxonomyIdentifier<AstNodePojo> {
	
	private static final Tuple2<Name, TypeName> TAXONOMY;
	
	private final AstNodeCollector collector;
	
	static {
		TAXONOMY = TechnicalTaxonomies.tuple(UI, PROGRAM_TYPE);
	}
	
	/**
	 * Creates a new UI program type Taxonomy identifier.
	 */
	public UiProgramTaxonomyIdentifier() {
		collector = new AstNodeCollector(node -> node.getSuperTypes().contains(AstNodeUtils.UI_STATEMENT));
	}

	@Override
	public List<Tuple2<Name, TypeName>> identify(final AstNodePojo node) {
		final List<Tuple2<Name, TypeName>> result = new ArrayList<>();
		collector.firstDeep(node).ifPresent(astNode -> result.add(TAXONOMY));
		return result;
	}

}
