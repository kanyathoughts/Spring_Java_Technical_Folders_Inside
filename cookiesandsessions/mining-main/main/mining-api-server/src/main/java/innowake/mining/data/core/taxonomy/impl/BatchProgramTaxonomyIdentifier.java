/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.impl;

import static innowake.mining.shared.model.TechnicalTaxonomies.Name.BATCH;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import innowake.mining.data.core.taxonomy.api.DependencyEdge;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.springframework.stereotype.Component;

/**
 * Taxonomy identifier for identifying batch program taxonomies.
 */
@Component
public class BatchProgramTaxonomyIdentifier implements TaxonomyIdentifier<DependencyModule> {
	
	private static final List<Tuple2<Name, TypeName>> IDENTIFIED_TAXONOMIES = new ArrayList<>();
	
	static {
		IDENTIFIED_TAXONOMIES.add(TechnicalTaxonomies.tuple(BATCH, TypeName.PROGRAM_TYPE));
	}

	@Override
	public List<Tuple2<Name, TypeName>> identify(final DependencyModule module) {
		final Type moduleType = module.getType();
		if (moduleType != Type.PROGRAM && moduleType != Type.MAINPROGRAM) {
			return Collections.emptyList();
		}
		final boolean moduleHasValidIncomingCall = module.getIncomings().stream()
				.filter(edge -> edge.getRelationship() == RelationshipType.CALLS)
				.map(DependencyEdge::getIncomingModule)
				.anyMatch(incomingModule -> (Technology.JCL == incomingModule.getTechnology()
						|| (Technology.VMS == incomingModule.getTechnology()
							&& Type.DCL == incomingModule.getType()))
						|| (Technology.ECL == incomingModule.getTechnology()
							&& Type.ECL_JOB == incomingModule.getType()));
		
		if ( ! moduleHasValidIncomingCall) {
			return Collections.emptyList();
		}
		
		return IDENTIFIED_TAXONOMIES;
	}

}
