/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.impl;

import static innowake.mining.shared.model.TechnicalTaxonomies.Name.MQ;
import static innowake.mining.shared.model.TechnicalTaxonomies.TypeName.PROGRAM_TYPE;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import innowake.mining.data.core.taxonomy.api.DependencyEdge;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.TechnicalTaxonomies.Name;
import innowake.mining.shared.model.TechnicalTaxonomies.TypeName;
import innowake.mining.shared.model.Type;
import org.springframework.stereotype.Component;

/**
 * Taxonomy identifier for identifying MQ program taxonomies.
 * <p>
 * The list of MQ related programs can be found
 * <a href="https://www.ibm.com/support/knowledgecenter/en/SSFKSJ_7.5.0/com.ibm.mq.ref.dev.doc/q089750_.htm">here</a>
 */
@Component
public class MqProgramTaxonomyIdentifier implements TaxonomyIdentifier<DependencyModule> {
	
	private static final List<Tuple2<Name, TypeName>> IDENTIFIED_TAXONOMIES = new ArrayList<>();
	private static final Set<String> RELEVANT_PROGRAM_NAME = new HashSet<>();
	
	static {
		final Tuple2<Name, TypeName> batchTaxonomy = TechnicalTaxonomies.tuple(MQ, PROGRAM_TYPE);
		
		IDENTIFIED_TAXONOMIES.add(batchTaxonomy);

		RELEVANT_PROGRAM_NAME.add("MQSET");
		RELEVANT_PROGRAM_NAME.add("MQINQ");
		RELEVANT_PROGRAM_NAME.add("MQGET");
		RELEVANT_PROGRAM_NAME.add("MQPUT1");
		RELEVANT_PROGRAM_NAME.add("MQPUT");
		RELEVANT_PROGRAM_NAME.add("MQCLOSE");
		RELEVANT_PROGRAM_NAME.add("MQOPEN");
		RELEVANT_PROGRAM_NAME.add("MQDISC");
		RELEVANT_PROGRAM_NAME.add("MQCONN");
	}

	@Override
	public List<Tuple2<Name, TypeName>> identify(final DependencyModule module) {
		final Type moduleType = module.getType();
		if (moduleType != Type.PROGRAM && moduleType != Type.MAINPROGRAM) {
			return Collections.emptyList();
		}
	
		final boolean moduleHasOutgoingMqCall = module.getOutgoings().stream()
				.filter(edge -> edge.getRelationship() == RelationshipType.CALLS)
				.map(DependencyEdge::getOutgoingModule)
				.map(DependencyModule::getName)
				.anyMatch(RELEVANT_PROGRAM_NAME::contains);
		
		if ( ! moduleHasOutgoingMqCall) {
			return Collections.emptyList();
		}

		return IDENTIFIED_TAXONOMIES;
	}

}
