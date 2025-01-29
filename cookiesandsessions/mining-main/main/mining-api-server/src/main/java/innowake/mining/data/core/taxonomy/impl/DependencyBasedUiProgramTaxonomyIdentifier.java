/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.impl;
import static innowake.mining.shared.model.TechnicalTaxonomies.Name.UI;
import static innowake.mining.shared.model.Technology.BASIC;
import static innowake.mining.shared.model.Technology.COBOL;
import static innowake.mining.shared.model.Technology.IMS;
import static innowake.mining.shared.model.Technology.PL1;
import static innowake.mining.shared.model.Type.APPLICATION;
import static innowake.mining.shared.model.Type.FUNCTION;
import static innowake.mining.shared.model.Type.IFDL_FORM;
import static innowake.mining.shared.model.Type.MAINPROGRAM;
import static innowake.mining.shared.model.Type.MFS;
import static innowake.mining.shared.model.Type.PROGRAM;
import static innowake.mining.shared.model.Type.SUBROUTINE;

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
 * TaxonomyPojo identifier for identifying ui program taxonomies, based on the inter-module dependencies.
 */
@Component
public class DependencyBasedUiProgramTaxonomyIdentifier implements TaxonomyIdentifier<DependencyModule> {
	

	private static final String ICSCRGET = "ICSCRGET";
	private static final String FORMS$ENABLE = "FORMS$ENABLE";
	private static final List<Tuple2<Name, TypeName>> IDENTIFIED_TAXONOMIES = new ArrayList<>();
	
	static {
		IDENTIFIED_TAXONOMIES.add(TechnicalTaxonomies.tuple(UI, TypeName.PROGRAM_TYPE));
	}
	
	@Override
	public List<Tuple2<Name, TypeName>> identify(final DependencyModule module) {
		
		final Type moduleType = module.getType();
		final Technology moduleTechnology = module.getTechnology();
		if (hasInvalidType(moduleType) || hasInvalidTechnology(moduleTechnology)) {
			return Collections.emptyList();
		}
		final boolean moduleHasValidIncomingCall = module.getIncomings(RelationshipType.CALLS).stream()
				.map(DependencyEdge::getIncomingModule)
				.anyMatch(incomingModule -> ((incomingModule.getType() == IFDL_FORM && moduleType == FUNCTION && moduleTechnology == BASIC)
											|| (incomingModule.getType() == APPLICATION && incomingModule.getTechnology() == IMS)));
		
		if(module.getTechnology() != PL1 && moduleHasValidIncomingCall) {
			return IDENTIFIED_TAXONOMIES;
		}
		
		final boolean moduleHasValidOutgoingCall = module.getOutgoings(RelationshipType.CALLS).stream()
				.map(DependencyEdge::getOutgoingModule)
				.anyMatch(outgoingModule -> (matchesIcscrget(outgoingModule.getName(), moduleType, moduleTechnology)
									|| matchesFormsEnable(outgoingModule.getName(), moduleType, moduleTechnology)
									|| outgoingModule.getType() == MFS));
		
		if (module.getTechnology() == PL1) {
			return (moduleHasValidIncomingCall && moduleHasValidOutgoingCall) ? IDENTIFIED_TAXONOMIES : Collections.emptyList();
		} else if ( ! moduleHasValidOutgoingCall) {
			return Collections.emptyList();
		}

		return IDENTIFIED_TAXONOMIES;
	}
	
	private boolean hasInvalidType(final Type moduleType) {
		return ! (moduleType == PROGRAM || moduleType == FUNCTION || moduleType == SUBROUTINE || moduleType == MAINPROGRAM); 
	}
	
	private boolean hasInvalidTechnology(final Technology moduleTechnology) {
		return ! (moduleTechnology == COBOL || moduleTechnology == BASIC || moduleTechnology == PL1);
	}
	
	private boolean matchesIcscrget(final String outGoingModuleName, final Type moduleType, final Technology moduleTechnology) {
		return outGoingModuleName.equalsIgnoreCase(ICSCRGET) && ((moduleTechnology == COBOL && moduleType == PROGRAM) 
				|| (moduleTechnology == BASIC && (moduleType == PROGRAM || moduleType == FUNCTION || moduleType == SUBROUTINE)));
	}
	
	private boolean matchesFormsEnable(final String outGoingModuleName, final Type moduleType, final Technology moduleTechnology) {
		return outGoingModuleName.equalsIgnoreCase(FORMS$ENABLE) && moduleTechnology == COBOL && moduleType == PROGRAM;
	}
}
