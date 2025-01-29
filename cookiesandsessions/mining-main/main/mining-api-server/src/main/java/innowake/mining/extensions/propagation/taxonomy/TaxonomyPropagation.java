/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.propagation.taxonomy;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.CLIENT_ADMIN;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Service;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.MimeResult;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.taxonomy.assignment.PropagationData;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Taxonomy Propagation with provision to run as a Job. </br>
 * Taxonomy Propagation allows taxonomies assigned to the start modules to be automatically propagated to its child modules. </br>
 */
@Service
public class TaxonomyPropagation implements MiningJobExtension<MimeResult> {
	
	private List<PropagationData> propagationData = new ArrayList<>();

	@Override
	public NatureType getRequiredNature() {
		return MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return CLIENT_ADMIN;
	}

	@Override
	public String getIdentifier() {
		return "taxonomyPropagation";
	}

	@Override
	public String getDescription() {
		return "Taxonomy Propagation";
	}

	@Override
	public Job<MimeResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new TaxonomyPropagationJob(projectId, propagationData);
	}
	
	/**
	 * Method for setting the taxonomy propagationData
	 *
	 * @param propagationData the taxonomy propagation data
	 */
	public void setPropagationData(final List<PropagationData> propagationData) {
		this.propagationData = propagationData;
	}

}