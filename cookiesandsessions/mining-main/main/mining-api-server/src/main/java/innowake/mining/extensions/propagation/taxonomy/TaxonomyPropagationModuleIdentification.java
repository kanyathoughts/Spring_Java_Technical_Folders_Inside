/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.propagation.taxonomy;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.CLIENT_ADMIN;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Service;

import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.taxonomy.assignment.PropagationData;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Taxonomy PrePropagation with provision to run as a Job
 */
@Service
public class TaxonomyPropagationModuleIdentification implements MiningJobExtension<ArrayList<PropagationData>> {

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
		return "taxonomy-propagation-module-identification";
	}

	@Override
	public String getDescription() {
		return "Identifying all the Modules to be affected by the Propagation job";
	}

	@Override
	public Job<ArrayList<PropagationData>> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		final List<EntityId> taxonomyIds = Optional.ofNullable(parameters.get("taxonomyIds"))
													.map(list -> list.stream().map(EntityId::of).collect(Collectors.toList()))
													.orElse(Collections.emptyList());
		final List<DatabaseAccessType> accesses;
		final List<RelationshipType> incomings = Optional.ofNullable(parameters.get("incomingReferences"))
														.map(list -> list.stream().map(RelationshipType::from).collect(Collectors.toList()))
														.orElse(Collections.emptyList());
		final List<RelationshipType> outgoings = Optional.ofNullable(parameters.get("outgoingReferences"))
														.map(list -> list.stream().map(RelationshipType::from).collect(Collectors.toList()))
														.orElse(Collections.emptyList());
		final List<EntityId> moduleIds = parameters.get("moduleIds").stream().map(EntityId::of).collect(Collectors.toList());
		if (incomings.contains(RelationshipType.ACCESSES) || outgoings.contains(RelationshipType.ACCESSES)) {
			accesses = Optional.ofNullable(parameters.get("readsWritesAccesses"))
								.map(list -> list.stream().map(DatabaseAccessType::getByValue).collect(Collectors.toList()))
								.orElse(Collections.emptyList());

			incomings.removeIf(reference -> reference == RelationshipType.ACCESSES);
			outgoings.removeIf(reference -> reference == RelationshipType.ACCESSES);
		} else {
			accesses = Collections.emptyList();
		}

		final TaxonomyPropagationRequest taxonomyPropagationRequest = new TaxonomyPropagationRequest(moduleIds, taxonomyIds, incomings, outgoings, accesses);
		return new TaxonomyPropagationModuleIdentificationJob(projectId, taxonomyPropagationRequest);
	}
}
