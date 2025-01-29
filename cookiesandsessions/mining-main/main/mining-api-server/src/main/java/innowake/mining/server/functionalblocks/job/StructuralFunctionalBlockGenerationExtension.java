/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job;

import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.io.ParameterDescription;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

//TODO: for testing / prototype only! remove this class
@Component
public class StructuralFunctionalBlockGenerationExtension implements MiningJobExtension<Serializable> {

	@Autowired
	private FF4j ff4j;

	@Override
	public String getIdentifier() {
		return "structural-functional-block";
	}

	@Override
	public String getDescription() {
		return "Generate Structural Functional Blocks";
	}

	@Override
	public Job<Serializable> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		//TODO: populate module matcher
		final Optional<String> taxonomy = Optional.ofNullable(parameters.get("Taxonomy")).flatMap(p -> p.stream().findAny());
		return new StructuralFunctionalBlockGenerationJob(projectId, new ModuleMatcher(Collections.emptyList(), Collections.emptyList()),
				taxonomy.orElse(null));
	}

	@Override
	public NatureType getRequiredNature() {
		return NatureType.MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.MANAGER;
	}

	@Override
	public ShowOnExportPage getShowOnExportPage() {
		if (ff4j.getFeature(FeatureId.GENERATE_FUNCTIONAL_BLOCK_AS_PER_MODULE_STRUCTURE.getId()).isEnable()) {
			return new ShowOnExportPage(true, "Functional Analysis", "Generate Structural Functional Blocks");
		} else {
			return new ShowOnExportPage(false, "Functional Analysis", "Generate Structural Functional Blocks");
		}
	}

	@Override
	public List<ParameterDescription> getParameterDescriptions() {
		return List.of(new ParameterDescription("Taxonomy",
				"Will only execute on Modules with this Taxonomy, if specified. Note: you can leave this blank - type e.g. space"
						+ " to enable the Export button \"-.-", ParameterDescription.ParameterType.STRING, false, "", false, "", Collections.emptyList()));
	}
}
