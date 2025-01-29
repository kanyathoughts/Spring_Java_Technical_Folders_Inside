/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.cfg;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue.FileAccess;
import innowake.mining.server.cfg.CfgNodeMetadata.MetadataType;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * JCL specific implementation of {@link CfgMetadataProvider}. It provides input and output file information in form of {@link CfgDatasetDependenciesMetadata}.
 */
public class JclCfgMetadataProvider implements CfgMetadataProvider {
	
	private static final List<Type> RELEVANT_FILE_TYPES = Arrays.asList(Type.FILE, Type.VSAM_FILE, Type.GDG_FILE);

	private final ModuleService moduleService;

	public JclCfgMetadataProvider(final ModuleService moduleService) {
		this.moduleService = moduleService;
	}
	
	@Override
	public List<CfgNodeMetadata> getMetadata(final ModulePojo module) {
		/* Currently there is only DATASET_DEPENDENCIES metadata so it is returned */
		return getMetadata(module, MetadataType.DATASET_DEPENDENCIES);
	}

	/**
	 * Processes the outgoing {@code ACCESSES} references to obtain output files (WRITE-relationships) and input files (READ-relationships).
	 * 
	 * @return list of available {@link CfgDatasetDependenciesMetadata} of the specified module or empty list if a MetadataType except DATASET_DEPENDENCIES was 
	 * provided.
	 */
	@Override
	public List<CfgNodeMetadata> getMetadata(final ModulePojo module, final MetadataType type) {
		if ( ! type.equals(MetadataType.DATASET_DEPENDENCIES)) {
			return Collections.emptyList();
		}

		final var outputFiles = moduleService.findModules(b -> b.withTechnology(Technology.RESOURCE)
															.withTypes(RELEVANT_FILE_TYPES)
															.withSourceRelationshipsFrom(module.identity(), RelationshipType.ACCESSES)
															.withRelationshipProperty(ModelAttributeKey.FILE_ACCESS_TYPE.name(), FileAccess.WRITE.name(), true));

		final var inputFiles = moduleService.findModules(b -> b.withTechnology(Technology.RESOURCE)
															.withTypes(RELEVANT_FILE_TYPES)
															.withSourceRelationshipsFrom(module.identity(), RelationshipType.ACCESSES)
															.withRelationshipProperty(ModelAttributeKey.FILE_ACCESS_TYPE.name(), FileAccess.READ.name(), true));

		return inputFiles.isEmpty() && outputFiles.isEmpty() ? Collections.emptyList() : Collections.singletonList(new CfgDatasetDependenciesMetadata(outputFiles, inputFiles));
	}
}
