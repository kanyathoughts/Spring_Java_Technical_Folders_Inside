/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Optional;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.moduledescription.impl.DescriptionExtractor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;

/**
 * Identifies module descriptions.
 */
public class ModuleDescriptionIdentifier {
	
	private static final int LUCENE_MAX_INDEX_LENGTH = 32766;
	
	private ModuleDescriptionIdentifier() {}

	/**
	 * Identifies the module description for the given {@code moduleId}.
	 *
	 * @param moduleId the Id of the module for which the description should be identified
	 * @param moduleService Module data access service.
	 * @return the identification result
	 */
	@Nullable
	public static Object identify(final EntityId moduleId, final ModuleService moduleService) {
		final Optional<ModulePojo> moduleVertex = moduleService.findAnyModule(q -> q.byId(moduleId).includeContent(true));
		if (moduleVertex.isEmpty()) {
			return String.format("Module with id %s could not be found.", moduleId.toString());
		}

		final ModulePojo module = moduleVertex.get();
		/*
		 * Skip-fast if the module already has a description.
		 * The module description must be empty to be overridden by this implementation.
		 * The description-property is already loaded. I assume this property check is faster for any module type than resolving the object-type link to 
		 * filter for modules of interest.
		 */
		@Nullable final String existingDescription = module.getDescription().orElse(null);
		if (StringUtils.isNotEmpty(existingDescription)) {
			return null;
		}

		final String description = getContentBasedDescription(module);
		if (description == null || description.length() == 0) {
			return "No handler for module with id:" + moduleId;
		}

		return moduleService.updateModules(q -> q.byId(moduleId), new ModulePojoPrototype()
																		.setDescription(limitDescriptionLength(description))
																		.setModifiedDate(Instant.now()));
	}
	
	@Nullable
	private static String getContentBasedDescription(final ModulePojo module) {
		@Nullable final String content = module.getContent().orElse(null);
		if (content != null) {
			switch (module.getType()) {
				case PROGRAM:
				case SUBPROGRAM:
				case SUBROUTINE:
				case OBJECT:
					switch (module.getTechnology()) {
						case COBOL:
							return DescriptionExtractor.FOR_COBOL.getDescription(content);
						case BASIC:
							return DescriptionExtractor.FOR_BASIC.getDescription(content);
						case NATURAL:
							return DescriptionExtractor.FOR_NATURAL.getDescription(content);
						default:
							return null;
					}
				case BMS_MAPSET:
					return DescriptionExtractor.FOR_BMS.getDescription(content);
				case JOB:
				case PROC:
					return DescriptionExtractor.FOR_JCL.getDescription(content);
				default:
					return null;
			}
		}

		return null;
	}

	private static String limitDescriptionLength(final String description) {
		/*
		 * Avoid java.lang.IllegalArgumentException: DocValuesField "description" is too large, must be <= 32766 in lucene
		 * 	at org.apache.lucene.index.SortedDocValuesWriter.addValue(SortedDocValuesWriter.java:73)
		 *	at org.apache.lucene.index.DefaultIndexingChain.indexDocValue(DefaultIndexingChain.java:550)
		 *  ...
		 *  
		 *  The check is done by counting bytes in lucene. In rare situations, one character could result in multiple bytes.
		 *  To address this, the index length is set to 32500 which leave some space for this double bytes for single character.
		 */
		final byte[] rawBytes = description.getBytes(StandardCharsets.UTF_8);
		if (rawBytes.length > LUCENE_MAX_INDEX_LENGTH) {
			return new String(rawBytes, 0, LUCENE_MAX_INDEX_LENGTH, StandardCharsets.UTF_8);
		}
		return description;
	}
}
