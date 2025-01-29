/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.service;

import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.genai.RequestMetadataUtil;
import innowake.mining.server.service.prompt.GenAiPromptService;
import innowake.mining.shared.model.codeviewer.AssembledContent;
import org.springframework.stereotype.Service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.genai.requestresponse.DeduceRequestModel;
import innowake.mining.server.genai.requestresponse.DeduceResponseModel;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;


/**
 * Generative AI translation for Module Description Related Services
 */
@Service
public class GenerativeModuleDescriptionService extends CentralCommunicationWithGenAIService {

	private static final Logger LOG = LoggerFactory.getLogger(GenerativeModuleDescriptionService.class);
	private final ContentAssemblingService contentAssemblingService;

	public GenerativeModuleDescriptionService(final GenericConfigProperties configProperties, final ModuleService moduleService,
			final GenAIAvailabilityService availabilityService, final GenAIModulePermissionChecker permissionChecker,
			final ContentAssemblingService contentAssemblingService, final GenAiPromptService promptService) {
		super(configProperties, moduleService, availabilityService, permissionChecker, promptService);
		this.contentAssemblingService = contentAssemblingService;
	}

	/**
	 * Create Description for a module using Generative AI.
	 *
	 * @param module module to generate a description for (content should be set)
	 * @return module with new description generated using generative AI
	 */
	public String deduceDescription(final ModulePojo module) {
		checkPermission(module);
		final DeduceResponseModel result;
		result = sendGenAiRequest(module);
		timestampToTokenCountMap.put(System.currentTimeMillis(), Integer.valueOf(result.getResponseMetadata().get("token_count")));
		final String purpose = result.getPurpose();
		if (purpose == null || purpose.isBlank()) {
			LOG.error("The returned result from the GenAI backend service was null for module: {} {}", module.getId(), result);
			throw new UserFacingException("The returned result from the GenAI backend service was null for module: " + module.getId());
		} else {
			return markContent(result.getPurpose()).trim();
		}
	}


	/**
	 * Create Description for a module using Generative AI.
	 *
	 * @param projectId Id of the project
	 * @param moduleId Id of the Module
	 * @return module with new description generated using generative AI
	 */
	public ModulePojo deduceDescription(final EntityId projectId, final EntityId moduleId) {
		final ModulePojo module = getModuleService().findAnyModule(q -> q.ofProject(projectId)
																		.byId(moduleId)
																		.includeContent(true))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with id: " + moduleId + " in project: " + projectId));
		checkPermission(module);
		return setDescription(module, deduceDescription(module));
	}


	private DeduceResponseModel sendGenAiRequest(final ModulePojo module) {
		final String content = getAssembledContent(module);
		final String language = module.getTechnology().name();
		final DeduceRequestModel request = new DeduceRequestModel(getConfigProperties().getGenAiPlugin(), content, getConfigProperties().getGenAiMaxNewToken(),
				getConfigProperties().getGenAiTemperature(), getConfigProperties().getGenAiDoSample(), language, RequestMetadataUtil.getModuleMetadata(module));
		return callGenAi(request, DeduceResponseModel.class);
	}

	public String getAssembledContent(final ModulePojo module) {
		checkModuleContent(module);
		final String moduleContent = module.getContent().orElse("");
		try {
			final AssembledContent assembledContent = contentAssemblingService.getModuleAssembledContent(module);
			return assembledContent.isAvailable() ? assembledContent.getContent() : moduleContent;
		} catch (final Exception e) {
			LOG.error("Could not assemble content for module: " + module.getName() + " (ID " + module.getId() + ")", e);
			return moduleContent;
		}
	}

	private static ModulePojo setDescription(final ModulePojo module, final String description) {
		return new ModulePojo(module.getUid(),
							  module.getId(),
							  module.getCustomProperties(),
							  module.getProject(), null, null,
							  module.getName(),
							  module.getPath().orElse(null),
							  module.getTechnology(),
							  module.getType(),
							  module.getStorage(),
							  module.getOrigin(),
							  module.getCreator(),
							  module.getIdentification(),
							  module.getInfo().orElse(null),
							  description,
							  module.getSource().orElse(null),
							  module.getContentHash().orElse(null),
							  module.getLinkHash(),
							  module.getLocation().orElse(null),
							  module.getRepresentation().orElse(null),
							  module.isRequiresReview(),
							  module.getModifiedDate().orElse(null),
							  module.getMetricsDate().orElse(null),
							  module.getSourceMetrics().orElse(null),
							  module.getContent().orElse(null),
							  module.getErrors(),
							  module.getStatements(),
							  module.getSqlStatements(),
							  module.isSourceCodeAvailable(),
							  module.getParent().orElse(null), null, null,
							  module.getParentPath().orElse(null),
							  module.getDependencyHash().orElse(null));
	}
}
