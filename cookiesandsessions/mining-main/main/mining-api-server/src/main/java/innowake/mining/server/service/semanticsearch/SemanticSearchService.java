package innowake.mining.server.service.semanticsearch;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.Logging;
import innowake.mining.server.error.UserFacingException;
import innowake.mining.server.genai.RequestMetadataUtil;
import innowake.mining.server.genai.ResponseFormat;
import innowake.mining.server.genai.requestresponse.BaseResponseModel;
import innowake.mining.server.genai.requestresponse.CustomPromptRequest;
import innowake.mining.server.permission.GenAIModulePermissionChecker;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.CentralCommunicationWithGenAIService;
import innowake.mining.server.service.GenAIAvailabilityService;
import innowake.mining.server.service.prompt.GenAiPromptService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.Technology;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
public class SemanticSearchService extends CentralCommunicationWithGenAIService {


	private static final List<String> MODULE_SAVED_SEARCHES = Arrays.asList("Missing Source Files", "No Taxonomies Assigned", "Not Referenced", "Requires Review",
			"Taxonomy Assignments");
	private static final List<String> ANNOTATION_SAVED_SEARCHES = Arrays.asList("Business Rule Candidates", "Database Candidates");
	private static final List<String> DICTIONARY_SAVED_SEARCHES = Arrays.asList("Business Related DD Entries", "Copybook Defined DD Entries", "Not Referenced DD entries");
	public static final List<String> SAVED_SEARCHES = Stream.of(MODULE_SAVED_SEARCHES, ANNOTATION_SAVED_SEARCHES, DICTIONARY_SAVED_SEARCHES)
			.flatMap(List::stream)
			.collect(Collectors.toList());

	private static final Logger LOG = LoggerFactory.getLogger(Logging.SERVICE);

	public SemanticSearchService(final GenericConfigProperties configProperties, final ModuleService moduleService, final GenAIAvailabilityService availabilityService,
			final GenAIModulePermissionChecker permissionChecker, final GenAiPromptService promptService) {
		super(configProperties, moduleService, availabilityService, permissionChecker, promptService);
	}

	public SemanticSearchResultPojo doSemanticSearch(final EntityId projectId, final String query) {
		final HttpServletRequest httpRequest = ((ServletRequestAttributes) Assert.assertNotNull(RequestContextHolder.getRequestAttributes())).getRequest();
		try {
			final String prompt = promptService.buildSimilaritySearchPrompt(query, SAVED_SEARCHES.toString());

			final var request = new CustomPromptRequest(getConfigProperties().getGenAiPlugin(), prompt,
					getConfigProperties().getGenAiMaxNewToken(), getConfigProperties().getGenAiTemperature(), getConfigProperties().getGenAiDoSample(),
					ResponseFormat.TEXT.name(), RequestMetadataUtil.getSimilaritySearchMetadata());
			final BaseResponseModel responseObject = callGenAi(request, BaseResponseModel.class);
			String finalQuery = Assert.assertNotNull(responseObject).getModelResponse().trim();
			if ( ! finalQuery.isEmpty()) {
				for (final String text : SAVED_SEARCHES) {
					if (finalQuery.contains(text)) {
						finalQuery = text;
						final String usage = getUsage(query, finalQuery);
						return getSavedSearchResult(projectId, finalQuery, usage, httpRequest);
					}
				}
			}
		} catch (final UserFacingException e) {
			LOG.error("There is some issue with loading the prompt or calling GEN-AI: " + e.getMessage(), e);
		}
		return getSemanticSearchResultPojo(projectId, Optional.of(query));
	}

	private String getUsage(final String query, final String finalQuery) {
		    if (query.contains("module")  || MODULE_SAVED_SEARCHES.contains(finalQuery)) {
				return "modules";
			} else if (query.contains("annotation") || ANNOTATION_SAVED_SEARCHES.contains(finalQuery)) {
				return "annotations";
			} else if (query.contains("dictionary")  || DICTIONARY_SAVED_SEARCHES.contains(finalQuery)) {
				return "data-dictionary";
			} else {
				return "";
			}
	}


	private SemanticSearchResultPojo getSavedSearchResult(final EntityId projectId,
			final String name, final String usage,  HttpServletRequest  request) {
		String answer = "We have found a related saved search for your query: " + getUrl(request, projectId, name, usage);
		return  new SemanticSearchResultPojo(answer, null);

	}

	private String getUrl(HttpServletRequest request, final EntityId projectId, final  String finalQuery, final String usage) {
		return "<a href=\""
				+ request.getScheme() + "://" +
				request.getServerName() +
				":" + request.getServerPort() +
	                 "/#/project-" + projectId.getNid() + "/"+ usage +"?savedSearch=" + getQuery(finalQuery) +
				"\"/a>" + finalQuery + "</a>";
	}

	private String getQuery (final String finalQuery) {
		return  finalQuery.replaceAll("\\s+","%20");
	}

	private SemanticSearchResultPojo getSemanticSearchResultPojo(final EntityId projectId, final Optional<String> finalQuery) {
		final var requestBody = new SearchRagRequest(getConfigProperties().getGenAiPlugin(), getConfigProperties().getGenAiMaxNewToken(),
				getConfigProperties().getGenAiTemperature(), getConfigProperties().getGenAiDoSample(), finalQuery.orElse(null), getConfigProperties().getRetrieverTopK());
		try {
			final SemanticSearchResultPojo exchange = callGenAi(requestBody, SemanticSearchResultPojo.class);
			LOG.debug(() -> "Semantic search response: " + exchange);
			addModuleIdsToResult(projectId, exchange);
			return exchange;
		} catch (final Exception e) {
			LOG.error("An error occurred while performing semantic search request: " + e.getMessage(), e);
			throw new UserFacingException("An error occurred while performing the semantic search request. Please contact your administrator.");
		}
	}

	private void addModuleIdsToResult(final EntityId projectId, final SemanticSearchResultPojo result) {
		final var documents = result.getDocuments();
		if (documents == null) {
			return;
		}
		for (final SemanticSearchDocumentPojo document : documents) {
			final var meta = document.getMeta();
			final String name = document.getMeta().get("name");
			if (name == null) {
				continue;
			}
			final var index = name.indexOf('#');
			var moduleFileName = name;
			String label = null;
			if (index > -1) {
				moduleFileName = name.substring(0, index);
				label = name.substring(index + 1);
			}
			final var moduleName = removeFileEnding(moduleFileName);
			/* This assumes there is only one module with the specific name. It would be better to have the module ID or full path in the Opensearch DB
			 * (Will be handled in WMIN-12187) */
			final List<ModulePojo> modules = getModuleService().findModules(q -> q.ofProject(projectId).withName(moduleName).includeContent(true));
			if ( ! modules.isEmpty()) {
				final var module = obtainMatchingModule(modules, moduleFileName);
				meta.put("moduleId", module.getId().toString());
				addOffset(module, label, meta);
			}
		}
	}

	private ModulePojo obtainMatchingModule(final List<ModulePojo> modules, final String moduleFileName) {
		if (modules.size() == 1) {
			return modules.get(0);
		}
		for (final ModulePojo module : modules) {
			final Optional<String> path = module.getPath();
			if (path.isPresent() && path.get().endsWith(moduleFileName)) {
				return module;
			}
		}
		return modules.get(0);
	}

	private String removeFileEnding(final String fileName) {
		final int index = fileName.lastIndexOf('.');
		if (index > -1) {
			return fileName.substring(0, index);
		} else {
			return fileName;
		}
	}

	private void addOffset(final ModulePojo module, @Nullable final String label, final Map<String, String> meta) {
		if (label == null || module.getTechnology() != Technology.COBOL) {
			return;
		}
		final Integer offset = findOffset(module, label);
		if (offset > -1) {
			meta.put("offset", offset.toString());
		}
	}

	private Integer findOffset(final ModulePojo module, final String label) {
		/* This is a hacky way of obtaining the offset which only works for COBOL and assumes the label name is unique.
		 * It would be better to have the offset in the Opensearch DB. */
		return module.getContent()
				.map(s -> s.indexOf("       " + label + "."))
				.orElse(-1);
	}

}
