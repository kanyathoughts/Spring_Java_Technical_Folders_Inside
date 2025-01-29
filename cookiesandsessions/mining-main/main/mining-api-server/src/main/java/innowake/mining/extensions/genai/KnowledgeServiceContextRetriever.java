/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.genai;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.genai.AiGenerationContext;
import innowake.mining.server.genai.AnnotationContext;
import innowake.mining.server.genai.GenAiAnnotationContextRetriever;
import innowake.mining.server.genai.knowledgeservice.KnowledgeQuery;
import innowake.mining.server.genai.knowledgeservice.KnowledgeServiceResultObject;
import innowake.mining.server.genai.requestresponse.KnowledgeServiceResponseModel;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.util.BranchStatementUtility;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.model.ModuleLocation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * Extension for retrieving context from the GenAI Knowledge Service.
 */
@Service
@Order(Ordered.HIGHEST_PRECEDENCE + 1)
@ConditionalOnExpression("!T(org.springframework.util.StringUtils).isEmpty('${mining.genAI.knowledgeService.url:}')")
public class KnowledgeServiceContextRetriever implements GenAiAnnotationContextRetriever {

	private static final Logger LOG = LoggerFactory.getLogger(KnowledgeServiceContextRetriever.class);

	@Autowired
	private transient KnowledgeQuery knowledgeQuery;

	@Autowired
	private transient BranchStatementUtility branchStatementUtility;

	@Autowired
	private transient DataDictionaryService dataDictionaryService;

	@Autowired
	private GenericConfigProperties configProperties;

	@Override
	public AiGenerationContext retrieve(final AnnotationContext context, final UUID requestUUID) {
		final AiGenerationContext generationContext = context.getGenerationContext();
		LOG.debug(() -> "Previous context: " + generationContext.getAdditionalPromptContext());

		final List<DataDictionaryPojo> applicableFields;
		try {
			applicableFields = getApplicableFields(context);
			LOG.debug(() -> "Identified applicable fields: " + applicableFields + " for\n" + context.getAnnotation().sourceAttachment.getNonNull());
		} catch (final Exception e) {
			// We don't want to fail the whole context retrieval if the data fields cannot be retrieved.
			LOG.warn(() -> "Error while retrieving data fields for Module " + context.getAnnotation().module.getNonNull() + ": " + e.getMessage(), e);
			return generationContext;
		}

		try {
			final KnowledgeServiceResponseModel knowledgeServiceResponseModel = knowledgeQuery.queryDocuments(configProperties.getGenAiAnnotationKnowledgeCategory(),
					getFieldsString(applicableFields), context.getAnnotation(), applicableFields.size(), requestUUID);
			final StringBuilder knowledgeServiceResult = new StringBuilder();
			final List<KnowledgeServiceResultObject> knowledgeServiceResultObjects = knowledgeServiceResponseModel.getResult();
			knowledgeServiceResultObjects.forEach(result -> knowledgeServiceResult.append(result.getContent()).append("\n"));
			LOG.debug("Adding ranked context retrieved from Knowledge Service: " + knowledgeServiceResult);
			return new AiGenerationContext(generationContext.getAdditionalPromptContext() + "\n" + knowledgeServiceResult + "\n");
		} catch (final Exception e) {
			LOG.error(() -> "Unable to retrieve context from Knowledge Service:", e);
			throw e;
		}
	}

	private List<DataDictionaryPojo> getApplicableFields(final AnnotationContext context) {
		final EntityId moduleId = context.getAnnotation().module.getNonNull();
		final Integer offset = context.getAnnotation().location.getNonNull().getOffset();
		final Integer length = context.getAnnotation().location.getNonNull().getLength();
		final List<Long> ddIds = new ArrayList<>();

		branchStatementUtility.getReferencedDDEntries(moduleId, new ModuleLocation(offset, length)).forEach(dd -> ddIds.add(dd.getId()));
		return dataDictionaryService.find(q -> q.byNids(ddIds));
	}

	private String getFieldsString(final List<DataDictionaryPojo> ddEntries) {
		final StringBuilder fields = new StringBuilder();
		ddEntries.forEach(dde -> fields.append(dde.getName()).append("\n"));
		return fields.toString();
	}

}
