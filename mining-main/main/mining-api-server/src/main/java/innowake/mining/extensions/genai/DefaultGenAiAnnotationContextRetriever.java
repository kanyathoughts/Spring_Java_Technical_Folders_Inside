/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.genai;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.genai.AiGenerationContext;
import innowake.mining.server.genai.AnnotationContext;
import innowake.mining.server.genai.GenAiAnnotationContextRetriever;
import innowake.mining.server.util.BranchStatementUtility;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleLocation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;

@Service
@Order(Ordered.HIGHEST_PRECEDENCE)
public class DefaultGenAiAnnotationContextRetriever implements GenAiAnnotationContextRetriever {

	private static final Logger LOG = LoggerFactory.getLogger(DefaultGenAiAnnotationContextRetriever.class);

	@Autowired
	private transient BranchStatementUtility branchStatementUtility;

	@Autowired
	private transient DataDictionaryService dataDictionaryService;

	@Override
	public AiGenerationContext retrieve(final AnnotationContext context, final UUID requestUUID) {
		final AiGenerationContext generationContext = context.getGenerationContext();
		final Integer offset = context.getAnnotation().location.getNonNull().getOffset();
		final Integer length = context.getAnnotation().location.getNonNull().getLength();
		final EntityId moduleId = context.getAnnotation().module.getNonNull();
		LOG.debug(() -> "Previous context: " + generationContext.getAdditionalPromptContext());

		final StringBuilder ddContext = new StringBuilder();
		final Collection<Long> ddIds = new ArrayList<>();
		try {
			branchStatementUtility.getReferencedDDEntries(moduleId, new ModuleLocation(offset, length)).forEach(dd -> ddIds.add(dd.getId()));
			dataDictionaryService.find(q -> q.byNids(ddIds)).forEach(dde -> ddContext.append(dde.getName())
					.append(" = ")
					.append(dde.getDescription())
					.append(". ")
					.append(dde.getTranslatedFieldValue().orElse(""))
					.append("\n"));
			LOG.info("Adding Data Dictionary context context for Module " + moduleId + ": " + ddContext);
		} catch (final Exception e) {
			// We don't want to fail the whole context retrieval if the Data Dictionary context cannot be retrieved.
			// Not all technologies have Data Dictionary support.
			LOG.warn(() -> "Error while retrieving Data Dictionary context for Module " + moduleId + ": " + e.getMessage(), e);
		}

		if ( ! ddContext.isEmpty()) {
			// Add a newline before the Data Dictionary context if there is any.
			ddContext.insert(0, "\n");
		}

		return new AiGenerationContext(generationContext.getAdditionalPromptContext() + ddContext);
	}

}
