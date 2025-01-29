/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.batch;

import org.apache.commons.lang.exception.ExceptionUtils;

import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.util.filter.OrFilter;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.Logging;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.core.parsing.IAstInlineNode;
import innowake.ndt.core.parsing.ITokens;
import innowake.ndt.core.parsing.IncludingCategoryFilter;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.jcl.JclLexerFactory;
import innowake.ndt.core.parsing.jcl.JclRegionCategory;
import innowake.ndt.core.parsing.spi.IInlineParserProvider;
import innowake.ndt.core.parsing.spi.InlineStatus;
import innowake.ndt.core.parsing.spi.StringContent;
import innowake.ndt.core.parsing.spi.Tokens;

/**
 * Jcl Parser provider class.
 */
final class JclParserProvider implements IInlineParserProvider<SourcePojo> {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.JCL_PARSER);
	
	public static final TokenPartitioner2 PARTITIONER = TokenPartitioner2.createWithRegionFilter(
			JclLexerFactory.get(),
			new OrFilter<>(
					new IncludingCategoryFilter(JclRegionCategory.CATEGORY_STANDARD), 
					new IncludingCategoryFilter(JclRegionCategory.CATEGORY_COMMENT),
					new IncludingCategoryFilter(JclRegionCategory.CATEGORY_STREAM)));
	
	@Override
	public ITokens createTokens(final SourcePojo sourceObject,final boolean isRoot) {
		try {
			final StringContent stringContent = new StringContent(sourceObject.getContent().toString());
			return PARTITIONER.doPartitioning(stringContent);
		} catch (final Exception e) {
			if (LOG.isErrorEnabled()) LOG.error("[{}] {}", sourceObject.getName(), e.getMessage(), e);
			if (LOG.isDebugEnabled()) LOG.debug(ExceptionUtils.getFullStackTrace(e)); 
			return new Tokens();
		}
	}

	@Override
	public Object getHashable(final SourcePojo sourceObject) {
		return sourceObject;
	}

	@Override
	@Nullable
	public SourcePojo resolveSource(final IAstInlineNode node, final InlineStatus status) {
		throw new UnsupportedOperationException("Resolving source is not supported.");
	}
}
