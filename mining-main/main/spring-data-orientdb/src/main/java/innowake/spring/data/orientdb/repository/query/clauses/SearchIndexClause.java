/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository.query.clauses;

import com.github.raymanrt.orientqb.query.Clause;
import innowake.mining.shared.lucene.LuceneUtil;

/**
 * Clause for search class that uses the Lucene full text index. See https://orientdb.com/docs/last/indexing/Full-Text-Index.html
 */
public class SearchIndexClause extends Clause {

	private static final String FORMAT = "SEARCH_INDEX(\"%s\", \"%s\") = true";

	private final String index;
	private final String searchTerm;

	public SearchIndexClause(final String index, final String searchTerm) {
		this.index = index;
		this.searchTerm = LuceneUtil.escapeSearchTerm(searchTerm.toLowerCase(), true);
	}

	@Override
	public String toString() {
		return String.format(FORMAT, index, searchTerm);
	}
}