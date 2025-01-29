/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.plugin.lucene;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.LowerCaseFilter;
import org.apache.lucene.analysis.core.KeywordTokenizer;

/**
 * This custom Analyzer is used in the Module_name_ft full text index
 * in order to allow searching for Module names ignoring case and treating the name as a single keyword.
 */
public class LowercasingKeywordAnalyzer extends Analyzer {

	@Override
	protected TokenStreamComponents createComponents(final String fieldName) {
		final KeywordTokenizer keywordTokenizer = new KeywordTokenizer();
		return new TokenStreamComponents(keywordTokenizer, new LowerCaseFilter(keywordTokenizer));
	}
}
