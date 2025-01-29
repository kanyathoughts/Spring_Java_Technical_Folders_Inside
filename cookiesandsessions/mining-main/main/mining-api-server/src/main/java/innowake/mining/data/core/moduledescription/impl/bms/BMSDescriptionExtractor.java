/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.bms;

import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.moduledescription.api.AllMatcher;
import innowake.mining.data.core.moduledescription.impl.DescriptionExtractor;
import innowake.mining.data.core.moduledescription.impl.RuleSet;
import innowake.ndt.core.parsing.ILexer;
import innowake.ndt.core.parsing.LexerConfiguration;
import innowake.ndt.core.parsing.bms.BmsLexerFactory;

/**
 * Extractor for descriptions of BMS maps.
 */
public class BMSDescriptionExtractor extends DescriptionExtractor {
	public static final String LINE_BREAK = "\r\n";
	
	/**
	 * Using this extractor you get the map as a text version.
	 */
	public BMSDescriptionExtractor() {
		super(RuleSet.create(AllMatcher.INSTANCE, BMSMapToTextExtractor.INSTANCE, lines -> lines.collect(Collectors.joining(LINE_BREAK))));
	}
	
	@Override
	@Nullable
	protected ILexer createLexer() {
		return BmsLexerFactory.get().createLexer(LexerConfiguration.DEFAULT);
	}

}
