/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.cobol;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import innowake.ndt.core.parsing.IToken;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.cobol.CobolLexerFactory;
import innowake.ndt.core.parsing.cobol.CobolRegionCategory;
import innowake.ndt.core.parsing.spi.StringContent;

/**
 * Class to check if string content is of type Cobol based on tokens.
 * Tokens are split by a single whitespace & line endings.
 */
public class CobolTokenCounter {

	private final List<String> tokens;
	private final int tokenCount;

	private int copyTokenCount;
	private int bmsTokenCount;
	private int fmsTokenCount;
	
	public CobolTokenCounter(final String content) {
		tokens = Arrays.asList(TokenPartitioner2.create(CobolLexerFactory.get())
				.doPartitioning(new StringContent(content))
				.getTokens(CobolRegionCategory.CATEGORY_CODE))
				.stream()
				.map(IToken::toString)
				.collect(Collectors.toList());
		
		tokenCount = tokens.size();
		copyTokenCount = 0;
		bmsTokenCount = 0;
		fmsTokenCount = 0;
	}
	
	public CobolTokenCounter count() {
		for(final String token : tokens) {
			final Optional<CobolIdentificationToken> t = CobolIdentificationToken.findEnumOf(token);
			if(t.isPresent()) {
				switch (t.get()) {
					/* copy tokens*/
					case PIC9:
					case PICTURE:
					case PIC:
					case X_LeftBracket:
						copyTokenCount++;
						break;
					/* map tokens*/
					/* BMS */
					case DFHMSD:
					case DFHMDI:
					case DFHMDF:
						bmsTokenCount++;
					break;
					/* FMS */
					case MSGEND:
					case FMTEND:
						fmsTokenCount++;
					break;
					default:
				}
			}
		}
		return this;
	}
	
	public int getTokenCount() {
		return tokenCount;
	}

	public int getCopyTokenCount() {
		return copyTokenCount;
	}

	public int getBmsTokenCount() {
		return bmsTokenCount;
	}

	public int getFmsTokenCount() {
		return fmsTokenCount;
	}
}
