/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.api;

import java.util.ArrayList;
import java.util.stream.Stream;

/**
 * No-op implementation of an {@link Extractor}.
 */
public class NothingExtractor implements Extractor {

	/**
	 * The {@link NothingExtractor} instance.
	 */
	public static final Extractor INSTANCE = new NothingExtractor();
	
	private NothingExtractor() {}
	
	@Override
	public Stream<String> extract(final String content) {
		return new ArrayList<String>().stream();
	}

}
