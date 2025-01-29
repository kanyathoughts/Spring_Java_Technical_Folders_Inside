/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.api;

/**
 * This matcher returns the whole content.
 */
public class AllMatcher implements Matcher {

	/** The {@link AllMatcher} instance. */
	public static final Matcher INSTANCE = new AllMatcher();
	
	private AllMatcher() { }
	
	@Override
	public String match(final String content) {
		return content;
	}

}
