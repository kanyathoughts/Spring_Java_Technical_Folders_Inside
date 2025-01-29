/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.api;

/**
 * No-op implementation of the {@link Matcher}.
 */
public class NothingMatcher implements Matcher {

	/**
	 * The {@link NothingMatcher} instance.
	 */
	public static final Matcher INSTANCE = new NothingMatcher();
	
	private NothingMatcher() {}
	
	@Override
	public String match(final String content) {
		return "";
	}

}
