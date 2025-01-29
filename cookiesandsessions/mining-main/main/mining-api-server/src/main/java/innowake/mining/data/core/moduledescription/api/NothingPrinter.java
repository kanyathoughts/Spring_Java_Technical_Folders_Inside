/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.api;

import java.util.stream.Stream;

/**
 * No-op implementation of the {@link PrettyPrinter}.
 */
public class NothingPrinter implements PrettyPrinter {

	/**
	 * The {@link NothingPrinter} instance.
	 */
	public static final PrettyPrinter INSTANCE = new NothingPrinter();
	
	private NothingPrinter() {}
	
	@Override
	public String print(final Stream<String> lines) {
		return "";
	}

}
