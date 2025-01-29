/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.csv;

import com.opencsv.CSVWriter;
import com.opencsv.ICSVWriter;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.extensions.export.callchain.model.CallChain;

import java.io.IOException;
import java.io.Writer;
import java.util.List;

/**
 * {@link CSVWriter} implementation for writing {@link CallChain Callchains}.
 */
public class CallChainCSVWriter extends CSVWriter {

	private final StringBuilder strb = new StringBuilder(1024);

	public CallChainCSVWriter(final Writer writer) {
		super(writer);
	}

	@Override
	protected boolean stringContainsSpecialCharacters(@Nullable final String line) {
		if (line != null) {
			for (int i = 0; i < line.length(); i++) {
				switch (line.charAt(i)) {
					case DEFAULT_QUOTE_CHARACTER:
						/*	case DEFAULT_ESCAPE_CHARACTER: for us it is the same as the DEFAULT_QUOTE_CHARACTER */
					case DEFAULT_SEPARATOR:
					case '\n':
					case '\r':
						return true;
					default:
						continue;
				}
			}
		}

		return false;
	}

	void writeCallChain(final CallChain callChain) {
		try {
			strb.setLength(0);
			final List<CallChain.CallChainEntry> callChainEntries = callChain.getCallChainEntries();
			for (int i = 0; i < callChainEntries.size(); i++) {
				final CallChain.CallChainEntry entry = callChainEntries.get(i);

				if (i != 0) {
					strb.append(ICSVWriter.DEFAULT_SEPARATOR);
				}
				strb.append(ICSVWriter.DEFAULT_QUOTE_CHARACTER);

				if (stringContainsSpecialCharacters(entry.getModule().getName())) {
					processLine(entry.getModule().getName(), strb);
				} else {
					strb.append(entry.getModule().getName());
				}

				strb.append('(')
						.append(entry.getModule().getTechnology().toString())
						.append(':')
						.append(entry.getModule().getType().toString())
						.append(")[")
						.append(entry.getCallType());

				final String accessType = entry.getAccessType();
				if (accessType != null) {
					strb.append(':');
					if (stringContainsSpecialCharacters(accessType)) {
						processLine(accessType, strb);
					} else {
						strb.append(accessType);
					}
				}

				strb.append(']');
				strb.append(ICSVWriter.DEFAULT_QUOTE_CHARACTER);
			}
			strb.append(lineEnd);
			writer.write(strb.toString());
		} catch (final IOException e) {
			exception = e;
		}
	}
}
