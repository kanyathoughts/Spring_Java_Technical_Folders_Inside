/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import java.util.List;
import java.util.Optional;

import org.apache.commons.io.FilenameUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.parser.natural.DataArea;
import innowake.mining.server.discovery.parser.natural.DataField;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Resolves initial and constant values of fields in data definition.
 */
class FieldValueResolver {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);

	private Optional<DataArea> dataArea = Optional.empty();
	private final SourcePojo sourceObject;
	private final NaturalParseResultProvider naturalParserResultProvider;

	public FieldValueResolver(final SourcePojo sourceObject, final NaturalParseResultProvider naturalParserResultProvider) {
		this.sourceObject = sourceObject;
		this.naturalParserResultProvider = naturalParserResultProvider;
	}

	/**
	 * Tries to resolve a name of a variable to a constant or initial value of a variable known to this {@link FieldValueResolver}.
	 *
	 * @param variableName a variable name
	 * @return a constant or initial value of a variable known to this {@link FieldValueResolver} matching the given variable name or it returns null
	 * if a constant or initial value cannot be found
	 */
	@Nullable
	String resolve(final String variableName) {
		String result = null;

		/* resolve(String) might get called or not which is why we resolve the dataArea here */
		Optional<DataArea> dataArea = this.dataArea;
		if ( ! dataArea.isPresent()) {
			dataArea = getDataArea(sourceObject, naturalParserResultProvider);
			this.dataArea = dataArea;
		}

		if (dataArea.isPresent()) {
			final List<DataField> fields = dataArea.get().resolve(variableName);
			if ( ! fields.isEmpty()) {
				if (fields.size() == 1) {
					final Optional<String> constValue = fields.get(0).getConstValue();
					if (constValue.isPresent()) {
						result = constValue.get();
					}
					final Optional<String> initValue = fields.get(0).getInitialValue();
					if (initValue.isPresent()) {
						result = initValue.get();
					}
				} else {
					LOG.error(() -> String.format("%s: Multiple data definitions found for variable %s", sourceObject.getPath(), variableName));
				}
			}
		}

		return result;
	}

	private static Optional<DataArea> getDataArea(final SourcePojo sourceObject, final NaturalParseResultProvider naturalParserResultProvider) {
		Optional<DataArea> result;
		try {
			result = naturalParserResultProvider.getParseResult(sourceObject).getDataArea();
			if ( ! result.isPresent()) {
				LOG.debug(() -> String.format("%s: No data definition found for %s", sourceObject.getPath(), FilenameUtils.getBaseName(sourceObject.getPath())));
			}
		} catch (final Exception exception) {
			result = Optional.empty();
			LOG.error(() -> String.format("%s: Error while parsing data definition", FilenameUtils.getBaseName(sourceObject.getPath())), exception);
		}

		return result;
	}
}
