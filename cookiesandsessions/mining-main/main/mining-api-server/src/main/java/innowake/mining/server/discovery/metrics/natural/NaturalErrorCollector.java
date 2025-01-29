/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import innowake.lib.parsing.error.ResourceError;
import innowake.lib.parsing.util.visitor.VisitableCollection;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.shared.lang.Functions;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import innowake.ndt.naturalparser.INaturalModel;
import innowake.ndt.naturalparser.INaturalModel.ProgramState;

/**
 * Utility methods to collect recoverable and unrecoverable parser errors for Natural objects.
 */
class NaturalErrorCollector {

	private static final String PARSE_ERRORS_SUMMARY = "%d parse errors were found in source file.";
	private static final String UNRECOVERABLE_PARSE_ERROR = "Unrecoverable parse error.";

	private NaturalErrorCollector() {
		throw new IllegalStateException();
	}

	/**
	 * Checks if any parse errors are present in the given {@code model} returns them as a list of {@link ErrorMarker ModelErrors}.
	 * <p>If no parse error were created then an empty list is returned.</p>
	 *
	 * @param model The {@link INaturalModel} containing the parse results
	 * @return list of {@link ErrorMarker}
	 */
	static List<ErrorMarker> collectErrors(final INaturalModel model) {
		final List<ErrorMarker> result = new ArrayList<>();
		final ProgramState state = model.getProgramState();

		if (state == ProgramState.UNRECOVERABLE_ERRORS) {
			result.add(createError(Severity.ERROR, UNRECOVERABLE_PARSE_ERROR));
		} else if (state == ProgramState.RECOVERABLE_ERRORS) {
			Functions.filter(model.getProgramObject().getErrors(), ResourceError.class)
				.stream()
				.map(error -> createError(Severity.ERROR, error.toString()))
				.forEach(result::add);
		}

		return result;
	}
	
	/**
	 * Checks if any parse errors are present in the given {@code model} and returns a {@link ErrorMarker} with a summary message if so.
	 * <p>If no parse error were created then an empty {@link Optional} is returned.</p>
	 *
	 * @param model The {@link INaturalModel} containing the parse results
	 * @return {@link Optional} containing a summary {@link ErrorMarker} or empty
	 */
	static Optional<ErrorMarker> getParserResult(final INaturalModel model) {
		final ProgramState state = model.getProgramState();
		if (state == ProgramState.UNRECOVERABLE_ERRORS) {
			return Optional.of(createError(Severity.ERROR, UNRECOVERABLE_PARSE_ERROR));
		}

		if (state == ProgramState.RECOVERABLE_ERRORS) {
			final VisitableCollection errors = model.getProgramObject().getErrors();
			final int size = errors == null ? 0 : errors.getChildCount();
			return Optional.of(createError(Severity.ERROR, String.format(PARSE_ERRORS_SUMMARY, Integer.valueOf(size))));
		}

		return Optional.empty();
	}

	private static ErrorMarker createError(final Severity severity, final String message) {
		return new ErrorMarker()
						.setSeverity(severity)
						.setKey(ErrorKey.PARSE_ERROR)
						.setCause(message)
						.validate();
	}

}
