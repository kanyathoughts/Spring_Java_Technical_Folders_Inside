/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.apache.commons.lang.StringUtils;

import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.parser.natural.AssemblingDataProvider;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.core.parsing.IToken;
import innowake.ndt.core.parsing.natural.NaturalRegionType;

/**
 * Parser for Natural dependencies which are not supported in natclipse dependency resolving.
 * E.g. CALL statements.
 */
class LightweightParser {

	private final SourcePojo natObject;
	private final FieldValueResolver fieldValueResolver;
	private final IToken[] tokens;
	private final SourceObjectResolver sourceObjectResolver;
	
	public LightweightParser(final SourcePojo natObject, final FieldValueResolver fieldValueResolver,
			final SourceObjectResolver sourceObjectResolver) throws DiscoveryException {
		this.natObject = natObject;
		this.fieldValueResolver = fieldValueResolver;
		this.sourceObjectResolver = sourceObjectResolver;
		tokens = tokenize();
	}

	Set<String> getOutgoingCalls() {
		final Set<String> result = new HashSet<>();
		final Iterator<IToken> iterator = Arrays.asList(tokens).iterator();
		while (iterator.hasNext()) {
			if (isCallStatement(iterator)) {
				final String callee = getCallee(iterator);
				result.add(unquoteIfLiteral(callee));
			}
		}
		return result;
	}
	
	private IToken[] tokenize() throws DiscoveryException {
		return new AssemblingDataProvider(sourceObjectResolver)
				.getParseResult(natObject)
				.getTokenPartitioning()
				.getTokens(NaturalRegionType.CATEGORY_CODE);
	}
	
	private boolean isCallStatement(final Iterator<IToken> iterator) {
		return iterator.next().getText().toString().equalsIgnoreCase("call") && iterator.hasNext();
	}
	
	private String getCallee(final Iterator<IToken> iterator) {
		return iterator.next().getText().toString();
	}
	
	private String unquoteIfLiteral(final String callee) {
		if (callee.startsWith("'") || callee.startsWith("\"")) {
			return callee.substring(1, callee.length() - 1);
		}
		final String resolve = fieldValueResolver.resolve(callee);
		if (resolve != null) {
			return resolve;
		}
		return StringUtils.EMPTY;
	}
	
}
