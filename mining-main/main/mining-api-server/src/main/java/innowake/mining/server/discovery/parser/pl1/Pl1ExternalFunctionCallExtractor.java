/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.pl1;

import static innowake.ndt.parsing.base.pattern.Pattern.any;
import static innowake.ndt.parsing.base.pattern.Pattern.is;
import static innowake.ndt.parsing.base.pattern.Pattern.not;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleType;
import innowake.ndt.core.parsing.IToken;
import innowake.ndt.core.parsing.base.BaseParserConfiguration;
import innowake.ndt.parsing.base.pattern.Pattern;
import innowake.ndt.parsing.base.pattern.PatternMatcher;
import innowake.ndt.parsing.base.pattern.StrictPatternMatcher;
import innowake.ndt.parsing.base.producer.NodeProducer;
import innowake.ndt.parsing.base.producer.NodeProducerResult;
import innowake.ndt.parsing.base.token.ast.TokenBasedAstNode;
import innowake.ndt.parsing.parser.base.Checkpoint;
import innowake.ndt.parsing.parser.base.TokenSequence;
import innowake.ndt.parsing.parser.pl1.Pl1LightWeightParser;
import innowake.ndt.parsing.parser.pl1.producer.AbstractPl1NodeProducer;

/**
 * Lightweight parser for PL1 that extracts all given external function calls.
 */
public class Pl1ExternalFunctionCallExtractor extends Pl1LightWeightParser<SourcePojo> {
	
	private final List<NodeProducer<TokenBasedAstNode>> producers = new ArrayList<>();
	
	/**
	 * Constructor.
	 * 
	 * @param parserConfiguration the parse configuration
	 * @param externalEntries the registered external entries that can be called
	 */
	public Pl1ExternalFunctionCallExtractor(final BaseParserConfiguration<SourcePojo> parserConfiguration, final Map<String, ModuleType> externalEntries) {
		super(parserConfiguration);
		producers.add(new FunctionCallProducer(Pattern.of(not(is("call")), is(externalEntries.keySet().toArray(new String[0])), is("("))));
		producers.add(new ExternalGenericProducer(externalEntries));
	}

	@Override
	protected List<IToken> advanceToSafePoint(final TokenSequence tokenSequence) {
		final List<IToken> tokens = new ArrayList<>();
		tokens.add(tokenSequence.get());
		tokenSequence.advance();
		return tokens;
	}
	
	@Override
	protected List<NodeProducer<TokenBasedAstNode>> getProducers() {
		return producers;
	}
	
	private static class FunctionCallProducer extends AbstractPl1NodeProducer {

		private final StrictPatternMatcher patternMatcher;
		
		public FunctionCallProducer(final Pattern pattern) {
			this.patternMatcher = new StrictPatternMatcher(pattern);
		}

		@Override
		public PatternMatcher getPattern() {
			return patternMatcher;
		}
		
		@Override
		public void advanceToNextStatement(final TokenSequence sequence) {
			/* don't advance, there might be nested function calls  */
		}

		@Override
		public NodeProducerResult<TokenBasedAstNode> consume(final TokenSequence tokens) {
			tokens.advance(); /* skip the "not call" token */
			final Pl1ExternalCallNode resultNode = new Pl1ExternalCallNode() {};
			final Checkpoint start = tokens.checkpoint();
			resultNode.externalCalls.put(tokens.get().getText().toString(), ModuleType.PL1_FUNCTION);
			tokens.advance();
			resultNode.addToken(tokens.allTokensSince(start));
			return NodeProducerResult.withoutTerminator(resultNode);
		}
		
	}
	
	private static class ExternalGenericProducer extends AbstractPl1NodeProducer {

		private static final StrictPatternMatcher patternMatcher = new StrictPatternMatcher(Pattern.of(any(), is("generic"), is("(")));
		
		private final Map<String, ModuleType> externalEntries;
		
		public ExternalGenericProducer(final Map<String, ModuleType> externalEntries) {
			this.externalEntries = externalEntries;
		}
		
		@Override
		public PatternMatcher getPattern() {
			return patternMatcher;
		}

		@Override
		public NodeProducerResult<TokenBasedAstNode> consume(final TokenSequence tokens) {
			final Checkpoint start = tokens.checkpoint();
			final Pl1ExternalCallNode resultNode = new Pl1ExternalCallNode() {};
			tokens.advance();
			tokens.advance();
			tokens.advance();
			while ( ! tokens.is(")") && ! tokens.is(";")) {
				if (tokens.is(",")) {
					tokens.advance();
				}
				final String entry = tokens.get().getText().toString();
				final var externalCallType = externalEntries.get(entry);
				if (externalCallType != null) {
					resultNode.externalCalls.put(entry, externalCallType);
				}
				tokens.advance(); /* entry name */
				tokens.advance(); /* when / otherwise */
				consumeParentheses(tokens);
			}
			tokens.advance();
			resultNode.addToken(tokens.allTokensSince(start));
			return NodeProducerResult.withoutTerminator(resultNode);
		}
	}
	
	public static class Pl1ExternalCallNode extends TokenBasedAstNode {
		private final Map<String, ModuleType> externalCalls = new HashMap<>();

		public Map<String, ModuleType> getExternalCalls() {
			return externalCalls;
		}
	}
}
