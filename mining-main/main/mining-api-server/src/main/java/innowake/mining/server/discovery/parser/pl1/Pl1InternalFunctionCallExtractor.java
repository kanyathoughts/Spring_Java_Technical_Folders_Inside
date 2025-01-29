/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.pl1;

import static innowake.ndt.parsing.base.pattern.Pattern.any;
import static innowake.ndt.parsing.base.pattern.Pattern.is;
import static innowake.ndt.parsing.base.pattern.Pattern.not;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import innowake.mining.shared.entities.SourcePojo;
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
import innowake.ndt.parsing.parser.pl1.producer.Pl1ProcedureProducer;

/**
 * Extracts all function calls together with the procedure structure.
 */
public class Pl1InternalFunctionCallExtractor extends Pl1LightWeightParser<SourcePojo> {

	private final List<NodeProducer<TokenBasedAstNode>> producers = new ArrayList<>();
	
	/**
	 * Constructor.
	 * 
	 * @param parserConfiguration the configuration
	 * @param procedureLabels potential names for functions
	 */
	public Pl1InternalFunctionCallExtractor(final BaseParserConfiguration<SourcePojo> parserConfiguration, final Set<String> procedureLabels) {
		super(parserConfiguration);
		producers.add(Pl1ProcedureProducer.INSTANCE);
		producers.add(new FunctionCallProducer(Pattern.of(not(is("call")), is(procedureLabels.toArray(new String[0])), is("("))));
		producers.add(new GenericProducer(procedureLabels));
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
			final Pl1InternalCallNode resultNode = new Pl1InternalCallNode() {};
			final Checkpoint start = tokens.checkpoint();
			resultNode.internalCalls.add(tokens.get().getText().toString());
			tokens.advance();
			resultNode.addToken(tokens.allTokensSince(start));
			return NodeProducerResult.withoutTerminator(resultNode);
		}
		
	}
	
	private static class GenericProducer extends AbstractPl1NodeProducer {

		private static final StrictPatternMatcher patternMatcher = new StrictPatternMatcher(Pattern.of(any(), is("generic"), is("(")));
		
		private final Set<String> procedureLabels;
		
		public GenericProducer(final Set<String> procedureLabels) {
			this.procedureLabels = procedureLabels;
		}
		
		@Override
		public PatternMatcher getPattern() {
			return patternMatcher;
		}

		@Override
		public NodeProducerResult<TokenBasedAstNode> consume(final TokenSequence tokens) {
			final Checkpoint start = tokens.checkpoint();
			final Pl1InternalCallNode resultNode = new Pl1InternalCallNode() {};
			tokens.advance();
			tokens.advance();
			tokens.advance();
			while ( ! tokens.is(")") && ! tokens.is(";")) {
				if (tokens.is(",")) {
					tokens.advance();
				}
				final String entry = tokens.get().getText().toString();
				if (procedureLabels.contains(entry)) {
					resultNode.internalCalls.add(entry);
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
	
	/**
	 * Represents an internal function call.
	 */
	public static class Pl1InternalCallNode extends TokenBasedAstNode {
		private final Set<String> internalCalls = new HashSet<>();

		/**
		 * Returns the call targets of this node.
		 *
		 * @return the call targets of this node.
		 */
		public Set<String> getInternalCalls() {
			return internalCalls;
		}
	}
}
