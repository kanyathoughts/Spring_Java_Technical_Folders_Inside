/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.JclAstNodeType;
import innowake.mining.data.core.NaturalAstNodeType;
import innowake.ndt.cobol.parser.ast.model.CobolNode;
import innowake.ndt.cobol.parser.ast.statement.CobolEvaluateCondition;
import innowake.ndt.cobol.parser.ast.statement.CobolEvaluateStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolIfStmt;
import innowake.ndt.core.cobol.CobolArea;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.model.cfg.CfgNodeLabelProvider;
import innowake.ndt.core.parsing.ast.model.cfg.CfgOffsetBasedNodeLabelProvider;
import innowake.ndt.core.parsing.spi.OneBasedDocument;
import innowake.ndt.jcl.parser.model.ast.JclCondition;
import innowake.ndt.jcl.parser.model.ast.JclJobNode;
import innowake.ndt.jcl.parser.model.ast.JclStepExec;
import innowake.ndt.jcl.parser.model.ast.JclStepIf;
import innowake.ndt.naturalparser.ast.RepeatCondition;
import innowake.ndt.naturalparser.ast.Statement.RepeatStmt;

/**
 * Creates labels for AST nodes based on the source code of the underlying module.
 */
public class LabelCreator {

	private static final String REPEAT = "REPEAT";
	private static final Logger LOG = LoggerFactory.getLogger(LabelCreator.class);

	private final String source;

	/**
	 * Creates a new label creator based on the given source code.
	 * 
	 * @param source the source from which the label will be retrieved
	 */
	public LabelCreator(final String source) {
		this.source = source;
	}

	/**
	 * Returns the label of the given node.
	 *
	 * @param node the node for which the label should be retrieved
	 * @return the label for the node or an empty string if no label can be determined, 
	 *         this can happen for example if the offset information of the node is not valid
	 */
	public String getLabel(final AstNode node) {
		if (node instanceof CobolIfStmt) {
			return normalizeCode("IF " + ((CobolIfStmt) node).getCondition().toString());
		}

		if (node instanceof CfgOffsetBasedNodeLabelProvider) {
			if (NaturalAstNodeType.REPEAT.equals(node.getAstNodeName())) {
				/* Requires extra handling as UNTIL and WHILE condition can be at the end or beginning of the REPEAT statement */
				final RepeatCondition rc = Assert.assertNotNull(node.getAdapter(RepeatStmt.class)).getRepeatCondition();
				if (rc.isNoCondition()) {
					return REPEAT;
				}

				/* ("REPEAT UNTIL" or "REPEAT WHILE" + code from offsets of repeat condition */
				return normalizeCode(REPEAT + (rc.isUntilAtBegin() || rc.isUntilAtEnd() ? " UNTIL " : " WHILE ")
									 + source.substring(((CfgOffsetBasedNodeLabelProvider) node).getLabelStartOffset(),
														((CfgOffsetBasedNodeLabelProvider) node).getLabelEndOffset() + 1));
			}

			if (((CfgOffsetBasedNodeLabelProvider) node).hasLabelLocationInfo()) {
				return normalizeCode(source.substring(((CfgOffsetBasedNodeLabelProvider) node).getLabelStartOffset(),
													  ((CfgOffsetBasedNodeLabelProvider) node).getLabelEndOffset() + 1));
			}

			/* "EVALUATE TRUE" is handled totally different by CobolParser without any offsets for the TRUE expression */
			if (node instanceof CobolEvaluateStmt) {
				final List<CobolEvaluateCondition> evaluations = ((CobolEvaluateStmt) node).getEvaluations();
				return evaluations.get(evaluations.size() - 1).isTrueEvaluation() ? "EVALUATE TRUE" : "EVALUATE";
			}
		}
		
		if (locationInformationIsValid(node)) {
			if (node instanceof CobolNode) {
				return cobolNodeLabel((CobolNode) node);
			}
			return normalizeCode(source.substring(node.getStartOffset(), node.getEndOffset() + 1));
		}
		if (node instanceof CfgNodeLabelProvider) {
			if (JclAstNodeType.STEP_IF.equals(node.getAstNodeName())) {
				return normalizeCode(((JclStepIf) node).getLabel());
			}
			if (JclAstNodeType.JOB.equals(node.getAstNodeName())) {
				return normalizeCode(((JclJobNode) node).getLabel());
			}
			if (JclAstNodeType.STEP_EXEC.equals(node.getAstNodeName())) {
				return normalizeCode(((JclStepExec) node).getLabel());
			}
			if (JclAstNodeType.CONDITION.equals(node.getAstNodeName())) {
				return normalizeCode(((JclCondition) node).getLabel());
			}
		}
		return StringUtils.EMPTY;
	}

	/** 
	 * Returns the label of the given Cobol node
	 * @param node the cobolNode for which the label should be retrieved
	 * @return return area B if it is multi-line statement otherwise return raw node source  
	 */
	private String cobolNodeLabel(final CobolNode node) {
		final String rawNodeSource = source.substring(node.getStartOffset(), node.getEndOffset() + 1);
		final OneBasedDocument document = new OneBasedDocument(rawNodeSource);
		if (document.numberOfLines() == 1) {
			/* No multi-line statement, so we don't have to do anything special here */
			return normalizeCode(rawNodeSource);
		}

		final List<String> lines = new ArrayList<>(Arrays.asList(document.lines()));
		/* The first line is already starting in Area B, so we add it without any manipulation */
		final String sourceOnlyInAreaB = lines.remove(0) + " " +
				lines.stream().map(CobolArea.B::substring).collect(Collectors.joining(" "));
		return normalizeCode(sourceOnlyInAreaB);
	}
	
	/**
	 * Determines the validity of the offset information of the given node.
	 * <p>
	 * Checks include:
	 * <ul>
	 *  <li>offset positive
	 *  <li>length of the given offsets positive
	 *  <li>tokens in given nodes are not artificial
	 *
	 * @param node the node for which the offset information should be checked
	 * @return {@code true} if the offsets are valid; {@code false} otherwise
	 */
	private boolean locationInformationIsValid(final AstNode node) {
		try {
			final int startOffset = node.getStartOffset();
			final int endOffset = node.getEndOffset() + 1;
			final int length = endOffset - startOffset;
			return startOffset >= 0 && endOffset >= 0 && length >= 0;
		} catch (final IllegalStateException e) {
			/* is actually an intended ArtificialTokenException from ndt-cobolparser
			 * we cannot find out if this exception occurs and avoid it */
			LOG.trace(e::getLocalizedMessage, e);
			return false;
		}
	}

	/**
	 * Returns the normalized {@code string} like {@code StringUtils.normalizeSpace(String)} except that text in single and double quotes stays as is.
	 *
	 * @param string the string to normalize; can be {@code null}
	 * @return the normalized string; not {@code null}
	 */
	private static String normalizeCode(@Nullable final String string) {
		if (string == null) {
			return  StringUtils.EMPTY;
		}

		final String stripped  = StringUtils.trim(string);
		if (stripped.isEmpty()) {
			return stripped;
		}

		final StringBuilder strb = new StringBuilder(stripped.length());

		boolean isQuote = false;
		boolean isDoubleQuote = false;
		for (int i = 0; i < stripped.length(); i++) {
			char c = stripped.charAt(i);
			switch (c) {
				case '\'':
					isQuote = ! isQuote;
					break;
				case '\"':
					isDoubleQuote = ! isDoubleQuote;
					break;
			}

			if (isQuote || isDoubleQuote || ! Character.isWhitespace(c)) {
				strb.append(c);
			} else if (i != 0 && ! Character.isWhitespace(stripped.charAt(i - 1))) {
				/* only add the whitespace if previous char is not already a whitespace */
				strb.append(' ');
			}
		}

		return strb.toString().trim();
	}
}
