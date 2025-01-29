/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.data.core.annotation.impl;

import static innowake.mining.data.core.api.AstNodeUtils.ARITHMETIC_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.BRANCH;
import static innowake.mining.data.core.api.AstNodeUtils.BRANCH_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.DEFAULT_BRANCH;
import static innowake.mining.data.core.api.AstNodeUtils.ERROR_PROCESSING_STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.FIELD_REFERENCE;
import static innowake.mining.data.core.api.AstNodeUtils.STATEMENT;
import static innowake.mining.data.core.api.AstNodeUtils.hasAnySuperType;
import static innowake.mining.data.core.api.AstNodeUtils.hasAnyType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.ndt.cobol.parser.ast.model.CobolAtEndBlock;
import innowake.ndt.cobol.parser.ast.model.CobolExceptionStatement;
import innowake.ndt.cobol.parser.ast.model.CobolOverflowStatement;
import innowake.ndt.cobol.parser.ast.model.CobolParserError;
import innowake.ndt.cobol.parser.ast.model.CobolSizeErrorStatement;
import innowake.ndt.cobol.parser.ast.statement.CobolEvaluateStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolWhenStmt;

/**
 * This class implement AnnotationTranslator for translating Cobol Annotations.
 */
public class CobolAnnotationTranslator extends AnnotationTranslator {
	
	private static final Logger LOG = LoggerFactory.getLogger(CobolAnnotationTranslator.class);

	private static final String WHEN_STATEMENT = CobolWhenStmt.class.getSimpleName();
	/** key words for translation*/
	private static final String NEW_LINE = "\n";
	private static final String TAB = "\t";
	private static final String IF_KEY_WORD = "If";
	private static final String THEN_KEY_WORD = "Then";
	private static final String WHILE_KEY_WORD = "While";
	private static final String DO_KEY_WORD = "Do";
	private static final String MULTIPLE_CONDITION_MESSAGE = "Any of the following conditions are true:";
	private static final String OTHERWISE_KEY_WORD = "Otherwise";
	private static final String EQUAL_KEY_WORD = " Equal to ";

	/** key words for investigation*/
	private static final String WHEN_KEY_WORD = "WHEN";
	private static final String COMPUTE_KEY_WORD = "COMPUTE";
	private static final String OTHER_KEY_WORD = "OTHER";
	private static final String EVALUATE_KEY_WORD = "EVALUATE";
	private static final String PERFORM_KEY_WORD = "PERFORM";
	private static final String UNTIL_KEY_WORD = "UNTIL";
	private static final String GIVING_KEY_WORD = "GIVING";
	private static final String AND_KEY_WORD = "AND ";
	private static final String OR_KEY_WORD = "OR ";
	private static final String END_KEY_WORD = "END-";
	private static final String AND_WITH_SPACE_KEY_WORD = " AND ";

	private static final String EXCEPTION_CLASS = CobolExceptionStatement.class.getSimpleName();
	private static final String ERROR_CLASS = CobolSizeErrorStatement.class.getSimpleName();
	private static final String OVERFLOW_CLASS = CobolOverflowStatement.class.getSimpleName();
	private static final String ERROR_PARSER_CLASS = CobolParserError.class.getSimpleName();

	/** find all special super types in node */
	private static final AstNodeCollector FIELD_REFERENCE_COLLECTOR = new AstNodeCollector(node -> node.getSuperTypes().contains(FIELD_REFERENCE));
	private static final AstNodeCollector BRANCH_COLLECTOR = new AstNodeCollector(node -> hasAnySuperType(node, BRANCH, DEFAULT_BRANCH));
	private static final AstNodeCollector WHEN_COLLECTOR = new AstNodeCollector(node -> hasAnyType(node, WHEN_STATEMENT));

	private static final Map<String, String> TRANSLATOR = new HashMap<>();

	private final Map<String, Tuple2<AstNodePojo, DataDictionaryPojo>> astAndDdeMap;
	
	private static final List<String> END_SUFFIX = new ArrayList<>();

	static {
		TRANSLATOR.put(" > ", " Greater than ");
		TRANSLATOR.put(" >= ", " Greater than or equal to ");
		TRANSLATOR.put(" = ", EQUAL_KEY_WORD);
		TRANSLATOR.put(" < ", " Less than ");
		TRANSLATOR.put(" <= ", " Less than or equal to ");
		TRANSLATOR.put(" \\+ ", " Added to ");
		TRANSLATOR.put(" - ", " Subtracted from ");
		TRANSLATOR.put(" \\* ", " Multiplied by ");
		TRANSLATOR.put(" / ", " Divided by ");
		TRANSLATOR.put(" EQ ", EQUAL_KEY_WORD);
		TRANSLATOR.put(" & ", AND_WITH_SPACE_KEY_WORD);
		TRANSLATOR.put(" && ", AND_WITH_SPACE_KEY_WORD);
		TRANSLATOR.put("\\|", "OR");
		TRANSLATOR.put(" NOT ", " Not ");
		TRANSLATOR.put(" ADD ", " Add ");
		TRANSLATOR.put(" SUBTRACT ", " Subtract ");
		TRANSLATOR.put(" TO ", " to ");
		TRANSLATOR.put(" DIVIDE ", " Divide ");
		TRANSLATOR.put(" BY ", " by ");
		TRANSLATOR.put(GIVING_KEY_WORD, "to get");
		TRANSLATOR.put("DISPLAY", "Display");
		TRANSLATOR.put(COMPUTE_KEY_WORD, "Calculate");
		TRANSLATOR.put("EXEC SQL", "Execute SQL statement: ");
		TRANSLATOR.put("READ", "Read file");
		TRANSLATOR.put("WRITE", "Write to file");
		TRANSLATOR.put("MOVE", "move");
		TRANSLATOR.put("EXEC CICS", "Execute CICS:");
		TRANSLATOR.put("EXCEPTION", "Exception");
		TRANSLATOR.put("OVERFLOW", "Overflow");
		TRANSLATOR.put(PERFORM_KEY_WORD, DO_KEY_WORD);
		END_SUFFIX.add("END-IF");
		END_SUFFIX.add("END-EXEC");
		END_SUFFIX.add("END-CALL");
		END_SUFFIX.add("END-DELETE");
		END_SUFFIX.add("END-DIVIDE");
		END_SUFFIX.add("END-INSPECT");
		END_SUFFIX.add("END-MULTIPLY");
		END_SUFFIX.add("END-READ");
		END_SUFFIX.add("END-REPLACE");
		END_SUFFIX.add("END-RETURN");
		END_SUFFIX.add("END-SEARCH");
		END_SUFFIX.add("END-START");
		END_SUFFIX.add("END-STRING");
		END_SUFFIX.add("END-SUBTRACT");
		END_SUFFIX.add("END-UNSTRING");
		END_SUFFIX.add("END-WRITE");
		END_SUFFIX.add("END-ACCEPT");
		END_SUFFIX.add("END-CANCEL");
		END_SUFFIX.add("END-ENTRY");
		END_SUFFIX.add("END-ERROR");
		END_SUFFIX.add("END-REPROT");
		END_SUFFIX.add("END-EVALUATE");
		END_SUFFIX.add("END-PERFORM");
	}

	public CobolAnnotationTranslator(final Map<String, Tuple2<AstNodePojo, DataDictionaryPojo>> astAndDdeMap) {
		this.astAndDdeMap = astAndDdeMap;
	}

	/**
	 * translate the annotation to English
	 *
	 * @param offset the level of translation
	 * @param node the node related to the annotation
	 * @return English translation of the annotation
	 */
	@Override
	public String translate(final int offset, final AstNodePojo node) {
		final Set<String> superTypes = node.getSuperTypes();
		final String type = node.getType();
		if (superTypes.contains(BRANCH_STATEMENT)) {
			return translateBranchStatement(offset, node);
		} else if (AstNodeUtils.isCobolPerformStatementLoop(node)) {
			return translateLoopStatement(offset, node);
		} else if (superTypes.contains(STATEMENT)) {
			return translateStatement(offset, node);
		} else if (CobolAtEndBlock.class.getSimpleName().equals(type)) {
			return translateAtEndBlock(offset, node);
		}
		return StringUtils.EMPTY;
	}

	private String translateBranchStatement(final int offset, final AstNodePojo node) {
		final String type = node.getType();
		if (CobolEvaluateStmt.class.getSimpleName().equals(type)) {
			return translateEvaluateStatement(offset, node);
		} else {
			return translateOtherBranchStatement(offset, node);
		}
	}

	private String translateStatement(final int offset, final AstNodePojo node) {
		final Set<String> superTypes = node.getSuperTypes();
		if (superTypes.contains(ARITHMETIC_STATEMENT)) {
			return translateArithmaticStatement(offset, node);
		} else if (superTypes.contains(ERROR_PROCESSING_STATEMENT)) {
			return translateErrorStatement(offset, node);
		} else {
			return prettifyWithOffset(offset, node.getLabel(), node);
		}
	}

	private String translateErrorStatement(final int offset, final AstNodePojo node) {
		final StringBuilder translation = new StringBuilder();
		if (node.getType().contains(EXCEPTION_CLASS) || node.getType().contains(OVERFLOW_CLASS)) {
			translation.append(IF_KEY_WORD);
			translation.append(NEW_LINE).append(TAB);
			translation.append(node.getLabel().contains("NOT") ? "No" : StringUtils.EMPTY);
			translation.append((node.getType().contains(EXCEPTION_CLASS)) ? " Exception occured" : " Overflow occured");
			translation.append(NEW_LINE).append(THEN_KEY_WORD).append(NEW_LINE);
			node.getChildren().stream()
					.filter(Objects::nonNull)
					.map(statement -> translate(offset + 1, statement))
					.forEach(translation::append);
		} else if (node.getType().contains(ERROR_CLASS) || node.getType().contains(ERROR_PARSER_CLASS)) {
			translation.append(IF_KEY_WORD);
			translation.append(NEW_LINE).append(TAB);
			translation.append("Error occured").append(NEW_LINE);
			translation.append(THEN_KEY_WORD).append(NEW_LINE);
			node.getChildren().stream()
					.filter(Objects::nonNull)
					.map(statement -> translate(offset + 1, statement))
					.forEach(translation::append);
		} else {
			translation.append(node.getLabel());
		}
		return prettifyWithOffset(offset, translateVariables(node, translation.toString()), node);
	}

	private String translateArithmaticStatement(final int offset, final AstNodePojo node) {
		final AstNodePojo statement = node.getChildren().get(0);
		if (statement == null) {
			return StringUtils.EMPTY;
		}
		String translation = StringUtils.trimToEmpty(statement.getLabel());
		if ( ! (translation.contains(GIVING_KEY_WORD) || translation.contains(COMPUTE_KEY_WORD))) {
			final String storedIn = translation.substring(translation.lastIndexOf(' '));
			translation = translation + " to get " + storedIn;
		}
		translation = translateVariables(statement, translation).replace(",", AND_KEY_WORD);
		return prettifyWithOffset(offset, translation, node);
	}

	private String translateAtEndBlock(final int offset, final AstNodePojo node) {
		final StringBuilder translation = new StringBuilder().append(translateReadFileStatement(node.getLabel()));
		node.getChildren().stream()
				.filter(Objects::nonNull)
				.map(statement -> translate(offset + 1, statement))
				.forEach(translation::append);
		return prettifyWithOffset(offset, translateVariables(node, translation.toString()), node);
	}

	private String translateOtherBranchStatement(final int offset, final AstNodePojo node) {
		final StringBuilder translation = new StringBuilder().append(IF_KEY_WORD);
		translation.append(NEW_LINE);
		final List<AstNodePojo> conditions = AstNodeUtils.getBranchConditions(node);
		translateConditions(translation, conditions);
		translation.append(THEN_KEY_WORD).append(NEW_LINE);
		BRANCH_COLLECTOR.all(node).forEach(branch -> {
			if (branch.getSuperTypes().contains(DEFAULT_BRANCH)) {
				translation.append(OTHERWISE_KEY_WORD);
				translation.append(NEW_LINE);
			}
			branch.getChildren().stream()
					.filter(Objects::nonNull)
					.map(statement -> translate(offset + 1, statement))
					.forEach(translation::append);
		});
		return prettifyWithOffset(offset, translateVariables(node, translation.toString()), node);
	}

	private String translateEvaluateStatement(final int offset, final AstNodePojo node) {
		final StringBuilder translation = new StringBuilder();
		final String evaluate = node.getLabel().replace(EVALUATE_KEY_WORD, StringUtils.EMPTY).trim();
		boolean hasThen = true;
		for (final AstNodePojo branch : WHEN_COLLECTOR.all(node)) {
			translation.append(translateWhenNode(branch, hasThen, evaluate, offset));
			if (branch.getChildren().stream().noneMatch(statementNode -> hasAnySuperType(statementNode, STATEMENT))) {
				hasThen = false;
			} else {
				hasThen = true;
			}
		}
		return prettifyWithOffset(offset, translateVariables(node, translation.toString()), node);
	}

	private String translateWhenNode(final AstNodePojo branch, final boolean hasThen, final String evaluate, final int offset) {
		final StringBuilder translation = new StringBuilder();
		if (branch.getLabel().contains(OTHER_KEY_WORD)) {
			translation.append(OTHERWISE_KEY_WORD).append(NEW_LINE);
			branch.getChildren().stream()
					.filter(Objects::nonNull)
					.map(statement -> translate(offset + 1, statement))
					.forEach(translation::append);
		} else {
			if (hasThen) {
				translation.append(IF_KEY_WORD).append(NEW_LINE);
			} else {
				translation.append(NEW_LINE).append(OR_KEY_WORD);
			}
			/* In case the we Evaluate True, or Evaluate False the condition should be vice versa */
			if (evaluate.equalsIgnoreCase("TRUE") || evaluate.equalsIgnoreCase("FALSE")) {
				translation.append(TAB).append(branch.getLabel().replace(WHEN_KEY_WORD, StringUtils.EMPTY)).append(EQUAL_KEY_WORD).append(evaluate);
			} else {
				translation.append(TAB).append(evaluate).append(EQUAL_KEY_WORD).append(branch.getLabel().replace(WHEN_KEY_WORD, StringUtils.EMPTY));
			}
			if (branch.getChildren().stream().anyMatch(statementNode -> hasAnySuperType(statementNode, STATEMENT))) {
				translation.append(NEW_LINE).append(THEN_KEY_WORD).append(NEW_LINE);
				branch.getChildren().stream()
						.filter(Objects::nonNull)
						.map(statement -> translate(offset + 1, statement))
						.forEach(translation::append);
			}
		}
		return translation.toString();
	}

	private String translateLoopStatement(final int offset, final AstNodePojo node) {
		final StringBuilder translation = new StringBuilder();
		final List<AstNodePojo> conditions = AstNodeUtils.getLoopConditions(node);
		if ( ! conditions.isEmpty()) {
			translation.append(WHILE_KEY_WORD).append(NEW_LINE);
			translateConditions(translation, conditions);
		}
		translation.append(DO_KEY_WORD).append(NEW_LINE);
		final int untilIndex = node.getLabel().indexOf(UNTIL_KEY_WORD);
		if (untilIndex != -1) {
			translation.append(node.getLabel().substring(0, untilIndex).replace(PERFORM_KEY_WORD, StringUtils.EMPTY));
		} else {
			translation.append(node.getLabel().replace(PERFORM_KEY_WORD, StringUtils.EMPTY));
		}
		node.getChildren().stream()
				.filter(Objects::nonNull)
				.map(statement -> translate(offset + 1, statement))
				.forEach(translation::append);
		return prettifyWithOffset(offset, translateVariables(node, translation.toString()), node);
	}

	private void translateConditions(final StringBuilder translation, final List<AstNodePojo> conditions) {
		if (conditions.size() > 1) {
			translation.append(TAB).append(MULTIPLE_CONDITION_MESSAGE).append(NEW_LINE);
		} else if (conditions.size() == 1) {
			final String conditionLabel = conditions.get(0).getLabel();
			if (conditionLabel.contains(AND_KEY_WORD) || conditionLabel.contains(OR_KEY_WORD)) {
				translation.append(TAB).append(MULTIPLE_CONDITION_MESSAGE).append(NEW_LINE);
			}
		}
		for (final AstNodePojo condition : conditions) {
			String conditionTranslation = condition.getLabel();
			conditionTranslation = translateVariables(condition, conditionTranslation);
			conditionTranslation = translateOperatorsAndStatement(conditionTranslation);
			conditionTranslation = conditionTranslation.replace(" OR ", NEW_LINE + TAB + " or ");
			conditionTranslation = conditionTranslation.replace(" AND ", NEW_LINE + TAB + " and ");
			translation.append(TAB).append(conditionTranslation).append(NEW_LINE);
		}
	}

	private String translateOperatorsAndStatement(final String translation) {
		final String translatedOperator = translateReadFileStatement(translation);
		final String regex = "'[^']*'|(" + String.join("|", TRANSLATOR.keySet()) + ")";
		final StringBuilder translatedOperators = new StringBuilder();
		try {
			final Matcher matcher = Pattern.compile(regex).matcher(translatedOperator);
			while (matcher.find()) {
				try {
					if (matcher.group(1) != null) {
						String replacement = TRANSLATOR.get(matcher.group(1));
						if (replacement == null) {
							replacement = TRANSLATOR.get(" \\" + matcher.group(1).trim() + " ");
						}
						if (replacement != null) {
							matcher.appendReplacement(translatedOperators, Matcher.quoteReplacement(replacement));
						} else {
							matcher.appendReplacement(translatedOperators, matcher.group(1));
						}
					} else {
						matcher.appendReplacement(translatedOperators, matcher.group(0).replace("<", " < "));
					}
				} catch (final IllegalStateException e) {
					LOG.info("No match found: ", e);
				}
			}
			matcher.appendTail(translatedOperators);
		} catch (final PatternSyntaxException e) {
			LOG.info("Invalid regex: ", e);
		}
		return translatedOperators.toString();
	}

	private String translateVariables(final AstNodePojo node, final String translation) {
		final List<String> variables = FIELD_REFERENCE_COLLECTOR.allDeep(node).stream().map(AstNodeUtils::getFieldDefinitionNameFromReference).distinct()
				.collect(Collectors.toList());
		String translatedVariable = translation;

		for (final String variableName : variables) {
			final Tuple2<AstNodePojo, DataDictionaryPojo> astAndDde = astAndDdeMap.get(variableName);
			final String escapedVariableName = Pattern.quote(variableName);
			final Pattern pattern = Pattern.compile("([?^\\s=])(" + escapedVariableName + ")([?\\s+,-=.])");
			translatedVariable = translatedVariable.replaceAll(pattern.toString(),
					astAndDde != null ? "$1" + astAndDde.b.getDescription() + "$3" : "$1<i>" + variableName + "</i>$3");
		}
		return translatedVariable;
	}

	private String prettifyWithOffset(final int offset, final String translation, final AstNodePojo node) {
		final StringBuilder tabs = new StringBuilder();
		if (offset >= 1) {
			tabs.append(TAB);
		}
		String editedTranslation = translateOperatorsAndStatement(translation);
		final int lastIndex = editedTranslation.lastIndexOf(END_KEY_WORD);

		if (lastIndex != -1) {
			final String endSubString = editedTranslation.substring(lastIndex);
			if (END_SUFFIX.stream().anyMatch(element -> endSubString.toUpperCase().contains(element.toUpperCase()))) {
				editedTranslation = editedTranslation.substring(0, lastIndex);
			}
		}
		editedTranslation = translateCobolSubstring(editedTranslation);
		editedTranslation = translateVariables(node, editedTranslation);
		return tabs + editedTranslation.replace(NEW_LINE, NEW_LINE + tabs) + NEW_LINE;
	}

	private static String translateCobolSubstring(final String translation) {
		final Pattern pattern = Pattern.compile("\\((\\d+):(\\d+)\\)");
		final Matcher matcher = pattern.matcher(translation);
		if (matcher.find()) {
			try {
				final int offset = Integer.parseInt(matcher.group(1)) - 1;
				final int length = Integer.parseInt(matcher.group(2)) - offset;
				return translation.replaceAll(pattern.toString(), " Substring Of (Offset " + (offset + 1) + " : Length " + length + ")");
			} catch (final NumberFormatException e) {
				LOG.debug("NumberFormatException occurred: " + e.getMessage());
			}
		}
		return translation;
	}

	private String translateReadFileStatement(final String nodeLable) {
		String fileAccessTranslation = nodeLable.replace("NOT AT END", "\nIf\n \tEnd of File Equal To false \nThen\n \t");
		fileAccessTranslation = fileAccessTranslation.replace("AT END", "\nIf\n \t End of File Equal To true \nThen\n \t");
		fileAccessTranslation = fileAccessTranslation.replace("NOT INVALID KEY", " \nIf\n \t Invalid key Equal To false \nThen \\n \\t");
		fileAccessTranslation = fileAccessTranslation.replace("INVALID KEY", "\nIf\n \t Invalid key Equal To true \n Then\n \\t");
		return fileAccessTranslation;
	}

}
