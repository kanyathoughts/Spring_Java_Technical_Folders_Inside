/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation.impl;

import org.apache.commons.lang.StringUtils;

import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Interface to translate annotations and get English translation of code.
 */
public abstract class AnnotationTranslator {
	
	/**
	 * Translate the annotation to English 
	 * 
	 * @param offset the level of translation
	 * @param node the node related to the annotation
	 * @return English translation of the annotation
	 */
	public abstract String translate(final int offset, final AstNodePojo node);
	
	/**
	 * Format the translation to remove extra enter and space
	 * 
	 * @param translation of annotation
	 * @return formated English translation of the annotation
	 */
	protected String formatTranslate(final String translation) {
		String editedTranslation = translation;
		if ( ! editedTranslation.equals(StringUtils.EMPTY)) {
			while (editedTranslation.charAt(0) == '\n') {
				editedTranslation = editedTranslation.substring(1);
			}
			while (editedTranslation.charAt(editedTranslation.length() - 1) == '\n') {
				editedTranslation = editedTranslation.substring(0, editedTranslation.length() - 1);
			}
		}
		return editedTranslation.replace("  ", " ");
	}
}
