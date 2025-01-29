/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.bms;

import java.awt.Dimension;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import innowake.lib.core.IProgress;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.moduledescription.api.Extractor;
import innowake.ndt.cobol.parser.bms.BMSParserAst;
import innowake.ndt.cobol.parser.bms.model.BmsDfhmdfNode;
import innowake.ndt.cobol.parser.bms.model.BmsDfhmdfNode.Attrb;
import innowake.ndt.cobol.parser.bms.model.BmsDfhmdfNode.Pos;
import innowake.ndt.cobol.parser.bms.model.BmsDfhmdfNode.PosDisplacement;
import innowake.ndt.cobol.parser.bms.model.BmsDfhmdfNode.PosXY;
import innowake.ndt.cobol.parser.bms.model.BmsDfhmdiNode;
import innowake.ndt.cobol.parser.bms.model.BmsModel;

/**
 * Extractor that turns BMS maps to text.
 */
public class BMSMapToTextExtractor implements Extractor {
	/**
	 * Singleton instance of a BMSMapToTextExtractor.
	 */
	public static final BMSMapToTextExtractor INSTANCE = new BMSMapToTextExtractor();
	/**
	 * Message that gets printed into the module description if a parsing error occurs.
	 */
	public static final String CORRUPTED_VIEW_MESSAGE = BMSDescriptionExtractor.LINE_BREAK + "!!! CORRUPTED MAP !!!" + BMSDescriptionExtractor.LINE_BREAK;
	
	private static final Logger LOG = LoggerFactory.getLogger(BMSMapToTextExtractor.class);
	private static final String NEW_LINE = BMSDescriptionExtractor.LINE_BREAK;
	
	private BMSMapToTextExtractor () {}
	
	@Override
	public Stream<String> extract(final String content) {
		final BmsModel bmsModel = new BMSParserAst(LOG, IProgress.DUMMY).parse(content);
		final StringBuilder extractedView = new StringBuilder();
		final List<String> list = new ArrayList<>();
		for (final BmsDfhmdiNode mapNode : bmsModel.getBmsDfhmdiNode()) {
			extractMap(mapNode, extractedView);
			list.add(extractedView.toString());
			extractedView.setLength(0);
		}
		return list.stream();
	}
	
	/**
	 * Extracts a mapNode to text by parsing it by hand.
	 * @param mapNode the mapNode to turn into text.
	 * @param extractedView the placeholder for the text.
	 */
	private void extractMap(final BmsDfhmdiNode mapNode, final StringBuilder extractedView) {
		extractedView.append(mapNode.getId()).append(":").append(NEW_LINE);
		int line = 1;
		int column = 1;
		final Dimension dimension = mapNode.getSize();
		final int max_col = dimension != null ? dimension.width : 80;
		final int max_row = dimension != null ? dimension.height : 24;
		for (final BmsDfhmdfNode field : mapNode.getBmsDfhmdfNodes()) {
			final Pos<?> pos = field.getPos();
			final PosXY posXY;
			
			if (pos instanceof PosDisplacement) {
				final PosDisplacement pd = (PosDisplacement) pos;
				
				posXY = field.new PosXY( (pd.displacement + 1) % max_col, (pd.displacement + 1) / max_col);
			} else {
				if (pos == null) {
					/* We found a corrupted map and we don't want to crash in a NullPointerException. */
					LOG.debug(() -> "Can't fully extract bms node " + field.getInitial());
					extractedView.append(CORRUPTED_VIEW_MESSAGE);
					return;
				}
				posXY = pos.getPosXY(0);
			}
			if (posXY == null) {
				extractedView.append("unsupported pos type found");
			}
			
			if (posXY == null) {
				LOG.debug(() -> "Can't fully extract bms node " + field.getInitial());
				extractedView.append(CORRUPTED_VIEW_MESSAGE);
				return;
			}

			final int fieldX = posXY.x;
			final int fieldY = posXY.y;
			
			for (; line < fieldY; line++) {
				for (; column < max_col; column++) {
					extractedView.append(' ');
				}
				extractedView.append(NEW_LINE);
				column = 1;
			}
			for (; column < fieldX; column++) {
				extractedView.append(' ');
			}
			final boolean isUnprotected = isUnprotected(field);
			int printedChars = 0;
			if (field.getInitial() != null) {
				extractedView.append(field.getInitial());
				printedChars = field.getInitial().length();
				if (isUnprotected) {
					for (int i = printedChars; i <= field.getLengthAttribute().intValue(); i++) {
						extractedView.append("_");
					}
				}
			} else if (isUnprotected) {
				extractedView.append(repeat('_', field.getLengthAttribute().intValue()));
				printedChars = field.getLengthAttribute().intValue();
			}
			column += printedChars;
		}
		for (; column < max_col; column++) {
			extractedView.append(' ');
		}
		for (; line <= max_row; line++) {
			extractedView.append(NEW_LINE);
			for (; column < max_col; column++) {
				extractedView.append(' ');
			}
		}
	}
	
	/**
	 * Repeats a {@link repeatChar} for {@link length} characters.
	 *
	 * @param repeatChar the char to repeat
	 * @param length how many chars to repeat
	 * @return A string containing the repeated chars.
	 */
	private String repeat(final char repeatChar, final int length) {
		final StringBuilder sb = new StringBuilder();
		for (int i = 0; i < length; i++) {
			sb.append(repeatChar);
		}
		return sb.toString();
	}
	
	/**
	 * Checks if a field is unprotected.
	 *
	 * @param field the field to check
	 * @return true if the field is unprotected.
	 */
	private boolean isUnprotected(final BmsDfhmdfNode field) {
		for (Attrb attrb : field.getAttributes()) {
			if (attrb == Attrb.UNPROT) {
				return true;
			}
		}
		return false;
	}

}
