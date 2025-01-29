/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.natural;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MultiValuedMap;

import innowake.lib.core.util.collection.CollectionUtil;
import innowake.lib.core.util.collection.MultiValueMapUtil;


/**
 * Natural data definitions within a define data block. 
 * <p>
 * Almost originally copied from {@code /ndt-analysis-scripts/expert-base/innowake/expert/base/ndt/parser/definedata/DataArea.java}.
 * Should be adapted to code guidelines if someone finds time.
 */
public class DataArea {
	
	private static final String LINE_SEPARATOR = System.getProperty("line.separator");
	
	private final List<DataField> parameter = new ArrayList<>();
	private final List<DataField> locals = new ArrayList<>();
	private final List<DataField> globals = new ArrayList<>();
	private final List<DataField> independents = new ArrayList<>();
	private final MultiValuedMap<String, DataField> fieldMap = MultiValueMapUtil.createArrayListHashMap();

	/**
	 * Resolves a data field by name.
	 *
	 * @param name the name of the data field
	 * @return a list of data fields with the name
	 */
	public List<DataField> resolve(final String name) {
		final List<DataField> list = (List<DataField>) fieldMap.get(assertNotNull(name));
		return CollectionUtil.unmodifiableList(list);
	}
	
	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("DataArea:").append(LINE_SEPARATOR);
		toString(sb, "globals", globals);
		toString(sb, "parameters", parameter);
		toString(sb, "locals", locals);
		toString(sb, "independents", independents);
		return sb.toString();
	}
	
	void addField(final DataField df) {
		fillFieldMap(df);
		switch (df.getType()) {
			case GLOBAL:
				globals.add(df);
				break;
			case LOCAL:
				locals.add(df);
				break;
			case PARAMETER:
				parameter.add(df);
				break;
			case INDEPENDENT:
				independents.add(df);
				break;
			default:
				throw new IllegalStateException("Unknown field type " + df.getType().name());
		}
	}
	
	void finalizeStructure() {
		finalizeStructure(globals);
		finalizeStructure(parameter);
		finalizeStructure(locals);
	}
	
	private void toString(final StringBuilder sb, final String name, final List<DataField> dataFields) {
		sb.append("--------------------------------------------------").append(LINE_SEPARATOR);
		sb.append(name).append(":").append(LINE_SEPARATOR);
		sb.append("--------------------------------------------------").append(LINE_SEPARATOR);
		for (final DataField field : dataFields) {
			sb.append(field).append(LINE_SEPARATOR);
		}
	}
	
	private void finalizeStructure(final List<DataField> fields) {
		for (final DataField field : fields) {
			if (field.isGroup()) {
				finalizeStructure(((Group) field).getFields());
			}

			/* during parsing only occurrences for actual field levels were recognized.
			 * => set array dimension after tree is complete (inheritance of group-occurrences etc.) */
			/* WMEE-2059: handle also views */
			final Optional<DataField> optionalParent = field.getParent();
			if (field.getLevel() == 1 || (optionalParent.isPresent() && optionalParent.get().isView() && field.getLevel() == 2)) {
				field.setOccurrencesOverall(field.getOccurrences());
			}
			
			/* link redefinitions with original fields */
			if (field.isRedefinition()) {
				final Redefinition redefinition = (Redefinition) field;
				DataField redefinedField = null; 
				final String redefinedFieldName = redefinition.getRedefinedFieldName();
				final Optional<DataField> parent = redefinition.getParent();
				if ( ! parent.isPresent()) {
					final List<DataField> fieldList = resolve(redefinedFieldName);
					if (CollectionUtils.isNotEmpty(fieldList)) {
						redefinedField = fieldList.get(0); 
					}
				} else {
					/* get redefined field within current group */
					final Group group = (Group) parent.get();
					for (final DataField gField : group.getFields()) {
						final Optional<String> quaifiedName = gField.getQualifiedName();
						if ( ! gField.isRedefinition() && (quaifiedName.isPresent() && quaifiedName.get().equals(redefinedFieldName)) ||
								gField.getFieldName().equals(redefinedFieldName)) {
								redefinedField = gField;
							break;
						}
					}
				}
				if (redefinedField != null) {
					redefinition.setRedefinedField(redefinedField);
					redefinedField.addRedefinition(redefinition);
				}
			}
		}	
	}
	
	private void fillFieldMap(final DataField field) {
		final Optional<String> qualifiedName = field.getQualifiedName();
		if (qualifiedName.isPresent() && qualifiedName.get().indexOf('.') != -1) {
			fieldMap.put(qualifiedName.get().toUpperCase(), field);
		} 
		fieldMap.put(field.getFieldName().toUpperCase(), field);
		if (field.isGroup()) {
			final Group group = (Group) field;
			for (final DataField groupField : group.getFields()) {
				fillFieldMap(groupField);
			}
		}
	}
	
}
