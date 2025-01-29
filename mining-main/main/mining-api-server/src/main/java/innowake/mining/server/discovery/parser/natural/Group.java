/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.natural;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;

import innowake.lib.core.lang.Nullable;
import innowake.ndt.naturalparser.ast.NaturalNode;

/**
 * Group for model of {@link DefineDataParser}.
 * <p>
 * Almost originally copied from {@code /ndt-analysis-scripts/expert-base/innowake/expert/base/ndt/parser/definedata/Group.java}.
 * Should be adapted to code guidelines if someone finds time.
 */
public class Group extends DataField {

	private List<DataField> fields = new ArrayList<>();
	
	Group(final String name, final FieldType type, final NaturalNode node) {
		super(name, type, node);
	}

	public void addField(final DataField field) {
		field.setParent(this);
		fields.add(field);
	}
	
	public List<DataField> getFields() {
		return fields;
	}

	public List<DataField> getAllFieldsFlat() {
		return getAllFieldsFlat(false);
	}
	
	public List<DataField> getAllFieldsFlat(final boolean ignoreRedifinitions) {
		final List<DataField> allFields = new ArrayList<>();
		for (final DataField field : fields) {
			if (field.isGroup() ) {
				if (ignoreRedifinitions && field.isRedefinition()) {
					/* continue */
				} else {
					final Group group = (Group) field;
					allFields.addAll(group.getAllFieldsFlat(ignoreRedifinitions));
				}
			} else {
				allFields.add(field);
			}
		}
		return allFields;
	}
	
	public List<DataField> getAllFieldsWithGroupsFlat(final boolean ignoreRedefinitions) {
		final List<DataField> allFields = new ArrayList<>();
		for (final DataField field : fields) {
			if ( ! field.isGroup()) {
				allFields.add(field);
			} else {
				if (ignoreRedefinitions && field.isRedefinition()) {
					/* continue */
				} else {
					allFields.add(field);
					final Group group = (Group) field;
					allFields.addAll(group.getAllFieldsWithGroupsFlat(ignoreRedefinitions));
				}
			}
		}
		return allFields;
	}
	
	public List<DataField> getAllFieldsWithGroupsFlat() {
		return getAllFieldsWithGroupsFlat(false);
	}
	
	@Override
	public void setOccurrencesOverall(final int[] arrayDimensions) {
		super.setOccurrencesOverall(arrayDimensions);
		for (final DataField member : fields) {
			final int[] memberOccs = member.getOccurrences();
			if (getDimension(memberOccs) == 0) {
				member.setOccurrencesOverall(arrayDimensions);
			} else {
				/* add parent occs */
				int[] newOccs = memberOccs;
				for (int i = 2; i >= 0; i--) {
					final int occ = arrayDimensions[i];
					if (occ != -1) {
						newOccs = ArrayUtils.add(memberOccs,0, occ); 
						newOccs = ArrayUtils.subarray(newOccs, 0, 3);
					}
				}
				member.setOccurrencesOverall(newOccs);
			}
		}
	}
	
	@Override
	void setType(final FieldType type) {
		super.setType(type);
		for (final DataField field : fields) {
			field.setType(type);
		}
	}
	
	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder(super.toString());
		for (final DataField field : fields) {
			sb.append(field.toString());
		}
		return sb.toString();
	}
	
	@Override
	public boolean equals(@Nullable Object obj) {
		return super.equals(obj);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
	
	@Override
	public int getByteLength() {
		if (byteLength == -1) {
			byteLength = 0;
			for (final DataField member : getFields()) {
				/* redefs have no additional length */
				if ( ! member.isRedefinition()) {
					final int memberLength = member.getByteLength();
					if (memberLength == Integer.MAX_VALUE) {
						byteLength = memberLength;
						break;
					} else {
						byteLength += memberLength;
					}
				}
			}
			if (byteLength != Integer.MAX_VALUE) {
				int occLength = 1;
				for (int i = 0; i < 3; i++) {
					if (occurrences[i] == DefineDataParser.DYNAMIC_LENGTH) {
						return Integer.MAX_VALUE;
					}
					if (occurrences[i] > 0) {
						occLength *= occurrences[i];
					}
				}
				byteLength = byteLength * occLength;
			}
		}
		return byteLength;
	}
	
}
