/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser.natural;


import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.lang.Nullable;
import innowake.ndt.naturalparser.ast.NaturalNode;


/**
 * DataField for model of {@link DefineDataParser}.
 * <p>
 * Almost originally copied from {@code /ndt-analysis-scripts/expert-base/innowake/expert/base/ndt/parser/definedata/DataField.java}.
 * Should be adapted to code guidelines if someone finds time.
 */
public class DataField {

	public enum FieldType {
		PARAMETER, LOCAL, GLOBAL, INDEPENDENT;
	}

	protected int byteLength = -1;

	private final String fieldName;
	private final NaturalNode	naturalNode;

	private FieldType type;
	private int length;
	@Nullable
	private DataField parent;
	@Nullable
	private String qualifiedName;
	private int	level;
	@Nullable
	private String naturalType;
	@Nullable
	private String sLength;
	private List<Redefinition> redefinitions = new ArrayList<>();
	private boolean optional;
	private boolean	byValue;
	private boolean	byValueResult;
	private boolean	constant;
	@Nullable
	private String constValue;
	@Nullable
	private String initialValue;
	@Nullable
	private DataField level1Field;

	/* occs for actual level, max 3 dim */
	protected int[]	occurrences = new int[] { -1, -1, -1 };
	/* occs for actual level and levels above, max 3 dim */
	private int[] occurrencesOverall = new int[] { -1, -1, -1 };
	/* ranges for actual level */
	private int[] ranges = new int[] { -1, -1, -1, -1, -1, -1 };
	private int						dimCount;
	
	DataField(final String name, final FieldType type, final NaturalNode node) {
		fieldName = name;
		naturalNode = node;
		this.type = type;
	}
	
	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		final String parentName = parent != null ? parent.getQualifiedName().orElse("") : "";
		sb.append("DataField [fieldName=").append(getFieldName()).append(", qualifiedName= ").append(qualifiedName).append(", occurrencesOverall=").append(Arrays.toString(occurrencesOverall)).append(", byValue=").append(byValue)
			.append(", dimCount=").append(dimCount).append(", length=").append(length).append(", level=").append(level).append(", constantValue=").append(constValue)
			.append(", initValue=").append(initialValue).append(", naturalNode=").append(naturalNode).append(", naturalType=").append(naturalType).append(", occurrences=")
			.append(Arrays.toString(occurrences)).append(", optional=").append(optional).append(", parent=").append(parentName).append(", redefinitions=").append(redefinitions).append(", sLength=").append(sLength).append(", type=").append(type).append("]")
			.append(System.getProperty("line.separator"));
		return sb.toString();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + fieldName.hashCode();
		result = prime * result + level;
		result = prime * result + naturalNode.hashCode();
		result = prime * result + type.hashCode();
		return result;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if ( ! (obj instanceof DataField)) {
			return false;
		}
		DataField other = (DataField) obj;
		if ( ! fieldName.equals(other.fieldName)) {
			return false;
		}
		if (level != other.level) {
			return false;
		}
		if ( ! naturalNode.equals(other.naturalNode)) {
			return false;
		}
		if (type != other.type) {
			return false;
		}
		return true;
	}
	
	public Optional<DataField> getParent() {
		return Optional.ofNullable(parent);
	}
	
	public int getLevel() {
		return level;
	}

	public FieldType getType() {
		return type;
	}
	
	
	public String getFieldName() {
		return fieldName;
	}
	
	public int[] getOccurrences() {
		return occurrences;
	}
	
	public int[] getOccurrencesOverall() {
		return occurrencesOverall;
	}
	
	public int[] getRanges() {
		return ranges;
	}
	
	public NaturalNode getNaturalNode() {
		return naturalNode;
	}
	
	public int getDimensionCount() {
		return dimCount;		
	}
	
	public Optional<String> getNaturalType() {
		return Optional.ofNullable(naturalType);
	}

	public boolean isLocal() {
		return type == FieldType.LOCAL;
	}
	
	public boolean isParameter() {
		return type == FieldType.PARAMETER;
	}
	
	public boolean isGlobal() {
		return type == FieldType.GLOBAL;
	}
	
	public boolean isIndependent() {
		return type == FieldType.INDEPENDENT;
	}
	
	/**
	 * @return length of the field or <code>Integer.MAX_VALUE</code>
	 * if length is dynamic
	 */
	public int getLength() {
		return length;
	}
	
	public int getByteLength() {
		if (byteLength == -1) {
			byteLength = 0;
			int occLength = 1;
			for (int i = 0; i < 3; i++) {
				if (occurrences[i] == DefineDataParser.DYNAMIC_LENGTH) {
					byteLength = Integer.MAX_VALUE;
					break;
				}
				if (occurrences[i] > 0) {
					occLength *= occurrences[i];
				}
			}
			if (byteLength != Integer.MAX_VALUE) {
				byteLength = getLength() * occLength;
			}
		}
		return byteLength;
	}

	public boolean isOptional() {
		return optional;
	}
	
	public boolean isByValue() {
		return byValue;
	}
	
	public boolean isConstant() {
		return constant;
	}
	
	public boolean isByValueResult() {
		return byValueResult;
	}
	
	/**
	 * @return string representation for field length, or "*" if length
	 * is dynamic
	 */
	public Optional<String> getsLength() {
		return Optional.ofNullable(sLength);
	}

	public List<Redefinition> getRedefinitions() {
		return redefinitions;
	}
	
	public boolean isGroup() {
		return (this instanceof Group);
	}
	
	public boolean isRedefinition() {
		return (this instanceof Redefinition);
	}
	
	public boolean isView() {
		return (this instanceof View);
	}
	
	public boolean isCStarField() {
		boolean result = false;
		if (StringUtils.startsWithIgnoreCase(fieldName, "C*")) {
			result = ! isGroup() 
					&& naturalNode instanceof innowake.ndt.naturalparser.ast.DataDefinition.Group 
					&& isPartOfView();
		}
		return result;
	}
	
	public boolean isPartOfView() {
		boolean result = false;
		DataField parentLocal = this.parent;
		while (parentLocal != null) {
			if (parentLocal.isView()) {
				result = true;
				break;
			}
			parentLocal = parentLocal.parent;
		}
		return result;
	}
	
	public Optional<String> getQualifiedName() {	
		if ( ! isRedefinition() && StringUtils.isEmpty(qualifiedName)) {
			qualifiedName = createQualifiedName();
		}
		return Optional.ofNullable(qualifiedName);
	}
	
	/**
	 * returns the ancestor of the field with the given
	 * qualified name (if exists)
	 * 
	 * @param qualifiedParentName the qualified name of the
	 * parent data field
	 * @return the parent datafield if found, else <code>null</code>;
	 */
	public Optional<DataField> getParent(final String qualifiedParentName) {
		DataField myParent = parent;
		while (myParent != null) {
			final Optional<String> myParentQualifiedName = myParent.getQualifiedName();
			if ( ! myParent.isRedefinition() && myParentQualifiedName.isPresent() && myParentQualifiedName.get().equals(qualifiedParentName)) {
				return Optional.of(myParent);
			} else {
				myParent = myParent.getParent().orElse(null);
			}
		}
		return Optional.empty();
	}
	
	public DataField getLevel1Field() {
		if (level1Field == null) {
			if (parent == null) {
				level1Field = this;
			} else {
				DataField myParent = assertNotNull(parent);
				Optional<DataField> myGrandParent = myParent.getParent();
				while (myGrandParent.isPresent()) {
					myParent = myGrandParent.get();
					myGrandParent = myParent.getParent();
				}
				level1Field = myParent;
			}
		}
		return assertNotNull(level1Field);
	}
	
	/**
	 * returns the redefined field (top level redefinition)
	 * 
	 * @return the redefined field if exists, else the field itself
	 */
	public DataField getRedefinedField() {
		DataField redefined = this;
		DataField myParent = parent;
		Redefinition lastRedefinition = null;
		if (myParent != null && myParent.isRedefinition()) {
			lastRedefinition = (Redefinition) myParent;
		}
		Optional<DataField> myGrandParent;
		while (myParent != null) {
			myGrandParent = myParent.getParent();
			if (myGrandParent.isPresent()) {
				myParent = myGrandParent.get();
				if (myParent.isRedefinition()) {
					lastRedefinition = (Redefinition) myParent;
				}
			} else {
				break;
			}
		}
		if (lastRedefinition != null) {
			redefined = lastRedefinition.getRedefinedField();
		}
		return redefined;
	}
	
	/**
	 * returns the nearest (bottom-up-order) redefined field
	 * 
	 * @return the nearest (bottom-up-order) redefined field, or the field itself
	 * if the field is not a child of a redefinition
	 */
	public DataField getNearestRedefinedField() {
		DataField redefined = this;
		DataField myParent = parent;
		Redefinition lastRedefinition = null;
		if (myParent != null && myParent.isRedefinition()) {
			lastRedefinition = (Redefinition) myParent;
		}
		Optional<DataField> myGrandParent;
		while (lastRedefinition == null && myParent != null) {
			myGrandParent = myParent.getParent();
			if (myGrandParent.isPresent()) {
				myParent = myGrandParent.get();
				if (myParent.isRedefinition()) {
					lastRedefinition = (Redefinition) myParent;
				}
			} else {
				break;
			}
		}
		if (lastRedefinition != null) {
			redefined = lastRedefinition.getRedefinedField();
		}
		return redefined;
	}
	
	/**
	 * returns the const value as string (don't support const arrays)
	 * 
	 * @return const value as string, or null if field is not a constant
	 */
	public Optional<String> getConstValue() {
		return Optional.ofNullable(constValue);
	}
	
	/**
	 * returns the initial value as string (don't support init arrays yet)
	 * 
	 * @return initial value as string, or null if field has no initial value
	 */
	public Optional<String> getInitialValue() {
		return Optional.ofNullable(initialValue);
	}
	
	void addRedefinition(final Redefinition redef) {
		this.redefinitions.add(redef);
	}
	
	void setLength(final int length) {
		this.length = length;
	}
	
	void setOptional(final boolean optional) {
		this.optional = optional;
	}
	
	void setByValue(final boolean byValue) {
		this.byValue = byValue;
	}
	
	void setConstant(final boolean constant) {
		this.constant = constant;
	}

	void setQualifiedName(final String qualifiedName) {
		this.qualifiedName = qualifiedName;
	}

	void setOccurrences(final int[] occurrences) {
		this.occurrences = occurrences;
	}

	void setOccurrencesOverall(final int[] occurrencesOverall) {
		this.occurrencesOverall = occurrencesOverall;
		this.dimCount = 0;
		for (int i = 0; i < this.occurrencesOverall.length; i++) {
			if (occurrencesOverall[i] != -1) {
				this.dimCount++;
			}
		}
	}
	
	void setRanges(final int[] ranges) {
		this.ranges = ranges;
	}
	
	void setParent(final DataField parent) {
		this.parent = parent;
	}
	
	void setsLength(final String sLength) {
		this.sLength = sLength;
	}
	
	void setLevel(final int level) {
		this.level = level;
	}

	void setType(final FieldType type) {
		this.type = type;
	}
	
	void setConstValue(@Nullable final String constValue) {
		this.constValue = constValue;
	}
	
	void setInitialValue(@Nullable final String initValue) {
		this.initialValue = initValue;
	}
	
	void setNaturalType(final String type) {
		this.naturalType = type;
	}
	
	void setByValueResult(final boolean byValueResult) {
		this.byValueResult = byValueResult;
	}

	protected static int getDimension(final int[] occs) {
		int dim = 0;
		for (int i = 0; i < occs.length; i++) {
			if (occs[i] != -1) {
				dim++;
			}
		}
		return dim;
	}

	private String createQualifiedName() {
		String result = "";
		DataField field = parent;
		DataField lastParent = this;
		while (field != null) {
			lastParent = field;
			field = field.getParent().orElse(null);
		}
		if (lastParent != this) {
			String parentName = lastParent.getFieldName(); 
			if (lastParent.isRedefinition()) {
				final  Redefinition redef = (Redefinition) lastParent;
				parentName = redef.getRedefinedFieldName();
			}
			result = parentName + ".";
		}
		result += getFieldName();
		return result;
	}

}
