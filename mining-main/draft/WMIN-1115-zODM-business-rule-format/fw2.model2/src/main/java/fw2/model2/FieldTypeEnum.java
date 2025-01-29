/**
 */
package fw2.model2;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Field Type Enum</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see fw2.model2.Model2Package#getFieldTypeEnum()
 * @model extendedMetaData="name='FieldTypeEnum'"
 * @generated
 */
public enum FieldTypeEnum implements Enumerator {
	/**
     * The '<em><b>Selectable</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #SELECTABLE_VALUE
     * @generated
     * @ordered
     */
	SELECTABLE(0, "Selectable", "Selectable"),

	/**
     * The '<em><b>Date</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #DATE_VALUE
     * @generated
     * @ordered
     */
	DATE(1, "Date", "Date"),

	/**
     * The '<em><b>Text</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #TEXT_VALUE
     * @generated
     * @ordered
     */
	TEXT(2, "Text", "Text"),

	/**
     * The '<em><b>Map</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #MAP_VALUE
     * @generated
     * @ordered
     */
	MAP(3, "Map", "Map"),

	/**
     * The '<em><b>Text Decimal</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #TEXT_DECIMAL_VALUE
     * @generated
     * @ordered
     */
	TEXT_DECIMAL(4, "TextDecimal", "Text_Decimal"),

	/**
     * The '<em><b>Zip</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #ZIP_VALUE
     * @generated
     * @ordered
     */
	ZIP(5, "Zip", "Zip"),

	/**
     * The '<em><b>Text Integer</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #TEXT_INTEGER_VALUE
     * @generated
     * @ordered
     */
	TEXT_INTEGER(6, "TextInteger", "Text_Integer"),

	/**
     * The '<em><b>Linked Object</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #LINKED_OBJECT_VALUE
     * @generated
     * @ordered
     */
	LINKED_OBJECT(7, "LinkedObject", "LinkedObject"),

	/**
     * The '<em><b>Label</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #LABEL_VALUE
     * @generated
     * @ordered
     */
	LABEL(8, "Label", "Label"),

	/**
     * The '<em><b>Month</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #MONTH_VALUE
     * @generated
     * @ordered
     */
	MONTH(9, "Month", "Month"),

	/**
     * The '<em><b>Zip Extension</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #ZIP_EXTENSION_VALUE
     * @generated
     * @ordered
     */
	ZIP_EXTENSION(10, "ZipExtension", "ZipExtension"),

	/**
     * The '<em><b>Row Text</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #ROW_TEXT_VALUE
     * @generated
     * @ordered
     */
	ROW_TEXT(11, "RowText", "RowText");

	/**
     * The '<em><b>Selectable</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Selectable</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #SELECTABLE
     * @model name="Selectable"
     * @generated
     * @ordered
     */
	public static final int SELECTABLE_VALUE = 0;

	/**
     * The '<em><b>Date</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Date</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #DATE
     * @model name="Date"
     * @generated
     * @ordered
     */
	public static final int DATE_VALUE = 1;

	/**
     * The '<em><b>Text</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Text</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #TEXT
     * @model name="Text"
     * @generated
     * @ordered
     */
	public static final int TEXT_VALUE = 2;

	/**
     * The '<em><b>Map</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Map</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #MAP
     * @model name="Map"
     * @generated
     * @ordered
     */
	public static final int MAP_VALUE = 3;

	/**
     * The '<em><b>Text Decimal</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Text Decimal</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #TEXT_DECIMAL
     * @model name="TextDecimal" literal="Text_Decimal"
     * @generated
     * @ordered
     */
	public static final int TEXT_DECIMAL_VALUE = 4;

	/**
     * The '<em><b>Zip</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Zip</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #ZIP
     * @model name="Zip"
     * @generated
     * @ordered
     */
	public static final int ZIP_VALUE = 5;

	/**
     * The '<em><b>Text Integer</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Text Integer</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #TEXT_INTEGER
     * @model name="TextInteger" literal="Text_Integer"
     * @generated
     * @ordered
     */
	public static final int TEXT_INTEGER_VALUE = 6;

	/**
     * The '<em><b>Linked Object</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Linked Object</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #LINKED_OBJECT
     * @model name="LinkedObject"
     * @generated
     * @ordered
     */
	public static final int LINKED_OBJECT_VALUE = 7;

	/**
     * The '<em><b>Label</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Label</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #LABEL
     * @model name="Label"
     * @generated
     * @ordered
     */
	public static final int LABEL_VALUE = 8;

	/**
     * The '<em><b>Month</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Month</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #MONTH
     * @model name="Month"
     * @generated
     * @ordered
     */
	public static final int MONTH_VALUE = 9;

	/**
     * The '<em><b>Zip Extension</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Zip Extension</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #ZIP_EXTENSION
     * @model name="ZipExtension"
     * @generated
     * @ordered
     */
	public static final int ZIP_EXTENSION_VALUE = 10;

	/**
     * The '<em><b>Row Text</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>Row Text</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #ROW_TEXT
     * @model name="RowText"
     * @generated
     * @ordered
     */
	public static final int ROW_TEXT_VALUE = 11;

	/**
     * An array of all the '<em><b>Field Type Enum</b></em>' enumerators.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private static final FieldTypeEnum[] VALUES_ARRAY =
		new FieldTypeEnum[] {
            SELECTABLE,
            DATE,
            TEXT,
            MAP,
            TEXT_DECIMAL,
            ZIP,
            TEXT_INTEGER,
            LINKED_OBJECT,
            LABEL,
            MONTH,
            ZIP_EXTENSION,
            ROW_TEXT,
        };

	/**
     * A public read-only list of all the '<em><b>Field Type Enum</b></em>' enumerators.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public static final List<FieldTypeEnum> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
     * Returns the '<em><b>Field Type Enum</b></em>' literal with the specified literal value.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param literal the literal.
     * @return the matching enumerator or <code>null</code>.
     * @generated
     */
	public static FieldTypeEnum get(String literal) {
        for (int i = 0; i < VALUES_ARRAY.length; ++i) {
            FieldTypeEnum result = VALUES_ARRAY[i];
            if (result.toString().equals(literal)) {
                return result;
            }
        }
        return null;
    }

	/**
     * Returns the '<em><b>Field Type Enum</b></em>' literal with the specified name.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param name the name.
     * @return the matching enumerator or <code>null</code>.
     * @generated
     */
	public static FieldTypeEnum getByName(String name) {
        for (int i = 0; i < VALUES_ARRAY.length; ++i) {
            FieldTypeEnum result = VALUES_ARRAY[i];
            if (result.getName().equals(name)) {
                return result;
            }
        }
        return null;
    }

	/**
     * Returns the '<em><b>Field Type Enum</b></em>' literal with the specified integer value.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the integer value.
     * @return the matching enumerator or <code>null</code>.
     * @generated
     */
	public static FieldTypeEnum get(int value) {
        switch (value) {
            case SELECTABLE_VALUE: return SELECTABLE;
            case DATE_VALUE: return DATE;
            case TEXT_VALUE: return TEXT;
            case MAP_VALUE: return MAP;
            case TEXT_DECIMAL_VALUE: return TEXT_DECIMAL;
            case ZIP_VALUE: return ZIP;
            case TEXT_INTEGER_VALUE: return TEXT_INTEGER;
            case LINKED_OBJECT_VALUE: return LINKED_OBJECT;
            case LABEL_VALUE: return LABEL;
            case MONTH_VALUE: return MONTH;
            case ZIP_EXTENSION_VALUE: return ZIP_EXTENSION;
            case ROW_TEXT_VALUE: return ROW_TEXT;
        }
        return null;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private final int value;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private final String name;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private final String literal;

	/**
     * Only this class can construct instances.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private FieldTypeEnum(int value, String name, String literal) {
        this.value = value;
        this.name = name;
        this.literal = literal;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getValue() {
      return value;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getName() {
      return name;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getLiteral() {
      return literal;
    }

	/**
     * Returns the literal value of the enumerator, which is its string representation.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public String toString() {
        return literal;
    }
	
} //FieldTypeEnum
