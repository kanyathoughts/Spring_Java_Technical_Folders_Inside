/**
 */
package fw2.model2;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Db Column Type Enum</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see fw2.model2.Model2Package#getDbColumnTypeEnum()
 * @model extendedMetaData="name='dbColumnTypeEnum'"
 * @generated
 */
public enum DbColumnTypeEnum implements Enumerator {
	/**
     * The '<em><b>BIGINT</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #BIGINT_VALUE
     * @generated
     * @ordered
     */
	BIGINT(0, "BIGINT", "BIGINT"),

	/**
     * The '<em><b>BLOB</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #BLOB_VALUE
     * @generated
     * @ordered
     */
	BLOB(1, "BLOB", "BLOB"),

	/**
     * The '<em><b>CHAR</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #CHAR_VALUE
     * @generated
     * @ordered
     */
	CHAR(2, "CHAR", "CHAR"),

	/**
     * The '<em><b>CHARACTER</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #CHARACTER_VALUE
     * @generated
     * @ordered
     */
	CHARACTER(3, "CHARACTER", "CHARACTER"),

	/**
     * The '<em><b>DATE</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #DATE_VALUE
     * @generated
     * @ordered
     */
	DATE(4, "DATE", "DATE"),

	/**
     * The '<em><b>INTEGER</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #INTEGER_VALUE
     * @generated
     * @ordered
     */
	INTEGER(5, "INTEGER", "INTEGER"),

	/**
     * The '<em><b>SHORT</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #SHORT_VALUE
     * @generated
     * @ordered
     */
	SHORT(6, "SHORT", "SHORT"),

	/**
     * The '<em><b>SMALLINT</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #SMALLINT_VALUE
     * @generated
     * @ordered
     */
	SMALLINT(7, "SMALLINT", "SMALLINT"),

	/**
     * The '<em><b>TIME</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #TIME_VALUE
     * @generated
     * @ordered
     */
	TIME(8, "TIME", "TIME"),

	/**
     * The '<em><b>TIMESTAMP</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #TIMESTAMP_VALUE
     * @generated
     * @ordered
     */
	TIMESTAMP(9, "TIMESTAMP", "TIMESTAMP"),

	/**
     * The '<em><b>VARCHAR</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #VARCHAR_VALUE
     * @generated
     * @ordered
     */
	VARCHAR(10, "VARCHAR", "VARCHAR"),

	/**
     * The '<em><b>DECIMAL</b></em>' literal object.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #DECIMAL_VALUE
     * @generated
     * @ordered
     */
	DECIMAL(11, "DECIMAL", "DECIMAL");

	/**
     * The '<em><b>BIGINT</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>BIGINT</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #BIGINT
     * @model
     * @generated
     * @ordered
     */
	public static final int BIGINT_VALUE = 0;

	/**
     * The '<em><b>BLOB</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>BLOB</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #BLOB
     * @model
     * @generated
     * @ordered
     */
	public static final int BLOB_VALUE = 1;

	/**
     * The '<em><b>CHAR</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>CHAR</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #CHAR
     * @model
     * @generated
     * @ordered
     */
	public static final int CHAR_VALUE = 2;

	/**
     * The '<em><b>CHARACTER</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>CHARACTER</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #CHARACTER
     * @model
     * @generated
     * @ordered
     */
	public static final int CHARACTER_VALUE = 3;

	/**
     * The '<em><b>DATE</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>DATE</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #DATE
     * @model
     * @generated
     * @ordered
     */
	public static final int DATE_VALUE = 4;

	/**
     * The '<em><b>INTEGER</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>INTEGER</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #INTEGER
     * @model
     * @generated
     * @ordered
     */
	public static final int INTEGER_VALUE = 5;

	/**
     * The '<em><b>SHORT</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>SHORT</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #SHORT
     * @model
     * @generated
     * @ordered
     */
	public static final int SHORT_VALUE = 6;

	/**
     * The '<em><b>SMALLINT</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>SMALLINT</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #SMALLINT
     * @model
     * @generated
     * @ordered
     */
	public static final int SMALLINT_VALUE = 7;

	/**
     * The '<em><b>TIME</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>TIME</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #TIME
     * @model
     * @generated
     * @ordered
     */
	public static final int TIME_VALUE = 8;

	/**
     * The '<em><b>TIMESTAMP</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>TIMESTAMP</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #TIMESTAMP
     * @model
     * @generated
     * @ordered
     */
	public static final int TIMESTAMP_VALUE = 9;

	/**
     * The '<em><b>VARCHAR</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>VARCHAR</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #VARCHAR
     * @model
     * @generated
     * @ordered
     */
	public static final int VARCHAR_VALUE = 10;

	/**
     * The '<em><b>DECIMAL</b></em>' literal value.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>DECIMAL</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @see #DECIMAL
     * @model
     * @generated
     * @ordered
     */
	public static final int DECIMAL_VALUE = 11;

	/**
     * An array of all the '<em><b>Db Column Type Enum</b></em>' enumerators.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private static final DbColumnTypeEnum[] VALUES_ARRAY =
		new DbColumnTypeEnum[] {
            BIGINT,
            BLOB,
            CHAR,
            CHARACTER,
            DATE,
            INTEGER,
            SHORT,
            SMALLINT,
            TIME,
            TIMESTAMP,
            VARCHAR,
            DECIMAL,
        };

	/**
     * A public read-only list of all the '<em><b>Db Column Type Enum</b></em>' enumerators.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public static final List<DbColumnTypeEnum> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
     * Returns the '<em><b>Db Column Type Enum</b></em>' literal with the specified literal value.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param literal the literal.
     * @return the matching enumerator or <code>null</code>.
     * @generated
     */
	public static DbColumnTypeEnum get(String literal) {
        for (int i = 0; i < VALUES_ARRAY.length; ++i) {
            DbColumnTypeEnum result = VALUES_ARRAY[i];
            if (result.toString().equals(literal)) {
                return result;
            }
        }
        return null;
    }

	/**
     * Returns the '<em><b>Db Column Type Enum</b></em>' literal with the specified name.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param name the name.
     * @return the matching enumerator or <code>null</code>.
     * @generated
     */
	public static DbColumnTypeEnum getByName(String name) {
        for (int i = 0; i < VALUES_ARRAY.length; ++i) {
            DbColumnTypeEnum result = VALUES_ARRAY[i];
            if (result.getName().equals(name)) {
                return result;
            }
        }
        return null;
    }

	/**
     * Returns the '<em><b>Db Column Type Enum</b></em>' literal with the specified integer value.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the integer value.
     * @return the matching enumerator or <code>null</code>.
     * @generated
     */
	public static DbColumnTypeEnum get(int value) {
        switch (value) {
            case BIGINT_VALUE: return BIGINT;
            case BLOB_VALUE: return BLOB;
            case CHAR_VALUE: return CHAR;
            case CHARACTER_VALUE: return CHARACTER;
            case DATE_VALUE: return DATE;
            case INTEGER_VALUE: return INTEGER;
            case SHORT_VALUE: return SHORT;
            case SMALLINT_VALUE: return SMALLINT;
            case TIME_VALUE: return TIME;
            case TIMESTAMP_VALUE: return TIMESTAMP;
            case VARCHAR_VALUE: return VARCHAR;
            case DECIMAL_VALUE: return DECIMAL;
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
	private DbColumnTypeEnum(int value, String name, String literal) {
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
	
} //DbColumnTypeEnum
