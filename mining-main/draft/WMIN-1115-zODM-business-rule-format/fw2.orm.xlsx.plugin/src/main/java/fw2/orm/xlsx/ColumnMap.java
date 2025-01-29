/**
 */
package fw2.orm.xlsx;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Column Map</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.ColumnMap#getColumnName <em>Column Name</em>}</li>
 *   <li>{@link fw2.orm.xlsx.ColumnMap#getColumnType <em>Column Type</em>}</li>
 *   <li>{@link fw2.orm.xlsx.ColumnMap#getDefaultValue <em>Default Value</em>}</li>
 *   <li>{@link fw2.orm.xlsx.ColumnMap#isNullable <em>Nullable</em>}</li>
 *   <li>{@link fw2.orm.xlsx.ColumnMap#isPrimaryKey <em>Primary Key</em>}</li>
 * </ul>
 *
 * @see fw2.orm.xlsx.XlsxPackage#getColumnMap()
 * @model
 * @generated
 */
public interface ColumnMap extends MapElement {
	/**
	 * Returns the value of the '<em><b>Column Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Column Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Column Name</em>' attribute.
	 * @see #setColumnName(String)
	 * @see fw2.orm.xlsx.XlsxPackage#getColumnMap_ColumnName()
	 * @model
	 * @generated
	 */
	String getColumnName();

	/**
	 * Sets the value of the '{@link fw2.orm.xlsx.ColumnMap#getColumnName <em>Column Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Column Name</em>' attribute.
	 * @see #getColumnName()
	 * @generated
	 */
	void setColumnName(String value);

	/**
	 * Returns the value of the '<em><b>Column Type</b></em>' attribute.
	 * The literals are from the enumeration {@link fw2.orm.xlsx.ColumnTypeEnum}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Column Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Column Type</em>' attribute.
	 * @see fw2.orm.xlsx.ColumnTypeEnum
	 * @see #setColumnType(ColumnTypeEnum)
	 * @see fw2.orm.xlsx.XlsxPackage#getColumnMap_ColumnType()
	 * @model
	 * @generated
	 */
	ColumnTypeEnum getColumnType();

	/**
	 * Sets the value of the '{@link fw2.orm.xlsx.ColumnMap#getColumnType <em>Column Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Column Type</em>' attribute.
	 * @see fw2.orm.xlsx.ColumnTypeEnum
	 * @see #getColumnType()
	 * @generated
	 */
	void setColumnType(ColumnTypeEnum value);

	/**
	 * Returns the value of the '<em><b>Default Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Default Value</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Default Value</em>' attribute.
	 * @see #setDefaultValue(String)
	 * @see fw2.orm.xlsx.XlsxPackage#getColumnMap_DefaultValue()
	 * @model
	 * @generated
	 */
	String getDefaultValue();

	/**
	 * Sets the value of the '{@link fw2.orm.xlsx.ColumnMap#getDefaultValue <em>Default Value</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Default Value</em>' attribute.
	 * @see #getDefaultValue()
	 * @generated
	 */
	void setDefaultValue(String value);

	/**
	 * Returns the value of the '<em><b>Nullable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Nullable</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Nullable</em>' attribute.
	 * @see #setNullable(boolean)
	 * @see fw2.orm.xlsx.XlsxPackage#getColumnMap_Nullable()
	 * @model
	 * @generated
	 */
	boolean isNullable();

	/**
	 * Sets the value of the '{@link fw2.orm.xlsx.ColumnMap#isNullable <em>Nullable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Nullable</em>' attribute.
	 * @see #isNullable()
	 * @generated
	 */
	void setNullable(boolean value);

	/**
	 * Returns the value of the '<em><b>Primary Key</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Primary Key</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Primary Key</em>' attribute.
	 * @see #setPrimaryKey(boolean)
	 * @see fw2.orm.xlsx.XlsxPackage#getColumnMap_PrimaryKey()
	 * @model
	 * @generated
	 */
	boolean isPrimaryKey();

	/**
	 * Sets the value of the '{@link fw2.orm.xlsx.ColumnMap#isPrimaryKey <em>Primary Key</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Primary Key</em>' attribute.
	 * @see #isPrimaryKey()
	 * @generated
	 */
	void setPrimaryKey(boolean value);

} // ColumnMap
