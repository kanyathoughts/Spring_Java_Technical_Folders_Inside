/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Db Column</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.DbColumn#getDefault <em>Default</em>}</li>
 *   <li>{@link fw2.model2.DbColumn#getKeySeq <em>Key Seq</em>}</li>
 *   <li>{@link fw2.model2.DbColumn#getLength <em>Length</em>}</li>
 *   <li>{@link fw2.model2.DbColumn#getNullable <em>Nullable</em>}</li>
 *   <li>{@link fw2.model2.DbColumn#getScale <em>Scale</em>}</li>
 *   <li>{@link fw2.model2.DbColumn#getSeq <em>Seq</em>}</li>
 *   <li>{@link fw2.model2.DbColumn#getSize <em>Size</em>}</li>
 *   <li>{@link fw2.model2.DbColumn#getType <em>Type</em>}</li>
 *   <li>{@link fw2.model2.DbColumn#getTable <em>Table</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getDbColumn()
 * @model extendedMetaData="name='dbColumn' kind='elementOnly'"
 * @generated
 */
public interface DbColumn extends ModelElement {
	/**
     * Returns the value of the '<em><b>Default</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Default</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Default</em>' attribute.
     * @see #setDefault(String)
     * @see fw2.model2.Model2Package#getDbColumn_Default()
     * @model dataType="org.eclipse.emf.ecore.xml.type.String"
     *        extendedMetaData="kind='attribute' name='default' namespace='##targetNamespace'"
     * @generated
     */
	String getDefault();

	/**
     * Sets the value of the '{@link fw2.model2.DbColumn#getDefault <em>Default</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Default</em>' attribute.
     * @see #getDefault()
     * @generated
     */
	void setDefault(String value);

	/**
     * Returns the value of the '<em><b>Key Seq</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Key Seq</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Key Seq</em>' attribute.
     * @see #setKeySeq(int)
     * @see fw2.model2.Model2Package#getDbColumn_KeySeq()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='keySeq' namespace='##targetNamespace'"
     * @generated
     */
	int getKeySeq();

	/**
     * Sets the value of the '{@link fw2.model2.DbColumn#getKeySeq <em>Key Seq</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Key Seq</em>' attribute.
     * @see #getKeySeq()
     * @generated
     */
	void setKeySeq(int value);

	/**
     * Returns the value of the '<em><b>Length</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Length</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Length</em>' attribute.
     * @see #setLength(int)
     * @see fw2.model2.Model2Package#getDbColumn_Length()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='length' namespace='##targetNamespace'"
     * @generated
     */
	int getLength();

	/**
     * Sets the value of the '{@link fw2.model2.DbColumn#getLength <em>Length</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Length</em>' attribute.
     * @see #getLength()
     * @generated
     */
	void setLength(int value);

	/**
     * Returns the value of the '<em><b>Nullable</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Nullable</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Nullable</em>' attribute.
     * @see #setNullable(String)
     * @see fw2.model2.Model2Package#getDbColumn_Nullable()
     * @model extendedMetaData="kind='attribute' name='nullable' namespace='##targetNamespace'"
     * @generated
     */
	String getNullable();

	/**
     * Sets the value of the '{@link fw2.model2.DbColumn#getNullable <em>Nullable</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Nullable</em>' attribute.
     * @see #getNullable()
     * @generated
     */
	void setNullable(String value);

	/**
     * Returns the value of the '<em><b>Scale</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Scale</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Scale</em>' attribute.
     * @see #setScale(int)
     * @see fw2.model2.Model2Package#getDbColumn_Scale()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='scale' namespace='##targetNamespace'"
     * @generated
     */
	int getScale();

	/**
     * Sets the value of the '{@link fw2.model2.DbColumn#getScale <em>Scale</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Scale</em>' attribute.
     * @see #getScale()
     * @generated
     */
	void setScale(int value);

	/**
     * Returns the value of the '<em><b>Seq</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Seq</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Seq</em>' attribute.
     * @see #setSeq(int)
     * @see fw2.model2.Model2Package#getDbColumn_Seq()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='seq' namespace='##targetNamespace'"
     * @generated
     */
	int getSeq();

	/**
     * Sets the value of the '{@link fw2.model2.DbColumn#getSeq <em>Seq</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Seq</em>' attribute.
     * @see #getSeq()
     * @generated
     */
	void setSeq(int value);

	/**
     * Returns the value of the '<em><b>Size</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Size</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Size</em>' attribute.
     * @see #setSize(int)
     * @see fw2.model2.Model2Package#getDbColumn_Size()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='size' namespace='##targetNamespace'"
     * @generated
     */
	int getSize();

	/**
     * Sets the value of the '{@link fw2.model2.DbColumn#getSize <em>Size</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Size</em>' attribute.
     * @see #getSize()
     * @generated
     */
	void setSize(int value);

	/**
     * Returns the value of the '<em><b>Type</b></em>' attribute.
     * The literals are from the enumeration {@link fw2.model2.DbColumnTypeEnum}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Type</em>' attribute.
     * @see fw2.model2.DbColumnTypeEnum
     * @see #setType(DbColumnTypeEnum)
     * @see fw2.model2.Model2Package#getDbColumn_Type()
     * @model extendedMetaData="kind='attribute' name='type' namespace='##targetNamespace'"
     * @generated
     */
	DbColumnTypeEnum getType();

	/**
     * Sets the value of the '{@link fw2.model2.DbColumn#getType <em>Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Type</em>' attribute.
     * @see fw2.model2.DbColumnTypeEnum
     * @see #getType()
     * @generated
     */
	void setType(DbColumnTypeEnum value);

	/**
     * Returns the value of the '<em><b>Table</b></em>' container reference.
     * It is bidirectional and its opposite is '{@link fw2.model2.DbTable#getColumns <em>Columns</em>}'.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Table</em>' container reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Table</em>' container reference.
     * @see #setTable(DbTable)
     * @see fw2.model2.Model2Package#getDbColumn_Table()
     * @see fw2.model2.DbTable#getColumns
     * @model opposite="columns" required="true" transient="false"
     * @generated
     */
	DbTable getTable();

	/**
     * Sets the value of the '{@link fw2.model2.DbColumn#getTable <em>Table</em>}' container reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Table</em>' container reference.
     * @see #getTable()
     * @generated
     */
	void setTable(DbTable value);

} // DbColumn
