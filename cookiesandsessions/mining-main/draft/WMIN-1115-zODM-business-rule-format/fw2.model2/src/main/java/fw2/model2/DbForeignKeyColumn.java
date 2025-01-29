/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Db Foreign Key Column</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.DbForeignKeyColumn#getKeySeq <em>Key Seq</em>}</li>
 *   <li>{@link fw2.model2.DbForeignKeyColumn#getSeq <em>Seq</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getDbForeignKeyColumn()
 * @model extendedMetaData="name='dbForeignKeyColumn' kind='elementOnly'"
 * @generated
 */
public interface DbForeignKeyColumn extends ModelElement {
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
     * @see fw2.model2.Model2Package#getDbForeignKeyColumn_KeySeq()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='keySeq' namespace='##targetNamespace'"
     * @generated
     */
	int getKeySeq();

	/**
     * Sets the value of the '{@link fw2.model2.DbForeignKeyColumn#getKeySeq <em>Key Seq</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Key Seq</em>' attribute.
     * @see #getKeySeq()
     * @generated
     */
	void setKeySeq(int value);

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
     * @see fw2.model2.Model2Package#getDbForeignKeyColumn_Seq()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Int"
     *        extendedMetaData="kind='attribute' name='seq' namespace='##targetNamespace'"
     * @generated
     */
	int getSeq();

	/**
     * Sets the value of the '{@link fw2.model2.DbForeignKeyColumn#getSeq <em>Seq</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Seq</em>' attribute.
     * @see #getSeq()
     * @generated
     */
	void setSeq(int value);

} // DbForeignKeyColumn
