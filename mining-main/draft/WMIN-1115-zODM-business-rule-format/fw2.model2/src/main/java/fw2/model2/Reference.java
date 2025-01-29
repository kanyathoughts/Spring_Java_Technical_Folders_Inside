/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Reference</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Reference#getRelation <em>Relation</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getReference()
 * @model extendedMetaData="name='reference' kind='elementOnly'"
 * @generated
 */
public interface Reference extends Association {
	/**
     * Returns the value of the '<em><b>Relation</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Relation</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Relation</em>' reference.
     * @see #setRelation(DbRelation)
     * @see fw2.model2.Model2Package#getReference_Relation()
     * @model resolveProxies="false"
     *        extendedMetaData="kind='element' name='relation' namespace='##targetNamespace'"
     * @generated
     */
	DbRelation getRelation();

	/**
     * Sets the value of the '{@link fw2.model2.Reference#getRelation <em>Relation</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Relation</em>' reference.
     * @see #getRelation()
     * @generated
     */
	void setRelation(DbRelation value);

} // Reference
