/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Association</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Association#getCardinality <em>Cardinality</em>}</li>
 *   <li>{@link fw2.model2.Association#getRelatedClassOrm <em>Related Class Orm</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getAssociation()
 * @model abstract="true"
 *        extendedMetaData="name='association' kind='elementOnly'"
 * @generated
 */
public interface Association extends Attribute {
	/**
     * Returns the value of the '<em><b>Cardinality</b></em>' attribute.
     * The literals are from the enumeration {@link fw2.model2.CardinalityTypeEnum}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Cardinality</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Cardinality</em>' attribute.
     * @see fw2.model2.CardinalityTypeEnum
     * @see #setCardinality(CardinalityTypeEnum)
     * @see fw2.model2.Model2Package#getAssociation_Cardinality()
     * @model extendedMetaData="kind='attribute' name='cardinality' namespace='##targetNamespace'"
     * @generated
     */
	CardinalityTypeEnum getCardinality();

	/**
     * Sets the value of the '{@link fw2.model2.Association#getCardinality <em>Cardinality</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Cardinality</em>' attribute.
     * @see fw2.model2.CardinalityTypeEnum
     * @see #getCardinality()
     * @generated
     */
	void setCardinality(CardinalityTypeEnum value);

	/**
     * Returns the value of the '<em><b>Related Class Orm</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Related Class Orm</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Related Class Orm</em>' reference.
     * @see #setRelatedClassOrm(ClassOrm)
     * @see fw2.model2.Model2Package#getAssociation_RelatedClassOrm()
     * @model
     * @generated
     */
	ClassOrm getRelatedClassOrm();

	/**
     * Sets the value of the '{@link fw2.model2.Association#getRelatedClassOrm <em>Related Class Orm</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Related Class Orm</em>' reference.
     * @see #getRelatedClassOrm()
     * @generated
     */
	void setRelatedClassOrm(ClassOrm value);

} // Association
