/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Domain Attribute</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.DomainAttribute#getDataType <em>Data Type</em>}</li>
 *   <li>{@link fw2.model2.DomainAttribute#isMultivalued <em>Multivalued</em>}</li>
 *   <li>{@link fw2.model2.DomainAttribute#isReference <em>Reference</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getDomainAttribute()
 * @model extendedMetaData="name='domainAttribute' kind='elementOnly'"
 * @generated
 */
public interface DomainAttribute extends ModelElement {
	/**
     * Returns the value of the '<em><b>Data Type</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Data Type</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Data Type</em>' reference.
     * @see #setDataType(DomainClass)
     * @see fw2.model2.Model2Package#getDomainAttribute_DataType()
     * @model resolveProxies="false" required="true"
     *        extendedMetaData="kind='element' name='dataType' namespace='##targetNamespace'"
     * @generated
     */
	DomainClass getDataType();

	/**
     * Sets the value of the '{@link fw2.model2.DomainAttribute#getDataType <em>Data Type</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Data Type</em>' reference.
     * @see #getDataType()
     * @generated
     */
	void setDataType(DomainClass value);

	/**
     * Returns the value of the '<em><b>Multivalued</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Multivalued</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Multivalued</em>' attribute.
     * @see #setMultivalued(boolean)
     * @see fw2.model2.Model2Package#getDomainAttribute_Multivalued()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Boolean"
     *        extendedMetaData="kind='attribute' name='multivalued' namespace='##targetNamespace'"
     * @generated
     */
	boolean isMultivalued();

	/**
     * Sets the value of the '{@link fw2.model2.DomainAttribute#isMultivalued <em>Multivalued</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Multivalued</em>' attribute.
     * @see #isMultivalued()
     * @generated
     */
	void setMultivalued(boolean value);

	/**
     * Returns the value of the '<em><b>Reference</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Reference</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Reference</em>' attribute.
     * @see #setReference(boolean)
     * @see fw2.model2.Model2Package#getDomainAttribute_Reference()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Boolean"
     *        extendedMetaData="kind='attribute' name='reference' namespace='##targetNamespace'"
     * @generated
     */
	boolean isReference();

	/**
     * Sets the value of the '{@link fw2.model2.DomainAttribute#isReference <em>Reference</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Reference</em>' attribute.
     * @see #isReference()
     * @generated
     */
	void setReference(boolean value);

} // DomainAttribute
