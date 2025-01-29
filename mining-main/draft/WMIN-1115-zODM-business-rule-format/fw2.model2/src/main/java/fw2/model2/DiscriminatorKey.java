/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Discriminator Key</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.DiscriminatorKey#getDiscriminatorValue <em>Discriminator Value</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getDiscriminatorKey()
 * @model extendedMetaData="name='discriminatorKey' kind='elementOnly'"
 * @generated
 */
public interface DiscriminatorKey extends ModelElement {
	/**
     * Returns the value of the '<em><b>Discriminator Value</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Discriminator Value</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Discriminator Value</em>' attribute.
     * @see #setDiscriminatorValue(String)
     * @see fw2.model2.Model2Package#getDiscriminatorKey_DiscriminatorValue()
     * @model dataType="org.eclipse.emf.ecore.xml.type.String"
     *        extendedMetaData="kind='element' name='discriminatorValue' namespace='##targetNamespace'"
     * @generated
     */
	String getDiscriminatorValue();

	/**
     * Sets the value of the '{@link fw2.model2.DiscriminatorKey#getDiscriminatorValue <em>Discriminator Value</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Discriminator Value</em>' attribute.
     * @see #getDiscriminatorValue()
     * @generated
     */
	void setDiscriminatorValue(String value);

} // DiscriminatorKey
