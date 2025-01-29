/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Type Extension</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.TypeExtension#getDomainClass <em>Domain Class</em>}</li>
 *   <li>{@link fw2.model2.TypeExtension#getProperties <em>Properties</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getTypeExtension()
 * @model extendedMetaData="name='typeExtension' kind='elementOnly'"
 * @generated
 */
public interface TypeExtension extends ModelElement {
	/**
     * Returns the value of the '<em><b>Domain Class</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Domain Class</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Domain Class</em>' reference.
     * @see #setDomainClass(DomainClass)
     * @see fw2.model2.Model2Package#getTypeExtension_DomainClass()
     * @model resolveProxies="false" required="true"
     *        extendedMetaData="kind='element' name='domainClass' namespace='##targetNamespace'"
     * @generated
     */
	DomainClass getDomainClass();

	/**
     * Sets the value of the '{@link fw2.model2.TypeExtension#getDomainClass <em>Domain Class</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Domain Class</em>' reference.
     * @see #getDomainClass()
     * @generated
     */
	void setDomainClass(DomainClass value);

	/**
     * Returns the value of the '<em><b>Properties</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Property}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Properties</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Properties</em>' containment reference list.
     * @see fw2.model2.Model2Package#getTypeExtension_Properties()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='properties' namespace='##targetNamespace'"
     * @generated
     */
	EList<Property> getProperties();

} // TypeExtension
