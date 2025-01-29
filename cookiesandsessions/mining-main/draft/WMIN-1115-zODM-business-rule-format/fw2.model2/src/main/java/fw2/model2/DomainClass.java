/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Domain Class</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.DomainClass#getSuperclass <em>Superclass</em>}</li>
 *   <li>{@link fw2.model2.DomainClass#getPlatformClass <em>Platform Class</em>}</li>
 *   <li>{@link fw2.model2.DomainClass#getAttributes <em>Attributes</em>}</li>
 *   <li>{@link fw2.model2.DomainClass#isAbstract <em>Abstract</em>}</li>
 *   <li>{@link fw2.model2.DomainClass#isPrimitive <em>Primitive</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getDomainClass()
 * @model extendedMetaData="name='domainClass' kind='elementOnly'"
 * @generated
 */
public interface DomainClass extends ModelElement {
	/**
     * Returns the value of the '<em><b>Superclass</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Superclass</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Superclass</em>' reference.
     * @see #setSuperclass(DomainClass)
     * @see fw2.model2.Model2Package#getDomainClass_Superclass()
     * @model resolveProxies="false" required="true"
     *        extendedMetaData="kind='element' name='superclass' namespace='##targetNamespace'"
     * @generated
     */
	DomainClass getSuperclass();

	/**
     * Sets the value of the '{@link fw2.model2.DomainClass#getSuperclass <em>Superclass</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Superclass</em>' reference.
     * @see #getSuperclass()
     * @generated
     */
	void setSuperclass(DomainClass value);

	/**
     * Returns the value of the '<em><b>Platform Class</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Platform Class</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Platform Class</em>' reference.
     * @see #setPlatformClass(DomainClass)
     * @see fw2.model2.Model2Package#getDomainClass_PlatformClass()
     * @model resolveProxies="false" required="true"
     *        extendedMetaData="kind='element' name='platformClass' namespace='##targetNamespace'"
     * @generated
     */
	DomainClass getPlatformClass();

	/**
     * Sets the value of the '{@link fw2.model2.DomainClass#getPlatformClass <em>Platform Class</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Platform Class</em>' reference.
     * @see #getPlatformClass()
     * @generated
     */
	void setPlatformClass(DomainClass value);

	/**
     * Returns the value of the '<em><b>Attributes</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.DomainAttribute}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Attributes</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Attributes</em>' containment reference list.
     * @see fw2.model2.Model2Package#getDomainClass_Attributes()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='attributes' namespace='##targetNamespace'"
     * @generated
     */
	EList<DomainAttribute> getAttributes();

	/**
     * Returns the value of the '<em><b>Abstract</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Abstract</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Abstract</em>' attribute.
     * @see #setAbstract(boolean)
     * @see fw2.model2.Model2Package#getDomainClass_Abstract()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Boolean"
     *        extendedMetaData="kind='attribute' name='abstract' namespace='##targetNamespace'"
     * @generated
     */
	boolean isAbstract();

	/**
     * Sets the value of the '{@link fw2.model2.DomainClass#isAbstract <em>Abstract</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Abstract</em>' attribute.
     * @see #isAbstract()
     * @generated
     */
	void setAbstract(boolean value);

	/**
     * Returns the value of the '<em><b>Primitive</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Primitive</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Primitive</em>' attribute.
     * @see #setPrimitive(boolean)
     * @see fw2.model2.Model2Package#getDomainClass_Primitive()
     * @model dataType="org.eclipse.emf.ecore.xml.type.Boolean"
     *        extendedMetaData="kind='attribute' name='primitive' namespace='##targetNamespace'"
     * @generated
     */
	boolean isPrimitive();

	/**
     * Sets the value of the '{@link fw2.model2.DomainClass#isPrimitive <em>Primitive</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Primitive</em>' attribute.
     * @see #isPrimitive()
     * @generated
     */
	void setPrimitive(boolean value);

} // DomainClass
