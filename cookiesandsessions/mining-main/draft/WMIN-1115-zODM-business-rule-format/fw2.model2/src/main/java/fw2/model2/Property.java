/**
 */
package fw2.model2;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Property</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Property#getValue <em>Value</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getProperty()
 * @model extendedMetaData="name='property' kind='elementOnly'"
 * @generated
 */
public interface Property extends ModelElement {
	/**
     * Returns the value of the '<em><b>Value</b></em>' containment reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Value</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Value</em>' containment reference.
     * @see #setValue(EObject)
     * @see fw2.model2.Model2Package#getProperty_Value()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='value' namespace='##targetNamespace'"
     * @generated
     */
	EObject getValue();

	/**
     * Sets the value of the '{@link fw2.model2.Property#getValue <em>Value</em>}' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Value</em>' containment reference.
     * @see #getValue()
     * @generated
     */
	void setValue(EObject value);

} // Property
