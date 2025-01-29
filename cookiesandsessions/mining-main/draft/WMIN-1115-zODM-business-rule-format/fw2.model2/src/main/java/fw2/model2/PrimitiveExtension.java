/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Primitive Extension</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.PrimitiveExtension#getExtension <em>Extension</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getPrimitiveExtension()
 * @model extendedMetaData="name='primitiveExtension' kind='elementOnly'"
 * @generated
 */
public interface PrimitiveExtension extends Primitive {
	/**
     * Returns the value of the '<em><b>Extension</b></em>' containment reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Extension</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Extension</em>' containment reference.
     * @see #setExtension(TypeExtension)
     * @see fw2.model2.Model2Package#getPrimitiveExtension_Extension()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='extension' namespace='##targetNamespace'"
     * @generated
     */
	TypeExtension getExtension();

	/**
     * Sets the value of the '{@link fw2.model2.PrimitiveExtension#getExtension <em>Extension</em>}' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Extension</em>' containment reference.
     * @see #getExtension()
     * @generated
     */
	void setExtension(TypeExtension value);

} // PrimitiveExtension
