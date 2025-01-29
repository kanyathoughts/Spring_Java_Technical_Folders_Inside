/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Field Extension</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.FieldExtension#getExtension <em>Extension</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getFieldExtension()
 * @model extendedMetaData="name='fieldExtension' kind='elementOnly'"
 * @generated
 */
public interface FieldExtension extends Field {
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
     * @see fw2.model2.Model2Package#getFieldExtension_Extension()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='extension' namespace='##targetNamespace'"
     * @generated
     */
	TypeExtension getExtension();

	/**
     * Sets the value of the '{@link fw2.model2.FieldExtension#getExtension <em>Extension</em>}' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Extension</em>' containment reference.
     * @see #getExtension()
     * @generated
     */
	void setExtension(TypeExtension value);

} // FieldExtension
