/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Aggregation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Aggregation#getComponent <em>Component</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getAggregation()
 * @model extendedMetaData="name='aggregation' kind='elementOnly'"
 * @generated
 */
public interface Aggregation extends Association {
	/**
     * Returns the value of the '<em><b>Component</b></em>' containment reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Component</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Component</em>' containment reference.
     * @see #setComponent(Component)
     * @see fw2.model2.Model2Package#getAggregation_Component()
     * @model containment="true" required="true"
     *        extendedMetaData="kind='element' name='component' namespace='##targetNamespace'"
     * @generated
     */
	Component getComponent();

	/**
     * Sets the value of the '{@link fw2.model2.Aggregation#getComponent <em>Component</em>}' containment reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Component</em>' containment reference.
     * @see #getComponent()
     * @generated
     */
	void setComponent(Component value);

} // Aggregation
