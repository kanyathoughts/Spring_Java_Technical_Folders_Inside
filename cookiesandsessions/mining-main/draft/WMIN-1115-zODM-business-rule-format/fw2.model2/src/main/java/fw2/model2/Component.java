/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Component</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Component#getLookups <em>Lookups</em>}</li>
 *   <li>{@link fw2.model2.Component#getPrimitives <em>Primitives</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getComponent()
 * @model extendedMetaData="name='component' kind='elementOnly'"
 * @generated
 */
public interface Component extends ModelElement {
	/**
     * Returns the value of the '<em><b>Lookups</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Lookup}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Lookups</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Lookups</em>' containment reference list.
     * @see fw2.model2.Model2Package#getComponent_Lookups()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='lookups' namespace='##targetNamespace'"
     * @generated
     */
	EList<Lookup> getLookups();

	/**
     * Returns the value of the '<em><b>Primitives</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Primitive}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Primitives</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Primitives</em>' containment reference list.
     * @see fw2.model2.Model2Package#getComponent_Primitives()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='primitives' namespace='##targetNamespace'"
     * @generated
     */
	EList<Primitive> getPrimitives();

} // Component
