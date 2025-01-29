/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Bap</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Bap#getComponentMapping <em>Component Mapping</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getBap()
 * @model extendedMetaData="name='bap' kind='elementOnly'"
 * @generated
 */
public interface Bap extends Editor {

	/**
     * Returns the value of the '<em><b>Component Mapping</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.ComponentMapping}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Component Mapping</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Component Mapping</em>' containment reference list.
     * @see fw2.model2.Model2Package#getBap_ComponentMapping()
     * @model containment="true"
     * @generated
     */
	EList<ComponentMapping> getComponentMapping();
} // Bap
