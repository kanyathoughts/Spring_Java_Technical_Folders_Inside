/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Application</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Application#getBaps <em>Baps</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getApplication()
 * @model extendedMetaData="name='application' kind='elementOnly'"
 * @generated
 */
public interface Application extends ViewComponent {
	/**
     * Returns the value of the '<em><b>Baps</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Bap}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Baps</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Baps</em>' containment reference list.
     * @see fw2.model2.Model2Package#getApplication_Baps()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='baps' namespace='##targetNamespace'"
     * @generated
     */
	EList<Bap> getBaps();

} // Application
