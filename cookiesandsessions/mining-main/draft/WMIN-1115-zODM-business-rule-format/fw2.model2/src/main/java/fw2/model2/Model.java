/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Model</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Model#getElements <em>Elements</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getModel()
 * @model extendedMetaData="name='model' kind='elementOnly'"
 * @generated
 */
public interface Model extends ModelElement {
	/**
     * Returns the value of the '<em><b>Elements</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.ModelElement}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Elements</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Elements</em>' containment reference list.
     * @see fw2.model2.Model2Package#getModel_Elements()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='elements' namespace='##targetNamespace'"
     * @generated
     */
	EList<ModelElement> getElements();

} // Model
