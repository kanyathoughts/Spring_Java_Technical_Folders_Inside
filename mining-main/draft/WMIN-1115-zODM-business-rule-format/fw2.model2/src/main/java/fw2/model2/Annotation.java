/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Annotation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Annotation#getTags <em>Tags</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getAnnotation()
 * @model extendedMetaData="name='annotation' kind='elementOnly'"
 * @generated
 */
public interface Annotation extends ModelElement {
	/**
     * Returns the value of the '<em><b>Tags</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Tag}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Tags</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Tags</em>' containment reference list.
     * @see fw2.model2.Model2Package#getAnnotation_Tags()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='tags' namespace='##targetNamespace'"
     * @generated
     */
	EList<Tag> getTags();

} // Annotation
