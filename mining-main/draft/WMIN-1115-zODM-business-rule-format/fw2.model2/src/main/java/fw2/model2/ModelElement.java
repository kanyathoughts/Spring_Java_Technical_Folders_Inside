/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Model Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.ModelElement#getAnnotations <em>Annotations</em>}</li>
 *   <li>{@link fw2.model2.ModelElement#getName <em>Name</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getModelElement()
 * @model abstract="true"
 *        extendedMetaData="name='modelElement' kind='elementOnly'"
 * @generated
 */
public interface ModelElement extends EObject {
	/**
     * Returns the value of the '<em><b>Annotations</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Annotation}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Annotations</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Annotations</em>' containment reference list.
     * @see fw2.model2.Model2Package#getModelElement_Annotations()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='annotations' namespace='##targetNamespace'"
     * @generated
     */
	EList<Annotation> getAnnotations();

	/**
     * Returns the value of the '<em><b>Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Name</em>' attribute.
     * @see #setName(String)
     * @see fw2.model2.Model2Package#getModelElement_Name()
     * @model dataType="org.eclipse.emf.ecore.xml.type.String"
     *        extendedMetaData="kind='attribute' name='name' namespace='##targetNamespace'"
     * @generated
     */
	String getName();

	/**
     * Sets the value of the '{@link fw2.model2.ModelElement#getName <em>Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Name</em>' attribute.
     * @see #getName()
     * @generated
     */
	void setName(String value);

} // ModelElement
