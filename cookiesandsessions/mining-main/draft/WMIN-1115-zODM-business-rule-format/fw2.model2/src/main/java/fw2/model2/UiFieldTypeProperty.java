/**
 */
package fw2.model2;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Ui Field Type Property</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.UiFieldTypeProperty#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link fw2.model2.UiFieldTypeProperty#getPropertyType <em>Property Type</em>}</li>
 *   <li>{@link fw2.model2.UiFieldTypeProperty#getTypeName <em>Type Name</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getUiFieldTypeProperty()
 * @model
 * @generated
 */
public interface UiFieldTypeProperty extends EObject {
	/**
     * Returns the value of the '<em><b>Property Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Property Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Property Name</em>' attribute.
     * @see #setPropertyName(String)
     * @see fw2.model2.Model2Package#getUiFieldTypeProperty_PropertyName()
     * @model
     * @generated
     */
	String getPropertyName();

	/**
     * Sets the value of the '{@link fw2.model2.UiFieldTypeProperty#getPropertyName <em>Property Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Property Name</em>' attribute.
     * @see #getPropertyName()
     * @generated
     */
	void setPropertyName(String value);

	/**
     * Returns the value of the '<em><b>Property Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Property Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Property Type</em>' attribute.
     * @see #setPropertyType(String)
     * @see fw2.model2.Model2Package#getUiFieldTypeProperty_PropertyType()
     * @model
     * @generated
     */
	String getPropertyType();

	/**
     * Sets the value of the '{@link fw2.model2.UiFieldTypeProperty#getPropertyType <em>Property Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Property Type</em>' attribute.
     * @see #getPropertyType()
     * @generated
     */
	void setPropertyType(String value);

	/**
     * Returns the value of the '<em><b>Type Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Type Name</em>' attribute.
     * @see #setTypeName(String)
     * @see fw2.model2.Model2Package#getUiFieldTypeProperty_TypeName()
     * @model
     * @generated
     */
	String getTypeName();

	/**
     * Sets the value of the '{@link fw2.model2.UiFieldTypeProperty#getTypeName <em>Type Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Type Name</em>' attribute.
     * @see #getTypeName()
     * @generated
     */
	void setTypeName(String value);

} // UiFieldTypeProperty
