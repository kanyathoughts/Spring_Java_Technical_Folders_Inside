/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Ui Field Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.UiFieldType#getDataType <em>Data Type</em>}</li>
 *   <li>{@link fw2.model2.UiFieldType#getHtmlType <em>Html Type</em>}</li>
 *   <li>{@link fw2.model2.UiFieldType#getOnScreenValidationMethod <em>On Screen Validation Method</em>}</li>
 *   <li>{@link fw2.model2.UiFieldType#getProperties <em>Properties</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getUiFieldType()
 * @model
 * @generated
 */
public interface UiFieldType extends EObject {
	/**
     * Returns the value of the '<em><b>Data Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Data Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Data Type</em>' attribute.
     * @see #setDataType(String)
     * @see fw2.model2.Model2Package#getUiFieldType_DataType()
     * @model
     * @generated
     */
	String getDataType();

	/**
     * Sets the value of the '{@link fw2.model2.UiFieldType#getDataType <em>Data Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Data Type</em>' attribute.
     * @see #getDataType()
     * @generated
     */
	void setDataType(String value);

	/**
     * Returns the value of the '<em><b>Html Type</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Html Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Html Type</em>' attribute.
     * @see #setHtmlType(String)
     * @see fw2.model2.Model2Package#getUiFieldType_HtmlType()
     * @model
     * @generated
     */
	String getHtmlType();

	/**
     * Sets the value of the '{@link fw2.model2.UiFieldType#getHtmlType <em>Html Type</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Html Type</em>' attribute.
     * @see #getHtmlType()
     * @generated
     */
	void setHtmlType(String value);

	/**
     * Returns the value of the '<em><b>On Screen Validation Method</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>On Screen Validation Method</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>On Screen Validation Method</em>' attribute.
     * @see #setOnScreenValidationMethod(String)
     * @see fw2.model2.Model2Package#getUiFieldType_OnScreenValidationMethod()
     * @model
     * @generated
     */
	String getOnScreenValidationMethod();

	/**
     * Sets the value of the '{@link fw2.model2.UiFieldType#getOnScreenValidationMethod <em>On Screen Validation Method</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>On Screen Validation Method</em>' attribute.
     * @see #getOnScreenValidationMethod()
     * @generated
     */
	void setOnScreenValidationMethod(String value);

	/**
     * Returns the value of the '<em><b>Properties</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.UiFieldTypeProperty}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Properties</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Properties</em>' containment reference list.
     * @see fw2.model2.Model2Package#getUiFieldType_Properties()
     * @model containment="true"
     * @generated
     */
	EList<UiFieldTypeProperty> getProperties();

} // UiFieldType
