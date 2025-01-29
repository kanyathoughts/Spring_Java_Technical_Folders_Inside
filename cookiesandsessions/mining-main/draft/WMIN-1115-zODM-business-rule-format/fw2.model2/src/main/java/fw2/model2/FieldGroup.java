/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Field Group</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.FieldGroup#getActions <em>Actions</em>}</li>
 *   <li>{@link fw2.model2.FieldGroup#getFields <em>Fields</em>}</li>
 *   <li>{@link fw2.model2.FieldGroup#getDisplayOrder <em>Display Order</em>}</li>
 *   <li>{@link fw2.model2.FieldGroup#getPresentInStaging <em>Present In Staging</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getFieldGroup()
 * @model extendedMetaData="name='fieldGroup' kind='elementOnly'"
 * @generated
 */
public interface FieldGroup extends ViewComponent {
	/**
     * Returns the value of the '<em><b>Actions</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Action}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Actions</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Actions</em>' containment reference list.
     * @see fw2.model2.Model2Package#getFieldGroup_Actions()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='actions' namespace='##targetNamespace'"
     * @generated
     */
	EList<Action> getActions();

	/**
     * Returns the value of the '<em><b>Fields</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Field}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Fields</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Fields</em>' containment reference list.
     * @see fw2.model2.Model2Package#getFieldGroup_Fields()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='fields' namespace='##targetNamespace'"
     * @generated
     */
	EList<Field> getFields();

	/**
     * Returns the value of the '<em><b>Display Order</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Display Order</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Display Order</em>' attribute.
     * @see #setDisplayOrder(int)
     * @see fw2.model2.Model2Package#getFieldGroup_DisplayOrder()
     * @model
     * @generated
     */
	int getDisplayOrder();

	/**
     * Sets the value of the '{@link fw2.model2.FieldGroup#getDisplayOrder <em>Display Order</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Display Order</em>' attribute.
     * @see #getDisplayOrder()
     * @generated
     */
	void setDisplayOrder(int value);

	/**
     * Returns the value of the '<em><b>Present In Staging</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Present In Staging</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Present In Staging</em>' attribute.
     * @see #setPresentInStaging(String)
     * @see fw2.model2.Model2Package#getFieldGroup_PresentInStaging()
     * @model
     * @generated
     */
	String getPresentInStaging();

	/**
     * Sets the value of the '{@link fw2.model2.FieldGroup#getPresentInStaging <em>Present In Staging</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Present In Staging</em>' attribute.
     * @see #getPresentInStaging()
     * @generated
     */
	void setPresentInStaging(String value);

} // FieldGroup
