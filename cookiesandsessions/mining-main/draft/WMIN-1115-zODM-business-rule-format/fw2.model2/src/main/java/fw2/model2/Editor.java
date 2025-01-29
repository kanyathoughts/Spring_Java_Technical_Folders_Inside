/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Editor</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Editor#getEditors <em>Editors</em>}</li>
 *   <li>{@link fw2.model2.Editor#getFieldGroups <em>Field Groups</em>}</li>
 *   <li>{@link fw2.model2.Editor#getSummaryTables <em>Summary Tables</em>}</li>
 *   <li>{@link fw2.model2.Editor#getPresentInStaging <em>Present In Staging</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getEditor()
 * @model extendedMetaData="name='editor' kind='elementOnly'"
 * @generated
 */
public interface Editor extends ViewComponent {
	/**
     * Returns the value of the '<em><b>Editors</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.Editor}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Editors</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Editors</em>' containment reference list.
     * @see fw2.model2.Model2Package#getEditor_Editors()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='editors' namespace='##targetNamespace'"
     * @generated
     */
	EList<Editor> getEditors();

	/**
     * Returns the value of the '<em><b>Field Groups</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.FieldGroup}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Field Groups</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Field Groups</em>' containment reference list.
     * @see fw2.model2.Model2Package#getEditor_FieldGroups()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='fieldGroups' namespace='##targetNamespace'"
     * @generated
     */
	EList<FieldGroup> getFieldGroups();

	/**
     * Returns the value of the '<em><b>Summary Tables</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.SummaryTable}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Summary Tables</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Summary Tables</em>' containment reference list.
     * @see fw2.model2.Model2Package#getEditor_SummaryTables()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='summaryTables' namespace='##targetNamespace'"
     * @generated
     */
	EList<SummaryTable> getSummaryTables();

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
     * @see fw2.model2.Model2Package#getEditor_PresentInStaging()
     * @model
     * @generated
     */
	String getPresentInStaging();

	/**
     * Sets the value of the '{@link fw2.model2.Editor#getPresentInStaging <em>Present In Staging</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Present In Staging</em>' attribute.
     * @see #getPresentInStaging()
     * @generated
     */
	void setPresentInStaging(String value);

} // Editor
