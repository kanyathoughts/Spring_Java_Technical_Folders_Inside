/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Action</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.Action#getAccessKey <em>Access Key</em>}</li>
 *   <li>{@link fw2.model2.Action#getUrl <em>Url</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getAction()
 * @model extendedMetaData="name='action' kind='elementOnly'"
 * @generated
 */
public interface Action extends ViewPrimitive {
	/**
     * Returns the value of the '<em><b>Access Key</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Access Key</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Access Key</em>' attribute.
     * @see #setAccessKey(String)
     * @see fw2.model2.Model2Package#getAction_AccessKey()
     * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
     *        extendedMetaData="kind='element' name='key' namespace='##targetNamespace'"
     * @generated
     */
	String getAccessKey();

	/**
     * Sets the value of the '{@link fw2.model2.Action#getAccessKey <em>Access Key</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Access Key</em>' attribute.
     * @see #getAccessKey()
     * @generated
     */
	void setAccessKey(String value);

	/**
     * Returns the value of the '<em><b>Url</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Url</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Url</em>' attribute.
     * @see #setUrl(String)
     * @see fw2.model2.Model2Package#getAction_Url()
     * @model dataType="org.eclipse.emf.ecore.xml.type.String" required="true"
     *        extendedMetaData="kind='element' name='url' namespace='##targetNamespace'"
     * @generated
     */
	String getUrl();

	/**
     * Sets the value of the '{@link fw2.model2.Action#getUrl <em>Url</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Url</em>' attribute.
     * @see #getUrl()
     * @generated
     */
	void setUrl(String value);

} // Action
