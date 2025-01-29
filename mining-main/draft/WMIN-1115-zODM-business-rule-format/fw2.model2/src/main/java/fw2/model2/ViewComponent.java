/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>View Component</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.ViewComponent#getComponentId <em>Component Id</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getViewComponent()
 * @model abstract="true"
 *        extendedMetaData="name='viewComponent' kind='elementOnly'"
 * @generated
 */
public interface ViewComponent extends ViewElement {

	/**
     * Returns the value of the '<em><b>Component Id</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Component Id</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Component Id</em>' attribute.
     * @see #setComponentId(String)
     * @see fw2.model2.Model2Package#getViewComponent_ComponentId()
     * @model
     * @generated
     */
	String getComponentId();

	/**
     * Sets the value of the '{@link fw2.model2.ViewComponent#getComponentId <em>Component Id</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Component Id</em>' attribute.
     * @see #getComponentId()
     * @generated
     */
	void setComponentId(String value);
} // ViewComponent
