/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Summary Table</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.SummaryTable#getColumns <em>Columns</em>}</li>
 *   <li>{@link fw2.model2.SummaryTable#getDomainWrapper <em>Domain Wrapper</em>}</li>
 *   <li>{@link fw2.model2.SummaryTable#getBapName <em>Bap Name</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getSummaryTable()
 * @model extendedMetaData="name='summaryTable' kind='elementOnly'"
 * @generated
 */
public interface SummaryTable extends ViewComponent {
	/**
     * Returns the value of the '<em><b>Columns</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.TableColumn}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Columns</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Columns</em>' containment reference list.
     * @see fw2.model2.Model2Package#getSummaryTable_Columns()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='columns' namespace='##targetNamespace'"
     * @generated
     */
	EList<TableColumn> getColumns();

	/**
     * Returns the value of the '<em><b>Domain Wrapper</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Domain Wrapper</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Domain Wrapper</em>' attribute.
     * @see #setDomainWrapper(String)
     * @see fw2.model2.Model2Package#getSummaryTable_DomainWrapper()
     * @model
     * @generated
     */
	String getDomainWrapper();

	/**
     * Sets the value of the '{@link fw2.model2.SummaryTable#getDomainWrapper <em>Domain Wrapper</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Domain Wrapper</em>' attribute.
     * @see #getDomainWrapper()
     * @generated
     */
	void setDomainWrapper(String value);

	/**
     * Returns the value of the '<em><b>Bap Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Bap Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Bap Name</em>' attribute.
     * @see #setBapName(String)
     * @see fw2.model2.Model2Package#getSummaryTable_BapName()
     * @model
     * @generated
     */
	String getBapName();

	/**
     * Sets the value of the '{@link fw2.model2.SummaryTable#getBapName <em>Bap Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Bap Name</em>' attribute.
     * @see #getBapName()
     * @generated
     */
	void setBapName(String value);

} // SummaryTable
