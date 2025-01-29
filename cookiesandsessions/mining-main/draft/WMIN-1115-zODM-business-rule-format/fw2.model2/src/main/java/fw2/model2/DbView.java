/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Db View</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.DbView#getJoinKeys <em>Join Keys</em>}</li>
 *   <li>{@link fw2.model2.DbView#getJoinParameters <em>Join Parameters</em>}</li>
 *   <li>{@link fw2.model2.DbView#getViewColumns <em>View Columns</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getDbView()
 * @model extendedMetaData="name='dbView' kind='elementOnly'"
 * @generated
 */
public interface DbView extends DbDataSet {
	/**
     * Returns the value of the '<em><b>Join Keys</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.DbJoinKey}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Join Keys</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Join Keys</em>' containment reference list.
     * @see fw2.model2.Model2Package#getDbView_JoinKeys()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='joinKeys' namespace='##targetNamespace'"
     * @generated
     */
	EList<DbJoinKey> getJoinKeys();

	/**
     * Returns the value of the '<em><b>Join Parameters</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.DbJoinParameter}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Join Parameters</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Join Parameters</em>' containment reference list.
     * @see fw2.model2.Model2Package#getDbView_JoinParameters()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='joinParameters' namespace='##targetNamespace'"
     * @generated
     */
	EList<DbJoinParameter> getJoinParameters();

	/**
     * Returns the value of the '<em><b>View Columns</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.DbViewColumn}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>View Columns</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>View Columns</em>' containment reference list.
     * @see fw2.model2.Model2Package#getDbView_ViewColumns()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='viewColumns' namespace='##targetNamespace'"
     * @generated
     */
	EList<DbViewColumn> getViewColumns();

} // DbView
