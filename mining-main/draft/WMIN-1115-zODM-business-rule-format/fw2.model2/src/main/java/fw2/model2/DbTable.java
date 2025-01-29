/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Db Table</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.DbTable#getColumns <em>Columns</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getDbTable()
 * @model extendedMetaData="name='dbTable' kind='elementOnly'"
 * @generated
 */
public interface DbTable extends DbDataSet {
	/**
     * Returns the value of the '<em><b>Columns</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.DbColumn}.
     * It is bidirectional and its opposite is '{@link fw2.model2.DbColumn#getTable <em>Table</em>}'.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Columns</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Columns</em>' containment reference list.
     * @see fw2.model2.Model2Package#getDbTable_Columns()
     * @see fw2.model2.DbColumn#getTable
     * @model opposite="table" containment="true" required="true"
     *        extendedMetaData="kind='element' name='columns' namespace='##targetNamespace'"
     * @generated
     */
	EList<DbColumn> getColumns();

} // DbTable
