/**
 */
package fw2.model2;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Db Relation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.DbRelation#getForeignKeyColumns <em>Foreign Key Columns</em>}</li>
 *   <li>{@link fw2.model2.DbRelation#getRefTable <em>Ref Table</em>}</li>
 *   <li>{@link fw2.model2.DbRelation#getTable <em>Table</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getDbRelation()
 * @model extendedMetaData="name='dbRelation' kind='elementOnly'"
 * @generated
 */
public interface DbRelation extends ModelElement {
	/**
     * Returns the value of the '<em><b>Foreign Key Columns</b></em>' containment reference list.
     * The list contents are of type {@link fw2.model2.DbForeignKeyColumn}.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Foreign Key Columns</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Foreign Key Columns</em>' containment reference list.
     * @see fw2.model2.Model2Package#getDbRelation_ForeignKeyColumns()
     * @model containment="true"
     *        extendedMetaData="kind='element' name='foreignKeyColumns' namespace='##targetNamespace'"
     * @generated
     */
	EList<DbForeignKeyColumn> getForeignKeyColumns();

	/**
     * Returns the value of the '<em><b>Ref Table</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ref Table</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Ref Table</em>' reference.
     * @see #setRefTable(DbDataSet)
     * @see fw2.model2.Model2Package#getDbRelation_RefTable()
     * @model resolveProxies="false"
     *        extendedMetaData="kind='element' name='refTable' namespace='##targetNamespace'"
     * @generated
     */
	DbDataSet getRefTable();

	/**
     * Sets the value of the '{@link fw2.model2.DbRelation#getRefTable <em>Ref Table</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Ref Table</em>' reference.
     * @see #getRefTable()
     * @generated
     */
	void setRefTable(DbDataSet value);

	/**
     * Returns the value of the '<em><b>Table</b></em>' reference.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Table</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Table</em>' reference.
     * @see #setTable(DbDataSet)
     * @see fw2.model2.Model2Package#getDbRelation_Table()
     * @model resolveProxies="false"
     *        extendedMetaData="kind='element' name='table' namespace='##targetNamespace'"
     * @generated
     */
	DbDataSet getTable();

	/**
     * Sets the value of the '{@link fw2.model2.DbRelation#getTable <em>Table</em>}' reference.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Table</em>' reference.
     * @see #getTable()
     * @generated
     */
	void setTable(DbDataSet value);

} // DbRelation
