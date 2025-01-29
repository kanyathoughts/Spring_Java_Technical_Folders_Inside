/**
 */
package fw2.model2;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Db Column Map</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.model2.DbColumnMap#getDbColumnName <em>Db Column Name</em>}</li>
 * </ul>
 *
 * @see fw2.model2.Model2Package#getDbColumnMap()
 * @model extendedMetaData="name='dbColumnMap' kind='elementOnly'"
 * @generated
 */
public interface DbColumnMap extends Primitive {
	/**
     * Returns the value of the '<em><b>Db Column Name</b></em>' attribute.
     * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Db Column Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
     * @return the value of the '<em>Db Column Name</em>' attribute.
     * @see #setDbColumnName(String)
     * @see fw2.model2.Model2Package#getDbColumnMap_DbColumnName()
     * @model required="true"
     * @generated
     */
	String getDbColumnName();

	/**
     * Sets the value of the '{@link fw2.model2.DbColumnMap#getDbColumnName <em>Db Column Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @param value the new value of the '<em>Db Column Name</em>' attribute.
     * @see #getDbColumnName()
     * @generated
     */
	void setDbColumnName(String value);

} // DbColumnMap
