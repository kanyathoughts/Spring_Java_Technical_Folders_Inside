/**
 */
package fw2.orm.xlsx;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Reference Key</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.ReferenceKey#getKeyColumnName <em>Key Column Name</em>}</li>
 * </ul>
 *
 * @see fw2.orm.xlsx.XlsxPackage#getReferenceKey()
 * @model
 * @generated
 */
public interface ReferenceKey extends MapElement {
	/**
	 * Returns the value of the '<em><b>Key Column Name</b></em>' attribute.
	 * The default value is <code>""</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Key Column Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Key Column Name</em>' attribute.
	 * @see #setKeyColumnName(String)
	 * @see fw2.orm.xlsx.XlsxPackage#getReferenceKey_KeyColumnName()
	 * @model default=""
	 * @generated
	 */
	String getKeyColumnName();

	/**
	 * Sets the value of the '{@link fw2.orm.xlsx.ReferenceKey#getKeyColumnName <em>Key Column Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Key Column Name</em>' attribute.
	 * @see #getKeyColumnName()
	 * @generated
	 */
	void setKeyColumnName(String value);

} // ReferenceKey
