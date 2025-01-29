/**
 */
package fw2.orm.xlsx;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Component Map</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.ComponentMap#getColumnMaps <em>Column Maps</em>}</li>
 *   <li>{@link fw2.orm.xlsx.ComponentMap#getLookupMaps <em>Lookup Maps</em>}</li>
 * </ul>
 *
 * @see fw2.orm.xlsx.XlsxPackage#getComponentMap()
 * @model
 * @generated
 */
public interface ComponentMap extends MapElement {
	/**
	 * Returns the value of the '<em><b>Column Maps</b></em>' containment reference list.
	 * The list contents are of type {@link fw2.orm.xlsx.ColumnMap}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Column Maps</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Column Maps</em>' containment reference list.
	 * @see fw2.orm.xlsx.XlsxPackage#getComponentMap_ColumnMaps()
	 * @model containment="true"
	 * @generated
	 */
	EList<ColumnMap> getColumnMaps();

	/**
	 * Returns the value of the '<em><b>Lookup Maps</b></em>' containment reference list.
	 * The list contents are of type {@link fw2.orm.xlsx.LookupMap}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Lookup Maps</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Lookup Maps</em>' containment reference list.
	 * @see fw2.orm.xlsx.XlsxPackage#getComponentMap_LookupMaps()
	 * @model containment="true"
	 * @generated
	 */
	EList<LookupMap> getLookupMaps();

} // ComponentMap
