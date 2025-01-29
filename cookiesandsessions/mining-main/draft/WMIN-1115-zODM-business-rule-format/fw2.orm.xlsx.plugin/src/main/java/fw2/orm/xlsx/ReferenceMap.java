/**
 */
package fw2.orm.xlsx;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Reference Map</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.ReferenceMap#getReferenceKeys <em>Reference Keys</em>}</li>
 * </ul>
 *
 * @see fw2.orm.xlsx.XlsxPackage#getReferenceMap()
 * @model
 * @generated
 */
public interface ReferenceMap extends AssociationMap {
	/**
	 * Returns the value of the '<em><b>Reference Keys</b></em>' containment reference list.
	 * The list contents are of type {@link fw2.orm.xlsx.ReferenceKey}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Reference Keys</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Reference Keys</em>' containment reference list.
	 * @see fw2.orm.xlsx.XlsxPackage#getReferenceMap_ReferenceKeys()
	 * @model containment="true"
	 * @generated
	 */
	EList<ReferenceKey> getReferenceKeys();

} // ReferenceMap
