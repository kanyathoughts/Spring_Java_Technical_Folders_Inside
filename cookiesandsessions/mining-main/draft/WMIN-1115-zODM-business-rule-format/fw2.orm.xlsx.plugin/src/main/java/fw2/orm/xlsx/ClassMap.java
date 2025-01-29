/**
 */
package fw2.orm.xlsx;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Class Map</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.ClassMap#getAggregationMaps <em>Aggregation Maps</em>}</li>
 *   <li>{@link fw2.orm.xlsx.ClassMap#getReferenceMaps <em>Reference Maps</em>}</li>
 * </ul>
 *
 * @see fw2.orm.xlsx.XlsxPackage#getClassMap()
 * @model
 * @generated
 */
public interface ClassMap extends ComponentMap {
	/**
	 * Returns the value of the '<em><b>Aggregation Maps</b></em>' containment reference list.
	 * The list contents are of type {@link fw2.orm.xlsx.AggregationMap}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Aggregation Maps</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Aggregation Maps</em>' containment reference list.
	 * @see fw2.orm.xlsx.XlsxPackage#getClassMap_AggregationMaps()
	 * @model containment="true"
	 * @generated
	 */
	EList<AggregationMap> getAggregationMaps();

	/**
	 * Returns the value of the '<em><b>Reference Maps</b></em>' containment reference list.
	 * The list contents are of type {@link fw2.orm.xlsx.ReferenceMap}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Reference Maps</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Reference Maps</em>' containment reference list.
	 * @see fw2.orm.xlsx.XlsxPackage#getClassMap_ReferenceMaps()
	 * @model containment="true"
	 * @generated
	 */
	EList<ReferenceMap> getReferenceMaps();

} // ClassMap
