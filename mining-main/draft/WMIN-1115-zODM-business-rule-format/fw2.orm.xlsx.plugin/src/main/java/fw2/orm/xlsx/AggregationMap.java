/**
 */
package fw2.orm.xlsx;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Aggregation Map</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.AggregationMap#getComponentMap <em>Component Map</em>}</li>
 * </ul>
 *
 * @see fw2.orm.xlsx.XlsxPackage#getAggregationMap()
 * @model
 * @generated
 */
public interface AggregationMap extends AssociationMap {
	/**
	 * Returns the value of the '<em><b>Component Map</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Component Map</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Component Map</em>' containment reference.
	 * @see #setComponentMap(ComponentMap)
	 * @see fw2.orm.xlsx.XlsxPackage#getAggregationMap_ComponentMap()
	 * @model containment="true"
	 * @generated
	 */
	ComponentMap getComponentMap();

	/**
	 * Sets the value of the '{@link fw2.orm.xlsx.AggregationMap#getComponentMap <em>Component Map</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Component Map</em>' containment reference.
	 * @see #getComponentMap()
	 * @generated
	 */
	void setComponentMap(ComponentMap value);

} // AggregationMap
