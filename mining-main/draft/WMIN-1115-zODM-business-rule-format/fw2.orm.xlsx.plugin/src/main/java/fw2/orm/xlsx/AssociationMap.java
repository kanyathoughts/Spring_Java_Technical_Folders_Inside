/**
 */
package fw2.orm.xlsx;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Association Map</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link fw2.orm.xlsx.AssociationMap#getCardinality <em>Cardinality</em>}</li>
 *   <li>{@link fw2.orm.xlsx.AssociationMap#getRelatedClassMap <em>Related Class Map</em>}</li>
 * </ul>
 *
 * @see fw2.orm.xlsx.XlsxPackage#getAssociationMap()
 * @model abstract="true"
 * @generated
 */
public interface AssociationMap extends MapElement {
	/**
	 * Returns the value of the '<em><b>Cardinality</b></em>' attribute.
	 * The literals are from the enumeration {@link fw2.orm.xlsx.CardinalityEnum}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Cardinality</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Cardinality</em>' attribute.
	 * @see fw2.orm.xlsx.CardinalityEnum
	 * @see #setCardinality(CardinalityEnum)
	 * @see fw2.orm.xlsx.XlsxPackage#getAssociationMap_Cardinality()
	 * @model
	 * @generated
	 */
	CardinalityEnum getCardinality();

	/**
	 * Sets the value of the '{@link fw2.orm.xlsx.AssociationMap#getCardinality <em>Cardinality</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Cardinality</em>' attribute.
	 * @see fw2.orm.xlsx.CardinalityEnum
	 * @see #getCardinality()
	 * @generated
	 */
	void setCardinality(CardinalityEnum value);

	/**
	 * Returns the value of the '<em><b>Related Class Map</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Related Class Map</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Related Class Map</em>' reference.
	 * @see #setRelatedClassMap(ClassMap)
	 * @see fw2.orm.xlsx.XlsxPackage#getAssociationMap_RelatedClassMap()
	 * @model
	 * @generated
	 */
	ClassMap getRelatedClassMap();

	/**
	 * Sets the value of the '{@link fw2.orm.xlsx.AssociationMap#getRelatedClassMap <em>Related Class Map</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Related Class Map</em>' reference.
	 * @see #getRelatedClassMap()
	 * @generated
	 */
	void setRelatedClassMap(ClassMap value);

} // AssociationMap
