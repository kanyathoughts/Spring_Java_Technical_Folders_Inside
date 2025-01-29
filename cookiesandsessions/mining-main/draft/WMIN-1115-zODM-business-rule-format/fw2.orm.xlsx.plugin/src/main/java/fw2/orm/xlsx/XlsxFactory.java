/**
 */
package fw2.orm.xlsx;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see fw2.orm.xlsx.XlsxPackage
 * @generated
 */
public interface XlsxFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	XlsxFactory eINSTANCE = fw2.orm.xlsx.impl.XlsxFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>Component Map</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Component Map</em>'.
	 * @generated
	 */
	ComponentMap createComponentMap();

	/**
	 * Returns a new object of class '<em>Class Map</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Class Map</em>'.
	 * @generated
	 */
	ClassMap createClassMap();

	/**
	 * Returns a new object of class '<em>Column Map</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Column Map</em>'.
	 * @generated
	 */
	ColumnMap createColumnMap();

	/**
	 * Returns a new object of class '<em>Reference Map</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Reference Map</em>'.
	 * @generated
	 */
	ReferenceMap createReferenceMap();

	/**
	 * Returns a new object of class '<em>Lookup Map</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Lookup Map</em>'.
	 * @generated
	 */
	LookupMap createLookupMap();

	/**
	 * Returns a new object of class '<em>Aggregation Map</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Aggregation Map</em>'.
	 * @generated
	 */
	AggregationMap createAggregationMap();

	/**
	 * Returns a new object of class '<em>Reference Key</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Reference Key</em>'.
	 * @generated
	 */
	ReferenceKey createReferenceKey();

	/**
	 * Returns a new object of class '<em>Mapping</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Mapping</em>'.
	 * @generated
	 */
	Mapping createMapping();

	/**
	 * Returns a new object of class '<em>Lookup Key</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Lookup Key</em>'.
	 * @generated
	 */
	LookupKey createLookupKey();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	XlsxPackage getXlsxPackage();

} //XlsxFactory
