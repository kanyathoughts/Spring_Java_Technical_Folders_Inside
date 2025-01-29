/**
 */
package fw2.orm.xlsx;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see fw2.orm.xlsx.XlsxFactory
 * @model kind="package"
 * @generated
 */
public interface XlsxPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "xlsx";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://fw2.orm.xlsx/1.0";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "fw2.orm.xlsx";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	XlsxPackage eINSTANCE = fw2.orm.xlsx.impl.XlsxPackageImpl.init();

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.MapElementImpl <em>Map Element</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.MapElementImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getMapElement()
	 * @generated
	 */
	int MAP_ELEMENT = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MAP_ELEMENT__NAME = 0;

	/**
	 * The number of structural features of the '<em>Map Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MAP_ELEMENT_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Map Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MAP_ELEMENT_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.AssociationMapImpl <em>Association Map</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.AssociationMapImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getAssociationMap()
	 * @generated
	 */
	int ASSOCIATION_MAP = 1;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSOCIATION_MAP__NAME = MAP_ELEMENT__NAME;

	/**
	 * The feature id for the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSOCIATION_MAP__CARDINALITY = MAP_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Related Class Map</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSOCIATION_MAP__RELATED_CLASS_MAP = MAP_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Association Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSOCIATION_MAP_FEATURE_COUNT = MAP_ELEMENT_FEATURE_COUNT + 2;

	/**
	 * The number of operations of the '<em>Association Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSOCIATION_MAP_OPERATION_COUNT = MAP_ELEMENT_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.ComponentMapImpl <em>Component Map</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.ComponentMapImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getComponentMap()
	 * @generated
	 */
	int COMPONENT_MAP = 2;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COMPONENT_MAP__NAME = MAP_ELEMENT__NAME;

	/**
	 * The feature id for the '<em><b>Column Maps</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COMPONENT_MAP__COLUMN_MAPS = MAP_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Lookup Maps</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COMPONENT_MAP__LOOKUP_MAPS = MAP_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Component Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COMPONENT_MAP_FEATURE_COUNT = MAP_ELEMENT_FEATURE_COUNT + 2;

	/**
	 * The number of operations of the '<em>Component Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COMPONENT_MAP_OPERATION_COUNT = MAP_ELEMENT_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.ClassMapImpl <em>Class Map</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.ClassMapImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getClassMap()
	 * @generated
	 */
	int CLASS_MAP = 3;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_MAP__NAME = COMPONENT_MAP__NAME;

	/**
	 * The feature id for the '<em><b>Column Maps</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_MAP__COLUMN_MAPS = COMPONENT_MAP__COLUMN_MAPS;

	/**
	 * The feature id for the '<em><b>Lookup Maps</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_MAP__LOOKUP_MAPS = COMPONENT_MAP__LOOKUP_MAPS;

	/**
	 * The feature id for the '<em><b>Aggregation Maps</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_MAP__AGGREGATION_MAPS = COMPONENT_MAP_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Reference Maps</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_MAP__REFERENCE_MAPS = COMPONENT_MAP_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Class Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_MAP_FEATURE_COUNT = COMPONENT_MAP_FEATURE_COUNT + 2;

	/**
	 * The number of operations of the '<em>Class Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CLASS_MAP_OPERATION_COUNT = COMPONENT_MAP_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.ColumnMapImpl <em>Column Map</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.ColumnMapImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getColumnMap()
	 * @generated
	 */
	int COLUMN_MAP = 4;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_MAP__NAME = MAP_ELEMENT__NAME;

	/**
	 * The feature id for the '<em><b>Column Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_MAP__COLUMN_NAME = MAP_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Column Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_MAP__COLUMN_TYPE = MAP_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Default Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_MAP__DEFAULT_VALUE = MAP_ELEMENT_FEATURE_COUNT + 2;

	/**
	 * The feature id for the '<em><b>Nullable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_MAP__NULLABLE = MAP_ELEMENT_FEATURE_COUNT + 3;

	/**
	 * The feature id for the '<em><b>Primary Key</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_MAP__PRIMARY_KEY = MAP_ELEMENT_FEATURE_COUNT + 4;

	/**
	 * The number of structural features of the '<em>Column Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_MAP_FEATURE_COUNT = MAP_ELEMENT_FEATURE_COUNT + 5;

	/**
	 * The number of operations of the '<em>Column Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int COLUMN_MAP_OPERATION_COUNT = MAP_ELEMENT_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.ReferenceMapImpl <em>Reference Map</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.ReferenceMapImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getReferenceMap()
	 * @generated
	 */
	int REFERENCE_MAP = 5;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFERENCE_MAP__NAME = ASSOCIATION_MAP__NAME;

	/**
	 * The feature id for the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFERENCE_MAP__CARDINALITY = ASSOCIATION_MAP__CARDINALITY;

	/**
	 * The feature id for the '<em><b>Related Class Map</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFERENCE_MAP__RELATED_CLASS_MAP = ASSOCIATION_MAP__RELATED_CLASS_MAP;

	/**
	 * The feature id for the '<em><b>Reference Keys</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFERENCE_MAP__REFERENCE_KEYS = ASSOCIATION_MAP_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Reference Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFERENCE_MAP_FEATURE_COUNT = ASSOCIATION_MAP_FEATURE_COUNT + 1;

	/**
	 * The number of operations of the '<em>Reference Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFERENCE_MAP_OPERATION_COUNT = ASSOCIATION_MAP_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.LookupMapImpl <em>Lookup Map</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.LookupMapImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getLookupMap()
	 * @generated
	 */
	int LOOKUP_MAP = 6;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOKUP_MAP__NAME = REFERENCE_MAP__NAME;

	/**
	 * The feature id for the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOKUP_MAP__CARDINALITY = REFERENCE_MAP__CARDINALITY;

	/**
	 * The feature id for the '<em><b>Related Class Map</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOKUP_MAP__RELATED_CLASS_MAP = REFERENCE_MAP__RELATED_CLASS_MAP;

	/**
	 * The feature id for the '<em><b>Reference Keys</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOKUP_MAP__REFERENCE_KEYS = REFERENCE_MAP__REFERENCE_KEYS;

	/**
	 * The number of structural features of the '<em>Lookup Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOKUP_MAP_FEATURE_COUNT = REFERENCE_MAP_FEATURE_COUNT + 0;

	/**
	 * The number of operations of the '<em>Lookup Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOKUP_MAP_OPERATION_COUNT = REFERENCE_MAP_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.AggregationMapImpl <em>Aggregation Map</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.AggregationMapImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getAggregationMap()
	 * @generated
	 */
	int AGGREGATION_MAP = 7;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AGGREGATION_MAP__NAME = ASSOCIATION_MAP__NAME;

	/**
	 * The feature id for the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AGGREGATION_MAP__CARDINALITY = ASSOCIATION_MAP__CARDINALITY;

	/**
	 * The feature id for the '<em><b>Related Class Map</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AGGREGATION_MAP__RELATED_CLASS_MAP = ASSOCIATION_MAP__RELATED_CLASS_MAP;

	/**
	 * The feature id for the '<em><b>Component Map</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AGGREGATION_MAP__COMPONENT_MAP = ASSOCIATION_MAP_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Aggregation Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AGGREGATION_MAP_FEATURE_COUNT = ASSOCIATION_MAP_FEATURE_COUNT + 1;

	/**
	 * The number of operations of the '<em>Aggregation Map</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AGGREGATION_MAP_OPERATION_COUNT = ASSOCIATION_MAP_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.ReferenceKeyImpl <em>Reference Key</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.ReferenceKeyImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getReferenceKey()
	 * @generated
	 */
	int REFERENCE_KEY = 8;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFERENCE_KEY__NAME = MAP_ELEMENT__NAME;

	/**
	 * The feature id for the '<em><b>Key Column Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFERENCE_KEY__KEY_COLUMN_NAME = MAP_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Reference Key</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFERENCE_KEY_FEATURE_COUNT = MAP_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The number of operations of the '<em>Reference Key</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFERENCE_KEY_OPERATION_COUNT = MAP_ELEMENT_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.MappingImpl <em>Mapping</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.MappingImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getMapping()
	 * @generated
	 */
	int MAPPING = 9;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MAPPING__NAME = MAP_ELEMENT__NAME;

	/**
	 * The feature id for the '<em><b>Class Maps</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MAPPING__CLASS_MAPS = MAP_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Mapping</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MAPPING_FEATURE_COUNT = MAP_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The number of operations of the '<em>Mapping</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MAPPING_OPERATION_COUNT = MAP_ELEMENT_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.impl.LookupKeyImpl <em>Lookup Key</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.impl.LookupKeyImpl
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getLookupKey()
	 * @generated
	 */
	int LOOKUP_KEY = 10;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOKUP_KEY__NAME = REFERENCE_KEY__NAME;

	/**
	 * The feature id for the '<em><b>Key Column Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOKUP_KEY__KEY_COLUMN_NAME = REFERENCE_KEY__KEY_COLUMN_NAME;

	/**
	 * The number of structural features of the '<em>Lookup Key</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOKUP_KEY_FEATURE_COUNT = REFERENCE_KEY_FEATURE_COUNT + 0;

	/**
	 * The number of operations of the '<em>Lookup Key</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOOKUP_KEY_OPERATION_COUNT = REFERENCE_KEY_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.CardinalityEnum <em>Cardinality Enum</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.CardinalityEnum
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getCardinalityEnum()
	 * @generated
	 */
	int CARDINALITY_ENUM = 11;

	/**
	 * The meta object id for the '{@link fw2.orm.xlsx.ColumnTypeEnum <em>Column Type Enum</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see fw2.orm.xlsx.ColumnTypeEnum
	 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getColumnTypeEnum()
	 * @generated
	 */
	int COLUMN_TYPE_ENUM = 12;


	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.MapElement <em>Map Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Map Element</em>'.
	 * @see fw2.orm.xlsx.MapElement
	 * @generated
	 */
	EClass getMapElement();

	/**
	 * Returns the meta object for the attribute '{@link fw2.orm.xlsx.MapElement#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see fw2.orm.xlsx.MapElement#getName()
	 * @see #getMapElement()
	 * @generated
	 */
	EAttribute getMapElement_Name();

	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.AssociationMap <em>Association Map</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Association Map</em>'.
	 * @see fw2.orm.xlsx.AssociationMap
	 * @generated
	 */
	EClass getAssociationMap();

	/**
	 * Returns the meta object for the attribute '{@link fw2.orm.xlsx.AssociationMap#getCardinality <em>Cardinality</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cardinality</em>'.
	 * @see fw2.orm.xlsx.AssociationMap#getCardinality()
	 * @see #getAssociationMap()
	 * @generated
	 */
	EAttribute getAssociationMap_Cardinality();

	/**
	 * Returns the meta object for the reference '{@link fw2.orm.xlsx.AssociationMap#getRelatedClassMap <em>Related Class Map</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Related Class Map</em>'.
	 * @see fw2.orm.xlsx.AssociationMap#getRelatedClassMap()
	 * @see #getAssociationMap()
	 * @generated
	 */
	EReference getAssociationMap_RelatedClassMap();

	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.ComponentMap <em>Component Map</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Component Map</em>'.
	 * @see fw2.orm.xlsx.ComponentMap
	 * @generated
	 */
	EClass getComponentMap();

	/**
	 * Returns the meta object for the containment reference list '{@link fw2.orm.xlsx.ComponentMap#getColumnMaps <em>Column Maps</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Column Maps</em>'.
	 * @see fw2.orm.xlsx.ComponentMap#getColumnMaps()
	 * @see #getComponentMap()
	 * @generated
	 */
	EReference getComponentMap_ColumnMaps();

	/**
	 * Returns the meta object for the containment reference list '{@link fw2.orm.xlsx.ComponentMap#getLookupMaps <em>Lookup Maps</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Lookup Maps</em>'.
	 * @see fw2.orm.xlsx.ComponentMap#getLookupMaps()
	 * @see #getComponentMap()
	 * @generated
	 */
	EReference getComponentMap_LookupMaps();

	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.ClassMap <em>Class Map</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Class Map</em>'.
	 * @see fw2.orm.xlsx.ClassMap
	 * @generated
	 */
	EClass getClassMap();

	/**
	 * Returns the meta object for the containment reference list '{@link fw2.orm.xlsx.ClassMap#getAggregationMaps <em>Aggregation Maps</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Aggregation Maps</em>'.
	 * @see fw2.orm.xlsx.ClassMap#getAggregationMaps()
	 * @see #getClassMap()
	 * @generated
	 */
	EReference getClassMap_AggregationMaps();

	/**
	 * Returns the meta object for the containment reference list '{@link fw2.orm.xlsx.ClassMap#getReferenceMaps <em>Reference Maps</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Reference Maps</em>'.
	 * @see fw2.orm.xlsx.ClassMap#getReferenceMaps()
	 * @see #getClassMap()
	 * @generated
	 */
	EReference getClassMap_ReferenceMaps();

	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.ColumnMap <em>Column Map</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Column Map</em>'.
	 * @see fw2.orm.xlsx.ColumnMap
	 * @generated
	 */
	EClass getColumnMap();

	/**
	 * Returns the meta object for the attribute '{@link fw2.orm.xlsx.ColumnMap#getColumnName <em>Column Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Column Name</em>'.
	 * @see fw2.orm.xlsx.ColumnMap#getColumnName()
	 * @see #getColumnMap()
	 * @generated
	 */
	EAttribute getColumnMap_ColumnName();

	/**
	 * Returns the meta object for the attribute '{@link fw2.orm.xlsx.ColumnMap#getColumnType <em>Column Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Column Type</em>'.
	 * @see fw2.orm.xlsx.ColumnMap#getColumnType()
	 * @see #getColumnMap()
	 * @generated
	 */
	EAttribute getColumnMap_ColumnType();

	/**
	 * Returns the meta object for the attribute '{@link fw2.orm.xlsx.ColumnMap#getDefaultValue <em>Default Value</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Default Value</em>'.
	 * @see fw2.orm.xlsx.ColumnMap#getDefaultValue()
	 * @see #getColumnMap()
	 * @generated
	 */
	EAttribute getColumnMap_DefaultValue();

	/**
	 * Returns the meta object for the attribute '{@link fw2.orm.xlsx.ColumnMap#isNullable <em>Nullable</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Nullable</em>'.
	 * @see fw2.orm.xlsx.ColumnMap#isNullable()
	 * @see #getColumnMap()
	 * @generated
	 */
	EAttribute getColumnMap_Nullable();

	/**
	 * Returns the meta object for the attribute '{@link fw2.orm.xlsx.ColumnMap#isPrimaryKey <em>Primary Key</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Primary Key</em>'.
	 * @see fw2.orm.xlsx.ColumnMap#isPrimaryKey()
	 * @see #getColumnMap()
	 * @generated
	 */
	EAttribute getColumnMap_PrimaryKey();

	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.ReferenceMap <em>Reference Map</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Reference Map</em>'.
	 * @see fw2.orm.xlsx.ReferenceMap
	 * @generated
	 */
	EClass getReferenceMap();

	/**
	 * Returns the meta object for the containment reference list '{@link fw2.orm.xlsx.ReferenceMap#getReferenceKeys <em>Reference Keys</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Reference Keys</em>'.
	 * @see fw2.orm.xlsx.ReferenceMap#getReferenceKeys()
	 * @see #getReferenceMap()
	 * @generated
	 */
	EReference getReferenceMap_ReferenceKeys();

	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.LookupMap <em>Lookup Map</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Lookup Map</em>'.
	 * @see fw2.orm.xlsx.LookupMap
	 * @generated
	 */
	EClass getLookupMap();

	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.AggregationMap <em>Aggregation Map</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Aggregation Map</em>'.
	 * @see fw2.orm.xlsx.AggregationMap
	 * @generated
	 */
	EClass getAggregationMap();

	/**
	 * Returns the meta object for the containment reference '{@link fw2.orm.xlsx.AggregationMap#getComponentMap <em>Component Map</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Component Map</em>'.
	 * @see fw2.orm.xlsx.AggregationMap#getComponentMap()
	 * @see #getAggregationMap()
	 * @generated
	 */
	EReference getAggregationMap_ComponentMap();

	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.ReferenceKey <em>Reference Key</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Reference Key</em>'.
	 * @see fw2.orm.xlsx.ReferenceKey
	 * @generated
	 */
	EClass getReferenceKey();

	/**
	 * Returns the meta object for the attribute '{@link fw2.orm.xlsx.ReferenceKey#getKeyColumnName <em>Key Column Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Key Column Name</em>'.
	 * @see fw2.orm.xlsx.ReferenceKey#getKeyColumnName()
	 * @see #getReferenceKey()
	 * @generated
	 */
	EAttribute getReferenceKey_KeyColumnName();

	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.Mapping <em>Mapping</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Mapping</em>'.
	 * @see fw2.orm.xlsx.Mapping
	 * @generated
	 */
	EClass getMapping();

	/**
	 * Returns the meta object for the containment reference list '{@link fw2.orm.xlsx.Mapping#getClassMaps <em>Class Maps</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Class Maps</em>'.
	 * @see fw2.orm.xlsx.Mapping#getClassMaps()
	 * @see #getMapping()
	 * @generated
	 */
	EReference getMapping_ClassMaps();

	/**
	 * Returns the meta object for class '{@link fw2.orm.xlsx.LookupKey <em>Lookup Key</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Lookup Key</em>'.
	 * @see fw2.orm.xlsx.LookupKey
	 * @generated
	 */
	EClass getLookupKey();

	/**
	 * Returns the meta object for enum '{@link fw2.orm.xlsx.CardinalityEnum <em>Cardinality Enum</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Cardinality Enum</em>'.
	 * @see fw2.orm.xlsx.CardinalityEnum
	 * @generated
	 */
	EEnum getCardinalityEnum();

	/**
	 * Returns the meta object for enum '{@link fw2.orm.xlsx.ColumnTypeEnum <em>Column Type Enum</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Column Type Enum</em>'.
	 * @see fw2.orm.xlsx.ColumnTypeEnum
	 * @generated
	 */
	EEnum getColumnTypeEnum();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	XlsxFactory getXlsxFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each operation of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.MapElementImpl <em>Map Element</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.MapElementImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getMapElement()
		 * @generated
		 */
		EClass MAP_ELEMENT = eINSTANCE.getMapElement();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute MAP_ELEMENT__NAME = eINSTANCE.getMapElement_Name();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.AssociationMapImpl <em>Association Map</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.AssociationMapImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getAssociationMap()
		 * @generated
		 */
		EClass ASSOCIATION_MAP = eINSTANCE.getAssociationMap();

		/**
		 * The meta object literal for the '<em><b>Cardinality</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ASSOCIATION_MAP__CARDINALITY = eINSTANCE.getAssociationMap_Cardinality();

		/**
		 * The meta object literal for the '<em><b>Related Class Map</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ASSOCIATION_MAP__RELATED_CLASS_MAP = eINSTANCE.getAssociationMap_RelatedClassMap();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.ComponentMapImpl <em>Component Map</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.ComponentMapImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getComponentMap()
		 * @generated
		 */
		EClass COMPONENT_MAP = eINSTANCE.getComponentMap();

		/**
		 * The meta object literal for the '<em><b>Column Maps</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference COMPONENT_MAP__COLUMN_MAPS = eINSTANCE.getComponentMap_ColumnMaps();

		/**
		 * The meta object literal for the '<em><b>Lookup Maps</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference COMPONENT_MAP__LOOKUP_MAPS = eINSTANCE.getComponentMap_LookupMaps();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.ClassMapImpl <em>Class Map</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.ClassMapImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getClassMap()
		 * @generated
		 */
		EClass CLASS_MAP = eINSTANCE.getClassMap();

		/**
		 * The meta object literal for the '<em><b>Aggregation Maps</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CLASS_MAP__AGGREGATION_MAPS = eINSTANCE.getClassMap_AggregationMaps();

		/**
		 * The meta object literal for the '<em><b>Reference Maps</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CLASS_MAP__REFERENCE_MAPS = eINSTANCE.getClassMap_ReferenceMaps();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.ColumnMapImpl <em>Column Map</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.ColumnMapImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getColumnMap()
		 * @generated
		 */
		EClass COLUMN_MAP = eINSTANCE.getColumnMap();

		/**
		 * The meta object literal for the '<em><b>Column Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute COLUMN_MAP__COLUMN_NAME = eINSTANCE.getColumnMap_ColumnName();

		/**
		 * The meta object literal for the '<em><b>Column Type</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute COLUMN_MAP__COLUMN_TYPE = eINSTANCE.getColumnMap_ColumnType();

		/**
		 * The meta object literal for the '<em><b>Default Value</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute COLUMN_MAP__DEFAULT_VALUE = eINSTANCE.getColumnMap_DefaultValue();

		/**
		 * The meta object literal for the '<em><b>Nullable</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute COLUMN_MAP__NULLABLE = eINSTANCE.getColumnMap_Nullable();

		/**
		 * The meta object literal for the '<em><b>Primary Key</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute COLUMN_MAP__PRIMARY_KEY = eINSTANCE.getColumnMap_PrimaryKey();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.ReferenceMapImpl <em>Reference Map</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.ReferenceMapImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getReferenceMap()
		 * @generated
		 */
		EClass REFERENCE_MAP = eINSTANCE.getReferenceMap();

		/**
		 * The meta object literal for the '<em><b>Reference Keys</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference REFERENCE_MAP__REFERENCE_KEYS = eINSTANCE.getReferenceMap_ReferenceKeys();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.LookupMapImpl <em>Lookup Map</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.LookupMapImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getLookupMap()
		 * @generated
		 */
		EClass LOOKUP_MAP = eINSTANCE.getLookupMap();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.AggregationMapImpl <em>Aggregation Map</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.AggregationMapImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getAggregationMap()
		 * @generated
		 */
		EClass AGGREGATION_MAP = eINSTANCE.getAggregationMap();

		/**
		 * The meta object literal for the '<em><b>Component Map</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference AGGREGATION_MAP__COMPONENT_MAP = eINSTANCE.getAggregationMap_ComponentMap();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.ReferenceKeyImpl <em>Reference Key</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.ReferenceKeyImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getReferenceKey()
		 * @generated
		 */
		EClass REFERENCE_KEY = eINSTANCE.getReferenceKey();

		/**
		 * The meta object literal for the '<em><b>Key Column Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute REFERENCE_KEY__KEY_COLUMN_NAME = eINSTANCE.getReferenceKey_KeyColumnName();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.MappingImpl <em>Mapping</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.MappingImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getMapping()
		 * @generated
		 */
		EClass MAPPING = eINSTANCE.getMapping();

		/**
		 * The meta object literal for the '<em><b>Class Maps</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference MAPPING__CLASS_MAPS = eINSTANCE.getMapping_ClassMaps();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.impl.LookupKeyImpl <em>Lookup Key</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.impl.LookupKeyImpl
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getLookupKey()
		 * @generated
		 */
		EClass LOOKUP_KEY = eINSTANCE.getLookupKey();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.CardinalityEnum <em>Cardinality Enum</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.CardinalityEnum
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getCardinalityEnum()
		 * @generated
		 */
		EEnum CARDINALITY_ENUM = eINSTANCE.getCardinalityEnum();

		/**
		 * The meta object literal for the '{@link fw2.orm.xlsx.ColumnTypeEnum <em>Column Type Enum</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see fw2.orm.xlsx.ColumnTypeEnum
		 * @see fw2.orm.xlsx.impl.XlsxPackageImpl#getColumnTypeEnum()
		 * @generated
		 */
		EEnum COLUMN_TYPE_ENUM = eINSTANCE.getColumnTypeEnum();

	}

} //XlsxPackage
