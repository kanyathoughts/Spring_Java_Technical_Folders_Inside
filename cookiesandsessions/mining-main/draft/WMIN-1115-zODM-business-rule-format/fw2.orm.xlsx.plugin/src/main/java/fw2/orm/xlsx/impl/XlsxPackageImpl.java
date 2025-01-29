/**
 */
package fw2.orm.xlsx.impl;

import fw2.orm.xlsx.AggregationMap;
import fw2.orm.xlsx.AssociationMap;
import fw2.orm.xlsx.CardinalityEnum;
import fw2.orm.xlsx.ClassMap;
import fw2.orm.xlsx.ColumnMap;
import fw2.orm.xlsx.ColumnTypeEnum;
import fw2.orm.xlsx.ComponentMap;
import fw2.orm.xlsx.LookupKey;
import fw2.orm.xlsx.LookupMap;
import fw2.orm.xlsx.MapElement;
import fw2.orm.xlsx.Mapping;
import fw2.orm.xlsx.ReferenceKey;
import fw2.orm.xlsx.ReferenceMap;
import fw2.orm.xlsx.XlsxFactory;
import fw2.orm.xlsx.XlsxPackage;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class XlsxPackageImpl extends EPackageImpl implements XlsxPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass mapElementEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass associationMapEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass componentMapEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass classMapEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass columnMapEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass referenceMapEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass lookupMapEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass aggregationMapEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass referenceKeyEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass mappingEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass lookupKeyEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum cardinalityEnumEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum columnTypeEnumEEnum = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see fw2.orm.xlsx.XlsxPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private XlsxPackageImpl() {
		super(eNS_URI, XlsxFactory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 * 
	 * <p>This method is used to initialize {@link XlsxPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static XlsxPackage init() {
		if (isInited) return (XlsxPackage)EPackage.Registry.INSTANCE.getEPackage(XlsxPackage.eNS_URI);

		// Obtain or create and register package
		XlsxPackageImpl theXlsxPackage = (XlsxPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof XlsxPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new XlsxPackageImpl());

		isInited = true;

		// Create package meta-data objects
		theXlsxPackage.createPackageContents();

		// Initialize created meta-data
		theXlsxPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theXlsxPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(XlsxPackage.eNS_URI, theXlsxPackage);
		return theXlsxPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getMapElement() {
		return mapElementEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getMapElement_Name() {
		return (EAttribute)mapElementEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getAssociationMap() {
		return associationMapEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getAssociationMap_Cardinality() {
		return (EAttribute)associationMapEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getAssociationMap_RelatedClassMap() {
		return (EReference)associationMapEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getComponentMap() {
		return componentMapEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getComponentMap_ColumnMaps() {
		return (EReference)componentMapEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getComponentMap_LookupMaps() {
		return (EReference)componentMapEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getClassMap() {
		return classMapEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getClassMap_AggregationMaps() {
		return (EReference)classMapEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getClassMap_ReferenceMaps() {
		return (EReference)classMapEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getColumnMap() {
		return columnMapEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getColumnMap_ColumnName() {
		return (EAttribute)columnMapEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getColumnMap_ColumnType() {
		return (EAttribute)columnMapEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getColumnMap_DefaultValue() {
		return (EAttribute)columnMapEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getColumnMap_Nullable() {
		return (EAttribute)columnMapEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getColumnMap_PrimaryKey() {
		return (EAttribute)columnMapEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getReferenceMap() {
		return referenceMapEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getReferenceMap_ReferenceKeys() {
		return (EReference)referenceMapEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getLookupMap() {
		return lookupMapEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getAggregationMap() {
		return aggregationMapEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getAggregationMap_ComponentMap() {
		return (EReference)aggregationMapEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getReferenceKey() {
		return referenceKeyEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getReferenceKey_KeyColumnName() {
		return (EAttribute)referenceKeyEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getMapping() {
		return mappingEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getMapping_ClassMaps() {
		return (EReference)mappingEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getLookupKey() {
		return lookupKeyEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EEnum getCardinalityEnum() {
		return cardinalityEnumEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EEnum getColumnTypeEnum() {
		return columnTypeEnumEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public XlsxFactory getXlsxFactory() {
		return (XlsxFactory)getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated) return;
		isCreated = true;

		// Create classes and their features
		mapElementEClass = createEClass(MAP_ELEMENT);
		createEAttribute(mapElementEClass, MAP_ELEMENT__NAME);

		associationMapEClass = createEClass(ASSOCIATION_MAP);
		createEAttribute(associationMapEClass, ASSOCIATION_MAP__CARDINALITY);
		createEReference(associationMapEClass, ASSOCIATION_MAP__RELATED_CLASS_MAP);

		componentMapEClass = createEClass(COMPONENT_MAP);
		createEReference(componentMapEClass, COMPONENT_MAP__COLUMN_MAPS);
		createEReference(componentMapEClass, COMPONENT_MAP__LOOKUP_MAPS);

		classMapEClass = createEClass(CLASS_MAP);
		createEReference(classMapEClass, CLASS_MAP__AGGREGATION_MAPS);
		createEReference(classMapEClass, CLASS_MAP__REFERENCE_MAPS);

		columnMapEClass = createEClass(COLUMN_MAP);
		createEAttribute(columnMapEClass, COLUMN_MAP__COLUMN_NAME);
		createEAttribute(columnMapEClass, COLUMN_MAP__COLUMN_TYPE);
		createEAttribute(columnMapEClass, COLUMN_MAP__DEFAULT_VALUE);
		createEAttribute(columnMapEClass, COLUMN_MAP__NULLABLE);
		createEAttribute(columnMapEClass, COLUMN_MAP__PRIMARY_KEY);

		referenceMapEClass = createEClass(REFERENCE_MAP);
		createEReference(referenceMapEClass, REFERENCE_MAP__REFERENCE_KEYS);

		lookupMapEClass = createEClass(LOOKUP_MAP);

		aggregationMapEClass = createEClass(AGGREGATION_MAP);
		createEReference(aggregationMapEClass, AGGREGATION_MAP__COMPONENT_MAP);

		referenceKeyEClass = createEClass(REFERENCE_KEY);
		createEAttribute(referenceKeyEClass, REFERENCE_KEY__KEY_COLUMN_NAME);

		mappingEClass = createEClass(MAPPING);
		createEReference(mappingEClass, MAPPING__CLASS_MAPS);

		lookupKeyEClass = createEClass(LOOKUP_KEY);

		// Create enums
		cardinalityEnumEEnum = createEEnum(CARDINALITY_ENUM);
		columnTypeEnumEEnum = createEEnum(COLUMN_TYPE_ENUM);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized) return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		associationMapEClass.getESuperTypes().add(this.getMapElement());
		componentMapEClass.getESuperTypes().add(this.getMapElement());
		classMapEClass.getESuperTypes().add(this.getComponentMap());
		columnMapEClass.getESuperTypes().add(this.getMapElement());
		referenceMapEClass.getESuperTypes().add(this.getAssociationMap());
		lookupMapEClass.getESuperTypes().add(this.getReferenceMap());
		aggregationMapEClass.getESuperTypes().add(this.getAssociationMap());
		referenceKeyEClass.getESuperTypes().add(this.getMapElement());
		mappingEClass.getESuperTypes().add(this.getMapElement());
		lookupKeyEClass.getESuperTypes().add(this.getReferenceKey());

		// Initialize classes, features, and operations; add parameters
		initEClass(mapElementEClass, MapElement.class, "MapElement", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getMapElement_Name(), ecorePackage.getEString(), "name", "", 0, 1, MapElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(associationMapEClass, AssociationMap.class, "AssociationMap", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getAssociationMap_Cardinality(), this.getCardinalityEnum(), "cardinality", null, 0, 1, AssociationMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getAssociationMap_RelatedClassMap(), this.getClassMap(), null, "relatedClassMap", null, 0, 1, AssociationMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(componentMapEClass, ComponentMap.class, "ComponentMap", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getComponentMap_ColumnMaps(), this.getColumnMap(), null, "columnMaps", null, 0, -1, ComponentMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getComponentMap_LookupMaps(), this.getLookupMap(), null, "lookupMaps", null, 0, -1, ComponentMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(classMapEClass, ClassMap.class, "ClassMap", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getClassMap_AggregationMaps(), this.getAggregationMap(), null, "aggregationMaps", null, 0, -1, ClassMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getClassMap_ReferenceMaps(), this.getReferenceMap(), null, "referenceMaps", null, 0, -1, ClassMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(columnMapEClass, ColumnMap.class, "ColumnMap", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getColumnMap_ColumnName(), ecorePackage.getEString(), "columnName", null, 0, 1, ColumnMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getColumnMap_ColumnType(), this.getColumnTypeEnum(), "columnType", null, 0, 1, ColumnMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getColumnMap_DefaultValue(), ecorePackage.getEString(), "defaultValue", null, 0, 1, ColumnMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getColumnMap_Nullable(), ecorePackage.getEBoolean(), "nullable", null, 0, 1, ColumnMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getColumnMap_PrimaryKey(), ecorePackage.getEBoolean(), "primaryKey", null, 0, 1, ColumnMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(referenceMapEClass, ReferenceMap.class, "ReferenceMap", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getReferenceMap_ReferenceKeys(), this.getReferenceKey(), null, "referenceKeys", null, 0, -1, ReferenceMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(lookupMapEClass, LookupMap.class, "LookupMap", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		initEClass(aggregationMapEClass, AggregationMap.class, "AggregationMap", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getAggregationMap_ComponentMap(), this.getComponentMap(), null, "componentMap", null, 0, 1, AggregationMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(referenceKeyEClass, ReferenceKey.class, "ReferenceKey", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getReferenceKey_KeyColumnName(), ecorePackage.getEString(), "keyColumnName", "", 0, 1, ReferenceKey.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(mappingEClass, Mapping.class, "Mapping", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getMapping_ClassMaps(), this.getClassMap(), null, "classMaps", null, 0, -1, Mapping.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(lookupKeyEClass, LookupKey.class, "LookupKey", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

		// Initialize enums and add enum literals
		initEEnum(cardinalityEnumEEnum, CardinalityEnum.class, "CardinalityEnum");
		addEEnumLiteral(cardinalityEnumEEnum, CardinalityEnum.ONE);
		addEEnumLiteral(cardinalityEnumEEnum, CardinalityEnum.MANY);

		initEEnum(columnTypeEnumEEnum, ColumnTypeEnum.class, "ColumnTypeEnum");
		addEEnumLiteral(columnTypeEnumEEnum, ColumnTypeEnum.TEXT);
		addEEnumLiteral(columnTypeEnumEEnum, ColumnTypeEnum.TIMESTAMP);
		addEEnumLiteral(columnTypeEnumEEnum, ColumnTypeEnum.DATE);
		addEEnumLiteral(columnTypeEnumEEnum, ColumnTypeEnum.INTEGER);
		addEEnumLiteral(columnTypeEnumEEnum, ColumnTypeEnum.TIME);
		addEEnumLiteral(columnTypeEnumEEnum, ColumnTypeEnum.DECIMAL);
		addEEnumLiteral(columnTypeEnumEEnum, ColumnTypeEnum.LONG);
		addEEnumLiteral(columnTypeEnumEEnum, ColumnTypeEnum.BOOLEAN);

		// Create resource
		createResource(eNS_URI);
	}

} //XlsxPackageImpl
