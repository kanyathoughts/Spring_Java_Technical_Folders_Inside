/**
 */
package fw2.orm.xlsx.impl;

import fw2.orm.xlsx.*;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class XlsxFactoryImpl extends EFactoryImpl implements XlsxFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static XlsxFactory init() {
		try {
			XlsxFactory theXlsxFactory = (XlsxFactory)EPackage.Registry.INSTANCE.getEFactory(XlsxPackage.eNS_URI);
			if (theXlsxFactory != null) {
				return theXlsxFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new XlsxFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public XlsxFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case XlsxPackage.COMPONENT_MAP: return createComponentMap();
			case XlsxPackage.CLASS_MAP: return createClassMap();
			case XlsxPackage.COLUMN_MAP: return createColumnMap();
			case XlsxPackage.REFERENCE_MAP: return createReferenceMap();
			case XlsxPackage.LOOKUP_MAP: return createLookupMap();
			case XlsxPackage.AGGREGATION_MAP: return createAggregationMap();
			case XlsxPackage.REFERENCE_KEY: return createReferenceKey();
			case XlsxPackage.MAPPING: return createMapping();
			case XlsxPackage.LOOKUP_KEY: return createLookupKey();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
		switch (eDataType.getClassifierID()) {
			case XlsxPackage.CARDINALITY_ENUM:
				return createCardinalityEnumFromString(eDataType, initialValue);
			case XlsxPackage.COLUMN_TYPE_ENUM:
				return createColumnTypeEnumFromString(eDataType, initialValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		switch (eDataType.getClassifierID()) {
			case XlsxPackage.CARDINALITY_ENUM:
				return convertCardinalityEnumToString(eDataType, instanceValue);
			case XlsxPackage.COLUMN_TYPE_ENUM:
				return convertColumnTypeEnumToString(eDataType, instanceValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ComponentMap createComponentMap() {
		ComponentMapImpl componentMap = new ComponentMapImpl();
		return componentMap;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ClassMap createClassMap() {
		ClassMapImpl classMap = new ClassMapImpl();
		return classMap;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ColumnMap createColumnMap() {
		ColumnMapImpl columnMap = new ColumnMapImpl();
		return columnMap;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ReferenceMap createReferenceMap() {
		ReferenceMapImpl referenceMap = new ReferenceMapImpl();
		return referenceMap;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public LookupMap createLookupMap() {
		LookupMapImpl lookupMap = new LookupMapImpl();
		return lookupMap;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AggregationMap createAggregationMap() {
		AggregationMapImpl aggregationMap = new AggregationMapImpl();
		return aggregationMap;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ReferenceKey createReferenceKey() {
		ReferenceKeyImpl referenceKey = new ReferenceKeyImpl();
		return referenceKey;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Mapping createMapping() {
		MappingImpl mapping = new MappingImpl();
		return mapping;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public LookupKey createLookupKey() {
		LookupKeyImpl lookupKey = new LookupKeyImpl();
		return lookupKey;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CardinalityEnum createCardinalityEnumFromString(EDataType eDataType, String initialValue) {
		CardinalityEnum result = CardinalityEnum.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertCardinalityEnumToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ColumnTypeEnum createColumnTypeEnumFromString(EDataType eDataType, String initialValue) {
		ColumnTypeEnum result = ColumnTypeEnum.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertColumnTypeEnumToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public XlsxPackage getXlsxPackage() {
		return (XlsxPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static XlsxPackage getPackage() {
		return XlsxPackage.eINSTANCE;
	}

} //XlsxFactoryImpl
