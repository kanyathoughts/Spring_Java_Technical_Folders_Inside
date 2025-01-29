/**
 */
package fw2.orm.xlsx.util;

import fw2.orm.xlsx.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see fw2.orm.xlsx.XlsxPackage
 * @generated
 */
public class XlsxAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static XlsxPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public XlsxAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = XlsxPackage.eINSTANCE;
		}
	}

	/**
	 * Returns whether this factory is applicable for the type of the object.
	 * <!-- begin-user-doc -->
	 * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
	 * <!-- end-user-doc -->
	 * @return whether this factory is applicable for the type of the object.
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage) {
			return true;
		}
		if (object instanceof EObject) {
			return ((EObject)object).eClass().getEPackage() == modelPackage;
		}
		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected XlsxSwitch<Adapter> modelSwitch =
		new XlsxSwitch<Adapter>() {
			@Override
			public Adapter caseMapElement(MapElement object) {
				return createMapElementAdapter();
			}
			@Override
			public Adapter caseAssociationMap(AssociationMap object) {
				return createAssociationMapAdapter();
			}
			@Override
			public Adapter caseComponentMap(ComponentMap object) {
				return createComponentMapAdapter();
			}
			@Override
			public Adapter caseClassMap(ClassMap object) {
				return createClassMapAdapter();
			}
			@Override
			public Adapter caseColumnMap(ColumnMap object) {
				return createColumnMapAdapter();
			}
			@Override
			public Adapter caseReferenceMap(ReferenceMap object) {
				return createReferenceMapAdapter();
			}
			@Override
			public Adapter caseLookupMap(LookupMap object) {
				return createLookupMapAdapter();
			}
			@Override
			public Adapter caseAggregationMap(AggregationMap object) {
				return createAggregationMapAdapter();
			}
			@Override
			public Adapter caseReferenceKey(ReferenceKey object) {
				return createReferenceKeyAdapter();
			}
			@Override
			public Adapter caseMapping(Mapping object) {
				return createMappingAdapter();
			}
			@Override
			public Adapter caseLookupKey(LookupKey object) {
				return createLookupKeyAdapter();
			}
			@Override
			public Adapter defaultCase(EObject object) {
				return createEObjectAdapter();
			}
		};

	/**
	 * Creates an adapter for the <code>target</code>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param target the object to adapt.
	 * @return the adapter for the <code>target</code>.
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject)target);
	}


	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.MapElement <em>Map Element</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.MapElement
	 * @generated
	 */
	public Adapter createMapElementAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.AssociationMap <em>Association Map</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.AssociationMap
	 * @generated
	 */
	public Adapter createAssociationMapAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.ComponentMap <em>Component Map</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.ComponentMap
	 * @generated
	 */
	public Adapter createComponentMapAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.ClassMap <em>Class Map</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.ClassMap
	 * @generated
	 */
	public Adapter createClassMapAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.ColumnMap <em>Column Map</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.ColumnMap
	 * @generated
	 */
	public Adapter createColumnMapAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.ReferenceMap <em>Reference Map</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.ReferenceMap
	 * @generated
	 */
	public Adapter createReferenceMapAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.LookupMap <em>Lookup Map</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.LookupMap
	 * @generated
	 */
	public Adapter createLookupMapAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.AggregationMap <em>Aggregation Map</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.AggregationMap
	 * @generated
	 */
	public Adapter createAggregationMapAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.ReferenceKey <em>Reference Key</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.ReferenceKey
	 * @generated
	 */
	public Adapter createReferenceKeyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.Mapping <em>Mapping</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.Mapping
	 * @generated
	 */
	public Adapter createMappingAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link fw2.orm.xlsx.LookupKey <em>Lookup Key</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see fw2.orm.xlsx.LookupKey
	 * @generated
	 */
	public Adapter createLookupKeyAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for the default case.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

} //XlsxAdapterFactory
