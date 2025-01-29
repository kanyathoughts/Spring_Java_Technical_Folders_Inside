/**
 */
package fw2.orm.xlsx.util;

import fw2.orm.xlsx.*;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

/**
 * <!-- begin-user-doc -->
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see fw2.orm.xlsx.XlsxPackage
 * @generated
 */
public class XlsxSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static XlsxPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public XlsxSwitch() {
		if (modelPackage == null) {
			modelPackage = XlsxPackage.eINSTANCE;
		}
	}

	/**
	 * Checks whether this is a switch for the given package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param ePackage the package in question.
	 * @return whether this is a switch for the given package.
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/**
	 * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the first non-null result returned by a <code>caseXXX</code> call.
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case XlsxPackage.MAP_ELEMENT: {
				MapElement mapElement = (MapElement)theEObject;
				T result = caseMapElement(mapElement);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case XlsxPackage.ASSOCIATION_MAP: {
				AssociationMap associationMap = (AssociationMap)theEObject;
				T result = caseAssociationMap(associationMap);
				if (result == null) result = caseMapElement(associationMap);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case XlsxPackage.COMPONENT_MAP: {
				ComponentMap componentMap = (ComponentMap)theEObject;
				T result = caseComponentMap(componentMap);
				if (result == null) result = caseMapElement(componentMap);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case XlsxPackage.CLASS_MAP: {
				ClassMap classMap = (ClassMap)theEObject;
				T result = caseClassMap(classMap);
				if (result == null) result = caseComponentMap(classMap);
				if (result == null) result = caseMapElement(classMap);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case XlsxPackage.COLUMN_MAP: {
				ColumnMap columnMap = (ColumnMap)theEObject;
				T result = caseColumnMap(columnMap);
				if (result == null) result = caseMapElement(columnMap);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case XlsxPackage.REFERENCE_MAP: {
				ReferenceMap referenceMap = (ReferenceMap)theEObject;
				T result = caseReferenceMap(referenceMap);
				if (result == null) result = caseAssociationMap(referenceMap);
				if (result == null) result = caseMapElement(referenceMap);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case XlsxPackage.LOOKUP_MAP: {
				LookupMap lookupMap = (LookupMap)theEObject;
				T result = caseLookupMap(lookupMap);
				if (result == null) result = caseReferenceMap(lookupMap);
				if (result == null) result = caseAssociationMap(lookupMap);
				if (result == null) result = caseMapElement(lookupMap);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case XlsxPackage.AGGREGATION_MAP: {
				AggregationMap aggregationMap = (AggregationMap)theEObject;
				T result = caseAggregationMap(aggregationMap);
				if (result == null) result = caseAssociationMap(aggregationMap);
				if (result == null) result = caseMapElement(aggregationMap);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case XlsxPackage.REFERENCE_KEY: {
				ReferenceKey referenceKey = (ReferenceKey)theEObject;
				T result = caseReferenceKey(referenceKey);
				if (result == null) result = caseMapElement(referenceKey);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case XlsxPackage.MAPPING: {
				Mapping mapping = (Mapping)theEObject;
				T result = caseMapping(mapping);
				if (result == null) result = caseMapElement(mapping);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case XlsxPackage.LOOKUP_KEY: {
				LookupKey lookupKey = (LookupKey)theEObject;
				T result = caseLookupKey(lookupKey);
				if (result == null) result = caseReferenceKey(lookupKey);
				if (result == null) result = caseMapElement(lookupKey);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Map Element</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Map Element</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseMapElement(MapElement object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Association Map</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Association Map</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAssociationMap(AssociationMap object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Component Map</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Component Map</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseComponentMap(ComponentMap object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Class Map</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Class Map</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseClassMap(ClassMap object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Column Map</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Column Map</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseColumnMap(ColumnMap object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Reference Map</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Reference Map</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseReferenceMap(ReferenceMap object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Lookup Map</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Lookup Map</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseLookupMap(LookupMap object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Aggregation Map</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Aggregation Map</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseAggregationMap(AggregationMap object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Reference Key</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Reference Key</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseReferenceKey(ReferenceKey object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Mapping</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Mapping</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseMapping(Mapping object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Lookup Key</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Lookup Key</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseLookupKey(LookupKey object) {
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch, but this is the last case anyway.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

} //XlsxSwitch
