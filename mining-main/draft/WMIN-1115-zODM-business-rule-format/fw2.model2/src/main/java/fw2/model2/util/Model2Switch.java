/**
 */
package fw2.model2.util;

import fw2.model2.*;

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
 * @see fw2.model2.Model2Package
 * @generated
 */
public class Model2Switch<T> extends Switch<T> {
	/**
     * The cached model package
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected static Model2Package modelPackage;

	/**
     * Creates an instance of the switch.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Model2Switch() {
        if (modelPackage == null) {
            modelPackage = Model2Package.eINSTANCE;
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
            case Model2Package.ACTION: {
                Action action = (Action)theEObject;
                T result = caseAction(action);
                if (result == null) result = caseViewPrimitive(action);
                if (result == null) result = caseViewElement(action);
                if (result == null) result = caseModelElement(action);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.AGGREGATION: {
                Aggregation aggregation = (Aggregation)theEObject;
                T result = caseAggregation(aggregation);
                if (result == null) result = caseAssociation(aggregation);
                if (result == null) result = caseAttribute(aggregation);
                if (result == null) result = caseModelElement(aggregation);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.ANNOTATION: {
                Annotation annotation = (Annotation)theEObject;
                T result = caseAnnotation(annotation);
                if (result == null) result = caseModelElement(annotation);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.APPLICATION: {
                Application application = (Application)theEObject;
                T result = caseApplication(application);
                if (result == null) result = caseViewComponent(application);
                if (result == null) result = caseViewElement(application);
                if (result == null) result = caseModelElement(application);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.ASSOCIATION: {
                Association association = (Association)theEObject;
                T result = caseAssociation(association);
                if (result == null) result = caseAttribute(association);
                if (result == null) result = caseModelElement(association);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.ATTRIBUTE: {
                Attribute attribute = (Attribute)theEObject;
                T result = caseAttribute(attribute);
                if (result == null) result = caseModelElement(attribute);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.BAP: {
                Bap bap = (Bap)theEObject;
                T result = caseBap(bap);
                if (result == null) result = caseEditor(bap);
                if (result == null) result = caseViewComponent(bap);
                if (result == null) result = caseViewElement(bap);
                if (result == null) result = caseModelElement(bap);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.CLASS_ORM: {
                ClassOrm classOrm = (ClassOrm)theEObject;
                T result = caseClassOrm(classOrm);
                if (result == null) result = caseComponent(classOrm);
                if (result == null) result = caseModelElement(classOrm);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.COMPONENT: {
                Component component = (Component)theEObject;
                T result = caseComponent(component);
                if (result == null) result = caseModelElement(component);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.CONSTANT: {
                Constant constant = (Constant)theEObject;
                T result = caseConstant(constant);
                if (result == null) result = casePrimitive(constant);
                if (result == null) result = caseAttribute(constant);
                if (result == null) result = caseModelElement(constant);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DB_COLUMN: {
                DbColumn dbColumn = (DbColumn)theEObject;
                T result = caseDbColumn(dbColumn);
                if (result == null) result = caseModelElement(dbColumn);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DB_COLUMN_MAP: {
                DbColumnMap dbColumnMap = (DbColumnMap)theEObject;
                T result = caseDbColumnMap(dbColumnMap);
                if (result == null) result = casePrimitive(dbColumnMap);
                if (result == null) result = caseAttribute(dbColumnMap);
                if (result == null) result = caseModelElement(dbColumnMap);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DB_DATA_SET: {
                DbDataSet dbDataSet = (DbDataSet)theEObject;
                T result = caseDbDataSet(dbDataSet);
                if (result == null) result = caseModelElement(dbDataSet);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DB_FOREIGN_KEY_COLUMN: {
                DbForeignKeyColumn dbForeignKeyColumn = (DbForeignKeyColumn)theEObject;
                T result = caseDbForeignKeyColumn(dbForeignKeyColumn);
                if (result == null) result = caseModelElement(dbForeignKeyColumn);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DB_JOIN_KEY: {
                DbJoinKey dbJoinKey = (DbJoinKey)theEObject;
                T result = caseDbJoinKey(dbJoinKey);
                if (result == null) result = caseModelElement(dbJoinKey);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DB_JOIN_PARAMETER: {
                DbJoinParameter dbJoinParameter = (DbJoinParameter)theEObject;
                T result = caseDbJoinParameter(dbJoinParameter);
                if (result == null) result = caseModelElement(dbJoinParameter);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DB_RELATION: {
                DbRelation dbRelation = (DbRelation)theEObject;
                T result = caseDbRelation(dbRelation);
                if (result == null) result = caseModelElement(dbRelation);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DB_TABLE: {
                DbTable dbTable = (DbTable)theEObject;
                T result = caseDbTable(dbTable);
                if (result == null) result = caseDbDataSet(dbTable);
                if (result == null) result = caseModelElement(dbTable);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DB_VIEW: {
                DbView dbView = (DbView)theEObject;
                T result = caseDbView(dbView);
                if (result == null) result = caseDbDataSet(dbView);
                if (result == null) result = caseModelElement(dbView);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DB_VIEW_COLUMN: {
                DbViewColumn dbViewColumn = (DbViewColumn)theEObject;
                T result = caseDbViewColumn(dbViewColumn);
                if (result == null) result = caseModelElement(dbViewColumn);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DISCRIMINATOR_KEY: {
                DiscriminatorKey discriminatorKey = (DiscriminatorKey)theEObject;
                T result = caseDiscriminatorKey(discriminatorKey);
                if (result == null) result = caseModelElement(discriminatorKey);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DOMAIN_ATTRIBUTE: {
                DomainAttribute domainAttribute = (DomainAttribute)theEObject;
                T result = caseDomainAttribute(domainAttribute);
                if (result == null) result = caseModelElement(domainAttribute);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.DOMAIN_CLASS: {
                DomainClass domainClass = (DomainClass)theEObject;
                T result = caseDomainClass(domainClass);
                if (result == null) result = caseModelElement(domainClass);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.EDITOR: {
                Editor editor = (Editor)theEObject;
                T result = caseEditor(editor);
                if (result == null) result = caseViewComponent(editor);
                if (result == null) result = caseViewElement(editor);
                if (result == null) result = caseModelElement(editor);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.FIELD: {
                Field field = (Field)theEObject;
                T result = caseField(field);
                if (result == null) result = caseViewPrimitive(field);
                if (result == null) result = caseViewElement(field);
                if (result == null) result = caseModelElement(field);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.FIELD_EXTENSION: {
                FieldExtension fieldExtension = (FieldExtension)theEObject;
                T result = caseFieldExtension(fieldExtension);
                if (result == null) result = caseField(fieldExtension);
                if (result == null) result = caseViewPrimitive(fieldExtension);
                if (result == null) result = caseViewElement(fieldExtension);
                if (result == null) result = caseModelElement(fieldExtension);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.FIELD_GROUP: {
                FieldGroup fieldGroup = (FieldGroup)theEObject;
                T result = caseFieldGroup(fieldGroup);
                if (result == null) result = caseViewComponent(fieldGroup);
                if (result == null) result = caseViewElement(fieldGroup);
                if (result == null) result = caseModelElement(fieldGroup);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.FIELD_GROUP_EXTENSION: {
                FieldGroupExtension fieldGroupExtension = (FieldGroupExtension)theEObject;
                T result = caseFieldGroupExtension(fieldGroupExtension);
                if (result == null) result = caseFieldGroup(fieldGroupExtension);
                if (result == null) result = caseViewComponent(fieldGroupExtension);
                if (result == null) result = caseViewElement(fieldGroupExtension);
                if (result == null) result = caseModelElement(fieldGroupExtension);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.LOOKUP: {
                Lookup lookup = (Lookup)theEObject;
                T result = caseLookup(lookup);
                if (result == null) result = caseReference(lookup);
                if (result == null) result = caseAssociation(lookup);
                if (result == null) result = caseAttribute(lookup);
                if (result == null) result = caseModelElement(lookup);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.MODEL: {
                Model model = (Model)theEObject;
                T result = caseModel(model);
                if (result == null) result = caseModelElement(model);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.MODEL_ELEMENT: {
                ModelElement modelElement = (ModelElement)theEObject;
                T result = caseModelElement(modelElement);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.PRIMITIVE: {
                Primitive primitive = (Primitive)theEObject;
                T result = casePrimitive(primitive);
                if (result == null) result = caseAttribute(primitive);
                if (result == null) result = caseModelElement(primitive);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.PRIMITIVE_EXTENSION: {
                PrimitiveExtension primitiveExtension = (PrimitiveExtension)theEObject;
                T result = casePrimitiveExtension(primitiveExtension);
                if (result == null) result = casePrimitive(primitiveExtension);
                if (result == null) result = caseAttribute(primitiveExtension);
                if (result == null) result = caseModelElement(primitiveExtension);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.PROPERTY: {
                Property property = (Property)theEObject;
                T result = caseProperty(property);
                if (result == null) result = caseModelElement(property);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.REFERENCE: {
                Reference reference = (Reference)theEObject;
                T result = caseReference(reference);
                if (result == null) result = caseAssociation(reference);
                if (result == null) result = caseAttribute(reference);
                if (result == null) result = caseModelElement(reference);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.SUMMARY_TABLE: {
                SummaryTable summaryTable = (SummaryTable)theEObject;
                T result = caseSummaryTable(summaryTable);
                if (result == null) result = caseViewComponent(summaryTable);
                if (result == null) result = caseViewElement(summaryTable);
                if (result == null) result = caseModelElement(summaryTable);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.TABLE_COLUMN: {
                TableColumn tableColumn = (TableColumn)theEObject;
                T result = caseTableColumn(tableColumn);
                if (result == null) result = caseViewPrimitive(tableColumn);
                if (result == null) result = caseViewElement(tableColumn);
                if (result == null) result = caseModelElement(tableColumn);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.TAG: {
                Tag tag = (Tag)theEObject;
                T result = caseTag(tag);
                if (result == null) result = caseModelElement(tag);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.TYPE_EXTENSION: {
                TypeExtension typeExtension = (TypeExtension)theEObject;
                T result = caseTypeExtension(typeExtension);
                if (result == null) result = caseModelElement(typeExtension);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.VIEW_COMPONENT: {
                ViewComponent viewComponent = (ViewComponent)theEObject;
                T result = caseViewComponent(viewComponent);
                if (result == null) result = caseViewElement(viewComponent);
                if (result == null) result = caseModelElement(viewComponent);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.VIEW_ELEMENT: {
                ViewElement viewElement = (ViewElement)theEObject;
                T result = caseViewElement(viewElement);
                if (result == null) result = caseModelElement(viewElement);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.VIEW_PRIMITIVE: {
                ViewPrimitive viewPrimitive = (ViewPrimitive)theEObject;
                T result = caseViewPrimitive(viewPrimitive);
                if (result == null) result = caseViewElement(viewPrimitive);
                if (result == null) result = caseModelElement(viewPrimitive);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.UI_FIELD_TYPE: {
                UiFieldType uiFieldType = (UiFieldType)theEObject;
                T result = caseUiFieldType(uiFieldType);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.UI_FIELD_TYPE_PROPERTY: {
                UiFieldTypeProperty uiFieldTypeProperty = (UiFieldTypeProperty)theEObject;
                T result = caseUiFieldTypeProperty(uiFieldTypeProperty);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            case Model2Package.COMPONENT_MAPPING: {
                ComponentMapping componentMapping = (ComponentMapping)theEObject;
                T result = caseComponentMapping(componentMapping);
                if (result == null) result = defaultCase(theEObject);
                return result;
            }
            default: return defaultCase(theEObject);
        }
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Action</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Action</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseAction(Action object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Aggregation</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Aggregation</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseAggregation(Aggregation object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Annotation</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Annotation</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseAnnotation(Annotation object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Application</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Application</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseApplication(Application object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Association</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Association</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseAssociation(Association object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Attribute</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Attribute</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseAttribute(Attribute object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Bap</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Bap</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseBap(Bap object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Class Orm</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Class Orm</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseClassOrm(ClassOrm object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Component</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Component</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseComponent(Component object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Constant</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Constant</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseConstant(Constant object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Db Column</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Db Column</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDbColumn(DbColumn object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Db Column Map</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Db Column Map</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDbColumnMap(DbColumnMap object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Db Data Set</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Db Data Set</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDbDataSet(DbDataSet object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Db Foreign Key Column</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Db Foreign Key Column</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDbForeignKeyColumn(DbForeignKeyColumn object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Db Join Key</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Db Join Key</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDbJoinKey(DbJoinKey object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Db Join Parameter</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Db Join Parameter</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDbJoinParameter(DbJoinParameter object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Db Relation</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Db Relation</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDbRelation(DbRelation object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Db Table</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Db Table</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDbTable(DbTable object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Db View</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Db View</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDbView(DbView object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Db View Column</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Db View Column</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDbViewColumn(DbViewColumn object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Discriminator Key</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Discriminator Key</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDiscriminatorKey(DiscriminatorKey object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Domain Attribute</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Domain Attribute</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDomainAttribute(DomainAttribute object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Domain Class</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Domain Class</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseDomainClass(DomainClass object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Editor</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Editor</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseEditor(Editor object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Field</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Field</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseField(Field object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Field Extension</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Field Extension</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseFieldExtension(FieldExtension object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Field Group</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Field Group</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseFieldGroup(FieldGroup object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Field Group Extension</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Field Group Extension</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseFieldGroupExtension(FieldGroupExtension object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Lookup</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Lookup</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseLookup(Lookup object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Model</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Model</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseModel(Model object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Model Element</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Model Element</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseModelElement(ModelElement object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Primitive</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Primitive</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T casePrimitive(Primitive object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Primitive Extension</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Primitive Extension</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T casePrimitiveExtension(PrimitiveExtension object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Property</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Property</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseProperty(Property object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Reference</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Reference</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseReference(Reference object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Summary Table</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Summary Table</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseSummaryTable(SummaryTable object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Table Column</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Table Column</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseTableColumn(TableColumn object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Tag</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Tag</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseTag(Tag object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Type Extension</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Type Extension</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseTypeExtension(TypeExtension object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>View Component</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>View Component</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseViewComponent(ViewComponent object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>View Element</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>View Element</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseViewElement(ViewElement object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>View Primitive</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>View Primitive</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseViewPrimitive(ViewPrimitive object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Ui Field Type</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Ui Field Type</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseUiFieldType(UiFieldType object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Ui Field Type Property</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Ui Field Type Property</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseUiFieldTypeProperty(UiFieldTypeProperty object) {
        return null;
    }

	/**
     * Returns the result of interpreting the object as an instance of '<em>Component Mapping</em>'.
     * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
     * @param object the target of the switch.
     * @return the result of interpreting the object as an instance of '<em>Component Mapping</em>'.
     * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
     * @generated
     */
	public T caseComponentMapping(ComponentMapping object) {
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

} //Model2Switch
