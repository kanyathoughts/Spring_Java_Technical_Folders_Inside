/**
 */
package fw2.model2.util;

import fw2.model2.*;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see fw2.model2.Model2Package
 * @generated
 */
public class Model2AdapterFactory extends AdapterFactoryImpl {
	/**
     * The cached model package.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected static Model2Package modelPackage;

	/**
     * Creates an instance of the adapter factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Model2AdapterFactory() {
        if (modelPackage == null) {
            modelPackage = Model2Package.eINSTANCE;
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
	protected Model2Switch<Adapter> modelSwitch =
		new Model2Switch<Adapter>() {
            @Override
            public Adapter caseAction(Action object) {
                return createActionAdapter();
            }
            @Override
            public Adapter caseAggregation(Aggregation object) {
                return createAggregationAdapter();
            }
            @Override
            public Adapter caseAnnotation(Annotation object) {
                return createAnnotationAdapter();
            }
            @Override
            public Adapter caseApplication(Application object) {
                return createApplicationAdapter();
            }
            @Override
            public Adapter caseAssociation(Association object) {
                return createAssociationAdapter();
            }
            @Override
            public Adapter caseAttribute(Attribute object) {
                return createAttributeAdapter();
            }
            @Override
            public Adapter caseBap(Bap object) {
                return createBapAdapter();
            }
            @Override
            public Adapter caseClassOrm(ClassOrm object) {
                return createClassOrmAdapter();
            }
            @Override
            public Adapter caseComponent(Component object) {
                return createComponentAdapter();
            }
            @Override
            public Adapter caseConstant(Constant object) {
                return createConstantAdapter();
            }
            @Override
            public Adapter caseDbColumn(DbColumn object) {
                return createDbColumnAdapter();
            }
            @Override
            public Adapter caseDbColumnMap(DbColumnMap object) {
                return createDbColumnMapAdapter();
            }
            @Override
            public Adapter caseDbDataSet(DbDataSet object) {
                return createDbDataSetAdapter();
            }
            @Override
            public Adapter caseDbForeignKeyColumn(DbForeignKeyColumn object) {
                return createDbForeignKeyColumnAdapter();
            }
            @Override
            public Adapter caseDbJoinKey(DbJoinKey object) {
                return createDbJoinKeyAdapter();
            }
            @Override
            public Adapter caseDbJoinParameter(DbJoinParameter object) {
                return createDbJoinParameterAdapter();
            }
            @Override
            public Adapter caseDbRelation(DbRelation object) {
                return createDbRelationAdapter();
            }
            @Override
            public Adapter caseDbTable(DbTable object) {
                return createDbTableAdapter();
            }
            @Override
            public Adapter caseDbView(DbView object) {
                return createDbViewAdapter();
            }
            @Override
            public Adapter caseDbViewColumn(DbViewColumn object) {
                return createDbViewColumnAdapter();
            }
            @Override
            public Adapter caseDiscriminatorKey(DiscriminatorKey object) {
                return createDiscriminatorKeyAdapter();
            }
            @Override
            public Adapter caseDomainAttribute(DomainAttribute object) {
                return createDomainAttributeAdapter();
            }
            @Override
            public Adapter caseDomainClass(DomainClass object) {
                return createDomainClassAdapter();
            }
            @Override
            public Adapter caseEditor(Editor object) {
                return createEditorAdapter();
            }
            @Override
            public Adapter caseField(Field object) {
                return createFieldAdapter();
            }
            @Override
            public Adapter caseFieldExtension(FieldExtension object) {
                return createFieldExtensionAdapter();
            }
            @Override
            public Adapter caseFieldGroup(FieldGroup object) {
                return createFieldGroupAdapter();
            }
            @Override
            public Adapter caseFieldGroupExtension(FieldGroupExtension object) {
                return createFieldGroupExtensionAdapter();
            }
            @Override
            public Adapter caseLookup(Lookup object) {
                return createLookupAdapter();
            }
            @Override
            public Adapter caseModel(Model object) {
                return createModelAdapter();
            }
            @Override
            public Adapter caseModelElement(ModelElement object) {
                return createModelElementAdapter();
            }
            @Override
            public Adapter casePrimitive(Primitive object) {
                return createPrimitiveAdapter();
            }
            @Override
            public Adapter casePrimitiveExtension(PrimitiveExtension object) {
                return createPrimitiveExtensionAdapter();
            }
            @Override
            public Adapter caseProperty(Property object) {
                return createPropertyAdapter();
            }
            @Override
            public Adapter caseReference(Reference object) {
                return createReferenceAdapter();
            }
            @Override
            public Adapter caseSummaryTable(SummaryTable object) {
                return createSummaryTableAdapter();
            }
            @Override
            public Adapter caseTableColumn(TableColumn object) {
                return createTableColumnAdapter();
            }
            @Override
            public Adapter caseTag(Tag object) {
                return createTagAdapter();
            }
            @Override
            public Adapter caseTypeExtension(TypeExtension object) {
                return createTypeExtensionAdapter();
            }
            @Override
            public Adapter caseViewComponent(ViewComponent object) {
                return createViewComponentAdapter();
            }
            @Override
            public Adapter caseViewElement(ViewElement object) {
                return createViewElementAdapter();
            }
            @Override
            public Adapter caseViewPrimitive(ViewPrimitive object) {
                return createViewPrimitiveAdapter();
            }
            @Override
            public Adapter caseUiFieldType(UiFieldType object) {
                return createUiFieldTypeAdapter();
            }
            @Override
            public Adapter caseUiFieldTypeProperty(UiFieldTypeProperty object) {
                return createUiFieldTypePropertyAdapter();
            }
            @Override
            public Adapter caseComponentMapping(ComponentMapping object) {
                return createComponentMappingAdapter();
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
     * Creates a new adapter for an object of class '{@link fw2.model2.Action <em>Action</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Action
     * @generated
     */
	public Adapter createActionAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Aggregation <em>Aggregation</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Aggregation
     * @generated
     */
	public Adapter createAggregationAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Annotation <em>Annotation</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Annotation
     * @generated
     */
	public Adapter createAnnotationAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Application <em>Application</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Application
     * @generated
     */
	public Adapter createApplicationAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Association <em>Association</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Association
     * @generated
     */
	public Adapter createAssociationAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Attribute <em>Attribute</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Attribute
     * @generated
     */
	public Adapter createAttributeAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Bap <em>Bap</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Bap
     * @generated
     */
	public Adapter createBapAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.ClassOrm <em>Class Orm</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.ClassOrm
     * @generated
     */
	public Adapter createClassOrmAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Component <em>Component</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Component
     * @generated
     */
	public Adapter createComponentAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Constant <em>Constant</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Constant
     * @generated
     */
	public Adapter createConstantAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DbColumn <em>Db Column</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DbColumn
     * @generated
     */
	public Adapter createDbColumnAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DbColumnMap <em>Db Column Map</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DbColumnMap
     * @generated
     */
	public Adapter createDbColumnMapAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DbDataSet <em>Db Data Set</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DbDataSet
     * @generated
     */
	public Adapter createDbDataSetAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DbForeignKeyColumn <em>Db Foreign Key Column</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DbForeignKeyColumn
     * @generated
     */
	public Adapter createDbForeignKeyColumnAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DbJoinKey <em>Db Join Key</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DbJoinKey
     * @generated
     */
	public Adapter createDbJoinKeyAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DbJoinParameter <em>Db Join Parameter</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DbJoinParameter
     * @generated
     */
	public Adapter createDbJoinParameterAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DbRelation <em>Db Relation</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DbRelation
     * @generated
     */
	public Adapter createDbRelationAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DbTable <em>Db Table</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DbTable
     * @generated
     */
	public Adapter createDbTableAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DbView <em>Db View</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DbView
     * @generated
     */
	public Adapter createDbViewAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DbViewColumn <em>Db View Column</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DbViewColumn
     * @generated
     */
	public Adapter createDbViewColumnAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DiscriminatorKey <em>Discriminator Key</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DiscriminatorKey
     * @generated
     */
	public Adapter createDiscriminatorKeyAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DomainAttribute <em>Domain Attribute</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DomainAttribute
     * @generated
     */
	public Adapter createDomainAttributeAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.DomainClass <em>Domain Class</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.DomainClass
     * @generated
     */
	public Adapter createDomainClassAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Editor <em>Editor</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Editor
     * @generated
     */
	public Adapter createEditorAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Field <em>Field</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Field
     * @generated
     */
	public Adapter createFieldAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.FieldExtension <em>Field Extension</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.FieldExtension
     * @generated
     */
	public Adapter createFieldExtensionAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.FieldGroup <em>Field Group</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.FieldGroup
     * @generated
     */
	public Adapter createFieldGroupAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.FieldGroupExtension <em>Field Group Extension</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.FieldGroupExtension
     * @generated
     */
	public Adapter createFieldGroupExtensionAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Lookup <em>Lookup</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Lookup
     * @generated
     */
	public Adapter createLookupAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Model <em>Model</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Model
     * @generated
     */
	public Adapter createModelAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.ModelElement <em>Model Element</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.ModelElement
     * @generated
     */
	public Adapter createModelElementAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Primitive <em>Primitive</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Primitive
     * @generated
     */
	public Adapter createPrimitiveAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.PrimitiveExtension <em>Primitive Extension</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.PrimitiveExtension
     * @generated
     */
	public Adapter createPrimitiveExtensionAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Property <em>Property</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Property
     * @generated
     */
	public Adapter createPropertyAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Reference <em>Reference</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Reference
     * @generated
     */
	public Adapter createReferenceAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.SummaryTable <em>Summary Table</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.SummaryTable
     * @generated
     */
	public Adapter createSummaryTableAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.TableColumn <em>Table Column</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.TableColumn
     * @generated
     */
	public Adapter createTableColumnAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.Tag <em>Tag</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.Tag
     * @generated
     */
	public Adapter createTagAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.TypeExtension <em>Type Extension</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.TypeExtension
     * @generated
     */
	public Adapter createTypeExtensionAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.ViewComponent <em>View Component</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.ViewComponent
     * @generated
     */
	public Adapter createViewComponentAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.ViewElement <em>View Element</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.ViewElement
     * @generated
     */
	public Adapter createViewElementAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.ViewPrimitive <em>View Primitive</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.ViewPrimitive
     * @generated
     */
	public Adapter createViewPrimitiveAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.UiFieldType <em>Ui Field Type</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.UiFieldType
     * @generated
     */
	public Adapter createUiFieldTypeAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.UiFieldTypeProperty <em>Ui Field Type Property</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.UiFieldTypeProperty
     * @generated
     */
	public Adapter createUiFieldTypePropertyAdapter() {
        return null;
    }

	/**
     * Creates a new adapter for an object of class '{@link fw2.model2.ComponentMapping <em>Component Mapping</em>}'.
     * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
     * @return the new adapter.
     * @see fw2.model2.ComponentMapping
     * @generated
     */
	public Adapter createComponentMappingAdapter() {
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

} //Model2AdapterFactory
