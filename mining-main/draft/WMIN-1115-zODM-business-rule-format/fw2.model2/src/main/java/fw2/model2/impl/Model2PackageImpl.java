/**
 */
package fw2.model2.impl;

import fw2.model2.Action;
import fw2.model2.Aggregation;
import fw2.model2.Annotation;
import fw2.model2.Application;
import fw2.model2.Association;
import fw2.model2.Attribute;
import fw2.model2.Bap;
import fw2.model2.CardinalityTypeEnum;
import fw2.model2.ClassOrm;
import fw2.model2.Component;
import fw2.model2.ComponentMapping;
import fw2.model2.Constant;
import fw2.model2.DbColumn;
import fw2.model2.DbColumnMap;
import fw2.model2.DbColumnTypeEnum;
import fw2.model2.DbDataSet;
import fw2.model2.DbForeignKeyColumn;
import fw2.model2.DbJoinKey;
import fw2.model2.DbJoinParameter;
import fw2.model2.DbRelation;
import fw2.model2.DbTable;
import fw2.model2.DbView;
import fw2.model2.DbViewColumn;
import fw2.model2.DiscriminatorKey;
import fw2.model2.DomainAttribute;
import fw2.model2.DomainClass;
import fw2.model2.Editor;
import fw2.model2.Field;
import fw2.model2.FieldExtension;
import fw2.model2.FieldGroup;
import fw2.model2.FieldGroupExtension;
import fw2.model2.FieldTypeEnum;
import fw2.model2.Lookup;
import fw2.model2.Model;
import fw2.model2.Model2Factory;
import fw2.model2.Model2Package;
import fw2.model2.ModelElement;
import fw2.model2.Primitive;
import fw2.model2.PrimitiveExtension;
import fw2.model2.Property;
import fw2.model2.Reference;
import fw2.model2.SummaryTable;
import fw2.model2.TableColumn;
import fw2.model2.Tag;
import fw2.model2.TypeExtension;
import fw2.model2.UiFieldType;
import fw2.model2.UiFieldTypeProperty;
import fw2.model2.ViewComponent;
import fw2.model2.ViewElement;
import fw2.model2.ViewPrimitive;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.emf.ecore.xml.type.XMLTypePackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class Model2PackageImpl extends EPackageImpl implements Model2Package {
	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass actionEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass aggregationEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass annotationEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass applicationEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass associationEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass attributeEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass bapEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass classOrmEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass componentEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass constantEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass dbColumnEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass dbColumnMapEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass dbDataSetEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass dbForeignKeyColumnEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass dbJoinKeyEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass dbJoinParameterEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass dbRelationEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass dbTableEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass dbViewEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass dbViewColumnEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass discriminatorKeyEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass domainAttributeEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass domainClassEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass editorEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass fieldEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass fieldExtensionEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass fieldGroupEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass fieldGroupExtensionEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass lookupEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass modelEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass modelElementEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass primitiveEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass primitiveExtensionEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass propertyEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass referenceEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass summaryTableEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass tableColumnEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass tagEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass typeExtensionEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass viewComponentEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass viewElementEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass viewPrimitiveEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass uiFieldTypeEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass uiFieldTypePropertyEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass componentMappingEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EEnum cardinalityTypeEnumEEnum = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EEnum dbColumnTypeEnumEEnum = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EEnum fieldTypeEnumEEnum = null;

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
     * @see fw2.model2.Model2Package#eNS_URI
     * @see #init()
     * @generated
     */
	private Model2PackageImpl() {
        super(eNS_URI, Model2Factory.eINSTANCE);
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
     * <p>This method is used to initialize {@link Model2Package#eINSTANCE} when that field is accessed.
     * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #eNS_URI
     * @see #createPackageContents()
     * @see #initializePackageContents()
     * @generated
     */
	public static Model2Package init() {
        if (isInited) return (Model2Package)EPackage.Registry.INSTANCE.getEPackage(Model2Package.eNS_URI);

        // Obtain or create and register package
        Model2PackageImpl theModel2Package = (Model2PackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof Model2PackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new Model2PackageImpl());

        isInited = true;

        // Initialize simple dependencies
        XMLTypePackage.eINSTANCE.eClass();

        // Create package meta-data objects
        theModel2Package.createPackageContents();

        // Initialize created meta-data
        theModel2Package.initializePackageContents();

        // Mark meta-data to indicate it can't be changed
        theModel2Package.freeze();

  
        // Update the registry and return the package
        EPackage.Registry.INSTANCE.put(Model2Package.eNS_URI, theModel2Package);
        return theModel2Package;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getAction() {
        return actionEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getAction_AccessKey() {
        return (EAttribute)actionEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getAction_Url() {
        return (EAttribute)actionEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getAggregation() {
        return aggregationEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getAggregation_Component() {
        return (EReference)aggregationEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getAnnotation() {
        return annotationEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getAnnotation_Tags() {
        return (EReference)annotationEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getApplication() {
        return applicationEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getApplication_Baps() {
        return (EReference)applicationEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getAssociation() {
        return associationEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getAssociation_Cardinality() {
        return (EAttribute)associationEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getAssociation_RelatedClassOrm() {
        return (EReference)associationEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getAttribute() {
        return attributeEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getBap() {
        return bapEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getBap_ComponentMapping() {
        return (EReference)bapEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getClassOrm() {
        return classOrmEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getClassOrm_Aggregations() {
        return (EReference)classOrmEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getClassOrm_DataSet() {
        return (EReference)classOrmEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getClassOrm_Key() {
        return (EReference)classOrmEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getClassOrm_References() {
        return (EReference)classOrmEClass.getEStructuralFeatures().get(3);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getComponent() {
        return componentEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getComponent_Lookups() {
        return (EReference)componentEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getComponent_Primitives() {
        return (EReference)componentEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getConstant() {
        return constantEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getConstant_Type() {
        return (EAttribute)constantEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getConstant_Value() {
        return (EAttribute)constantEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDbColumn() {
        return dbColumnEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbColumn_Default() {
        return (EAttribute)dbColumnEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbColumn_KeySeq() {
        return (EAttribute)dbColumnEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbColumn_Length() {
        return (EAttribute)dbColumnEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbColumn_Nullable() {
        return (EAttribute)dbColumnEClass.getEStructuralFeatures().get(3);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbColumn_Scale() {
        return (EAttribute)dbColumnEClass.getEStructuralFeatures().get(4);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbColumn_Seq() {
        return (EAttribute)dbColumnEClass.getEStructuralFeatures().get(5);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbColumn_Size() {
        return (EAttribute)dbColumnEClass.getEStructuralFeatures().get(6);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbColumn_Type() {
        return (EAttribute)dbColumnEClass.getEStructuralFeatures().get(7);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDbColumn_Table() {
        return (EReference)dbColumnEClass.getEStructuralFeatures().get(8);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDbColumnMap() {
        return dbColumnMapEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbColumnMap_DbColumnName() {
        return (EAttribute)dbColumnMapEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDbDataSet() {
        return dbDataSetEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDbForeignKeyColumn() {
        return dbForeignKeyColumnEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbForeignKeyColumn_KeySeq() {
        return (EAttribute)dbForeignKeyColumnEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDbForeignKeyColumn_Seq() {
        return (EAttribute)dbForeignKeyColumnEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDbJoinKey() {
        return dbJoinKeyEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDbJoinParameter() {
        return dbJoinParameterEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDbRelation() {
        return dbRelationEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDbRelation_ForeignKeyColumns() {
        return (EReference)dbRelationEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDbRelation_RefTable() {
        return (EReference)dbRelationEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDbRelation_Table() {
        return (EReference)dbRelationEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDbTable() {
        return dbTableEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDbTable_Columns() {
        return (EReference)dbTableEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDbView() {
        return dbViewEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDbView_JoinKeys() {
        return (EReference)dbViewEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDbView_JoinParameters() {
        return (EReference)dbViewEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDbView_ViewColumns() {
        return (EReference)dbViewEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDbViewColumn() {
        return dbViewColumnEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDiscriminatorKey() {
        return discriminatorKeyEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDiscriminatorKey_DiscriminatorValue() {
        return (EAttribute)discriminatorKeyEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDomainAttribute() {
        return domainAttributeEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDomainAttribute_DataType() {
        return (EReference)domainAttributeEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDomainAttribute_Multivalued() {
        return (EAttribute)domainAttributeEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDomainAttribute_Reference() {
        return (EAttribute)domainAttributeEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDomainClass() {
        return domainClassEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDomainClass_Superclass() {
        return (EReference)domainClassEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDomainClass_PlatformClass() {
        return (EReference)domainClassEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getDomainClass_Attributes() {
        return (EReference)domainClassEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDomainClass_Abstract() {
        return (EAttribute)domainClassEClass.getEStructuralFeatures().get(3);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDomainClass_Primitive() {
        return (EAttribute)domainClassEClass.getEStructuralFeatures().get(4);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getEditor() {
        return editorEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getEditor_Editors() {
        return (EReference)editorEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getEditor_FieldGroups() {
        return (EReference)editorEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getEditor_SummaryTables() {
        return (EReference)editorEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getEditor_PresentInStaging() {
        return (EAttribute)editorEClass.getEStructuralFeatures().get(3);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getField() {
        return fieldEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getField_Mandatory() {
        return (EAttribute)fieldEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getField_MaxLength() {
        return (EAttribute)fieldEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getField_ReferenceListName() {
        return (EAttribute)fieldEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getField_RowIndex() {
        return (EAttribute)fieldEClass.getEStructuralFeatures().get(3);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getField_SelectDisabled() {
        return (EAttribute)fieldEClass.getEStructuralFeatures().get(4);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getField_SelectHeaderValue() {
        return (EAttribute)fieldEClass.getEStructuralFeatures().get(5);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getField_Size() {
        return (EAttribute)fieldEClass.getEStructuralFeatures().get(6);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getField_PresentInStaging() {
        return (EAttribute)fieldEClass.getEStructuralFeatures().get(7);
    }

	/**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getField_Comments() {
        return (EAttribute)fieldEClass.getEStructuralFeatures().get(8);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getField_Mapping() {
        return (EAttribute)fieldEClass.getEStructuralFeatures().get(9);
    }

    /**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getFieldExtension() {
        return fieldExtensionEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getFieldExtension_Extension() {
        return (EReference)fieldExtensionEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getFieldGroup() {
        return fieldGroupEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getFieldGroup_Actions() {
        return (EReference)fieldGroupEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getFieldGroup_Fields() {
        return (EReference)fieldGroupEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getFieldGroup_DisplayOrder() {
        return (EAttribute)fieldGroupEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getFieldGroup_PresentInStaging() {
        return (EAttribute)fieldGroupEClass.getEStructuralFeatures().get(3);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getFieldGroupExtension() {
        return fieldGroupExtensionEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getFieldGroupExtension_Extension() {
        return (EReference)fieldGroupExtensionEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getLookup() {
        return lookupEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getModel() {
        return modelEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getModel_Elements() {
        return (EReference)modelEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getModelElement() {
        return modelElementEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getModelElement_Annotations() {
        return (EReference)modelElementEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getModelElement_Name() {
        return (EAttribute)modelElementEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getPrimitive() {
        return primitiveEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getPrimitiveExtension() {
        return primitiveExtensionEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getPrimitiveExtension_Extension() {
        return (EReference)primitiveExtensionEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getProperty() {
        return propertyEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getProperty_Value() {
        return (EReference)propertyEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getReference() {
        return referenceEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getReference_Relation() {
        return (EReference)referenceEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getSummaryTable() {
        return summaryTableEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getSummaryTable_Columns() {
        return (EReference)summaryTableEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getSummaryTable_DomainWrapper() {
        return (EAttribute)summaryTableEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getSummaryTable_BapName() {
        return (EAttribute)summaryTableEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getTableColumn() {
        return tableColumnEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTableColumn_HeaderValue() {
        return (EAttribute)tableColumnEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTableColumn_Index() {
        return (EAttribute)tableColumnEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTableColumn_Property() {
        return (EAttribute)tableColumnEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTableColumn_ReferenceListName() {
        return (EAttribute)tableColumnEClass.getEStructuralFeatures().get(3);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTableColumn_SelectDisabled() {
        return (EAttribute)tableColumnEClass.getEStructuralFeatures().get(4);
    }

	/**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getTableColumn_Comments() {
        return (EAttribute)tableColumnEClass.getEStructuralFeatures().get(5);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getTableColumn_Mapping() {
        return (EAttribute)tableColumnEClass.getEStructuralFeatures().get(6);
    }

    /**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getTag() {
        return tagEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTag_Type() {
        return (EAttribute)tagEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTag_Value() {
        return (EAttribute)tagEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getTypeExtension() {
        return typeExtensionEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getTypeExtension_DomainClass() {
        return (EReference)typeExtensionEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getTypeExtension_Properties() {
        return (EReference)typeExtensionEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getViewComponent() {
        return viewComponentEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getViewComponent_ComponentId() {
        return (EAttribute)viewComponentEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getViewElement() {
        return viewElementEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getViewElement_Id() {
        return (EAttribute)viewElementEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getViewElement_Label() {
        return (EAttribute)viewElementEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getViewElement_Type() {
        return (EAttribute)viewElementEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getViewPrimitive() {
        return viewPrimitiveEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getUiFieldType() {
        return uiFieldTypeEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getUiFieldType_DataType() {
        return (EAttribute)uiFieldTypeEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getUiFieldType_HtmlType() {
        return (EAttribute)uiFieldTypeEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getUiFieldType_OnScreenValidationMethod() {
        return (EAttribute)uiFieldTypeEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getUiFieldType_Properties() {
        return (EReference)uiFieldTypeEClass.getEStructuralFeatures().get(3);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getUiFieldTypeProperty() {
        return uiFieldTypePropertyEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getUiFieldTypeProperty_PropertyName() {
        return (EAttribute)uiFieldTypePropertyEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getUiFieldTypeProperty_PropertyType() {
        return (EAttribute)uiFieldTypePropertyEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getUiFieldTypeProperty_TypeName() {
        return (EAttribute)uiFieldTypePropertyEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getComponentMapping() {
        return componentMappingEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getComponentMapping_ClassName() {
        return (EAttribute)componentMappingEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getComponentMapping_ComponentId() {
        return (EAttribute)componentMappingEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getComponentMapping_Id() {
        return (EAttribute)componentMappingEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getComponentMapping_TypeCode() {
        return (EAttribute)componentMappingEClass.getEStructuralFeatures().get(3);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EEnum getCardinalityTypeEnum() {
        return cardinalityTypeEnumEEnum;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EEnum getDbColumnTypeEnum() {
        return dbColumnTypeEnumEEnum;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EEnum getFieldTypeEnum() {
        return fieldTypeEnumEEnum;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Model2Factory getModel2Factory() {
        return (Model2Factory)getEFactoryInstance();
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
        actionEClass = createEClass(ACTION);
        createEAttribute(actionEClass, ACTION__ACCESS_KEY);
        createEAttribute(actionEClass, ACTION__URL);

        aggregationEClass = createEClass(AGGREGATION);
        createEReference(aggregationEClass, AGGREGATION__COMPONENT);

        annotationEClass = createEClass(ANNOTATION);
        createEReference(annotationEClass, ANNOTATION__TAGS);

        applicationEClass = createEClass(APPLICATION);
        createEReference(applicationEClass, APPLICATION__BAPS);

        associationEClass = createEClass(ASSOCIATION);
        createEAttribute(associationEClass, ASSOCIATION__CARDINALITY);
        createEReference(associationEClass, ASSOCIATION__RELATED_CLASS_ORM);

        attributeEClass = createEClass(ATTRIBUTE);

        bapEClass = createEClass(BAP);
        createEReference(bapEClass, BAP__COMPONENT_MAPPING);

        classOrmEClass = createEClass(CLASS_ORM);
        createEReference(classOrmEClass, CLASS_ORM__AGGREGATIONS);
        createEReference(classOrmEClass, CLASS_ORM__DATA_SET);
        createEReference(classOrmEClass, CLASS_ORM__KEY);
        createEReference(classOrmEClass, CLASS_ORM__REFERENCES);

        componentEClass = createEClass(COMPONENT);
        createEReference(componentEClass, COMPONENT__LOOKUPS);
        createEReference(componentEClass, COMPONENT__PRIMITIVES);

        constantEClass = createEClass(CONSTANT);
        createEAttribute(constantEClass, CONSTANT__TYPE);
        createEAttribute(constantEClass, CONSTANT__VALUE);

        dbColumnEClass = createEClass(DB_COLUMN);
        createEAttribute(dbColumnEClass, DB_COLUMN__DEFAULT);
        createEAttribute(dbColumnEClass, DB_COLUMN__KEY_SEQ);
        createEAttribute(dbColumnEClass, DB_COLUMN__LENGTH);
        createEAttribute(dbColumnEClass, DB_COLUMN__NULLABLE);
        createEAttribute(dbColumnEClass, DB_COLUMN__SCALE);
        createEAttribute(dbColumnEClass, DB_COLUMN__SEQ);
        createEAttribute(dbColumnEClass, DB_COLUMN__SIZE);
        createEAttribute(dbColumnEClass, DB_COLUMN__TYPE);
        createEReference(dbColumnEClass, DB_COLUMN__TABLE);

        dbColumnMapEClass = createEClass(DB_COLUMN_MAP);
        createEAttribute(dbColumnMapEClass, DB_COLUMN_MAP__DB_COLUMN_NAME);

        dbDataSetEClass = createEClass(DB_DATA_SET);

        dbForeignKeyColumnEClass = createEClass(DB_FOREIGN_KEY_COLUMN);
        createEAttribute(dbForeignKeyColumnEClass, DB_FOREIGN_KEY_COLUMN__KEY_SEQ);
        createEAttribute(dbForeignKeyColumnEClass, DB_FOREIGN_KEY_COLUMN__SEQ);

        dbJoinKeyEClass = createEClass(DB_JOIN_KEY);

        dbJoinParameterEClass = createEClass(DB_JOIN_PARAMETER);

        dbRelationEClass = createEClass(DB_RELATION);
        createEReference(dbRelationEClass, DB_RELATION__FOREIGN_KEY_COLUMNS);
        createEReference(dbRelationEClass, DB_RELATION__REF_TABLE);
        createEReference(dbRelationEClass, DB_RELATION__TABLE);

        dbTableEClass = createEClass(DB_TABLE);
        createEReference(dbTableEClass, DB_TABLE__COLUMNS);

        dbViewEClass = createEClass(DB_VIEW);
        createEReference(dbViewEClass, DB_VIEW__JOIN_KEYS);
        createEReference(dbViewEClass, DB_VIEW__JOIN_PARAMETERS);
        createEReference(dbViewEClass, DB_VIEW__VIEW_COLUMNS);

        dbViewColumnEClass = createEClass(DB_VIEW_COLUMN);

        discriminatorKeyEClass = createEClass(DISCRIMINATOR_KEY);
        createEAttribute(discriminatorKeyEClass, DISCRIMINATOR_KEY__DISCRIMINATOR_VALUE);

        domainAttributeEClass = createEClass(DOMAIN_ATTRIBUTE);
        createEReference(domainAttributeEClass, DOMAIN_ATTRIBUTE__DATA_TYPE);
        createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__MULTIVALUED);
        createEAttribute(domainAttributeEClass, DOMAIN_ATTRIBUTE__REFERENCE);

        domainClassEClass = createEClass(DOMAIN_CLASS);
        createEReference(domainClassEClass, DOMAIN_CLASS__SUPERCLASS);
        createEReference(domainClassEClass, DOMAIN_CLASS__PLATFORM_CLASS);
        createEReference(domainClassEClass, DOMAIN_CLASS__ATTRIBUTES);
        createEAttribute(domainClassEClass, DOMAIN_CLASS__ABSTRACT);
        createEAttribute(domainClassEClass, DOMAIN_CLASS__PRIMITIVE);

        editorEClass = createEClass(EDITOR);
        createEReference(editorEClass, EDITOR__EDITORS);
        createEReference(editorEClass, EDITOR__FIELD_GROUPS);
        createEReference(editorEClass, EDITOR__SUMMARY_TABLES);
        createEAttribute(editorEClass, EDITOR__PRESENT_IN_STAGING);

        fieldEClass = createEClass(FIELD);
        createEAttribute(fieldEClass, FIELD__MANDATORY);
        createEAttribute(fieldEClass, FIELD__MAX_LENGTH);
        createEAttribute(fieldEClass, FIELD__REFERENCE_LIST_NAME);
        createEAttribute(fieldEClass, FIELD__ROW_INDEX);
        createEAttribute(fieldEClass, FIELD__SELECT_DISABLED);
        createEAttribute(fieldEClass, FIELD__SELECT_HEADER_VALUE);
        createEAttribute(fieldEClass, FIELD__SIZE);
        createEAttribute(fieldEClass, FIELD__PRESENT_IN_STAGING);
        createEAttribute(fieldEClass, FIELD__COMMENTS);
        createEAttribute(fieldEClass, FIELD__MAPPING);

        fieldExtensionEClass = createEClass(FIELD_EXTENSION);
        createEReference(fieldExtensionEClass, FIELD_EXTENSION__EXTENSION);

        fieldGroupEClass = createEClass(FIELD_GROUP);
        createEReference(fieldGroupEClass, FIELD_GROUP__ACTIONS);
        createEReference(fieldGroupEClass, FIELD_GROUP__FIELDS);
        createEAttribute(fieldGroupEClass, FIELD_GROUP__DISPLAY_ORDER);
        createEAttribute(fieldGroupEClass, FIELD_GROUP__PRESENT_IN_STAGING);

        fieldGroupExtensionEClass = createEClass(FIELD_GROUP_EXTENSION);
        createEReference(fieldGroupExtensionEClass, FIELD_GROUP_EXTENSION__EXTENSION);

        lookupEClass = createEClass(LOOKUP);

        modelEClass = createEClass(MODEL);
        createEReference(modelEClass, MODEL__ELEMENTS);

        modelElementEClass = createEClass(MODEL_ELEMENT);
        createEReference(modelElementEClass, MODEL_ELEMENT__ANNOTATIONS);
        createEAttribute(modelElementEClass, MODEL_ELEMENT__NAME);

        primitiveEClass = createEClass(PRIMITIVE);

        primitiveExtensionEClass = createEClass(PRIMITIVE_EXTENSION);
        createEReference(primitiveExtensionEClass, PRIMITIVE_EXTENSION__EXTENSION);

        propertyEClass = createEClass(PROPERTY);
        createEReference(propertyEClass, PROPERTY__VALUE);

        referenceEClass = createEClass(REFERENCE);
        createEReference(referenceEClass, REFERENCE__RELATION);

        summaryTableEClass = createEClass(SUMMARY_TABLE);
        createEReference(summaryTableEClass, SUMMARY_TABLE__COLUMNS);
        createEAttribute(summaryTableEClass, SUMMARY_TABLE__DOMAIN_WRAPPER);
        createEAttribute(summaryTableEClass, SUMMARY_TABLE__BAP_NAME);

        tableColumnEClass = createEClass(TABLE_COLUMN);
        createEAttribute(tableColumnEClass, TABLE_COLUMN__HEADER_VALUE);
        createEAttribute(tableColumnEClass, TABLE_COLUMN__INDEX);
        createEAttribute(tableColumnEClass, TABLE_COLUMN__PROPERTY);
        createEAttribute(tableColumnEClass, TABLE_COLUMN__REFERENCE_LIST_NAME);
        createEAttribute(tableColumnEClass, TABLE_COLUMN__SELECT_DISABLED);
        createEAttribute(tableColumnEClass, TABLE_COLUMN__COMMENTS);
        createEAttribute(tableColumnEClass, TABLE_COLUMN__MAPPING);

        tagEClass = createEClass(TAG);
        createEAttribute(tagEClass, TAG__TYPE);
        createEAttribute(tagEClass, TAG__VALUE);

        typeExtensionEClass = createEClass(TYPE_EXTENSION);
        createEReference(typeExtensionEClass, TYPE_EXTENSION__DOMAIN_CLASS);
        createEReference(typeExtensionEClass, TYPE_EXTENSION__PROPERTIES);

        viewComponentEClass = createEClass(VIEW_COMPONENT);
        createEAttribute(viewComponentEClass, VIEW_COMPONENT__COMPONENT_ID);

        viewElementEClass = createEClass(VIEW_ELEMENT);
        createEAttribute(viewElementEClass, VIEW_ELEMENT__ID);
        createEAttribute(viewElementEClass, VIEW_ELEMENT__LABEL);
        createEAttribute(viewElementEClass, VIEW_ELEMENT__TYPE);

        viewPrimitiveEClass = createEClass(VIEW_PRIMITIVE);

        uiFieldTypeEClass = createEClass(UI_FIELD_TYPE);
        createEAttribute(uiFieldTypeEClass, UI_FIELD_TYPE__DATA_TYPE);
        createEAttribute(uiFieldTypeEClass, UI_FIELD_TYPE__HTML_TYPE);
        createEAttribute(uiFieldTypeEClass, UI_FIELD_TYPE__ON_SCREEN_VALIDATION_METHOD);
        createEReference(uiFieldTypeEClass, UI_FIELD_TYPE__PROPERTIES);

        uiFieldTypePropertyEClass = createEClass(UI_FIELD_TYPE_PROPERTY);
        createEAttribute(uiFieldTypePropertyEClass, UI_FIELD_TYPE_PROPERTY__PROPERTY_NAME);
        createEAttribute(uiFieldTypePropertyEClass, UI_FIELD_TYPE_PROPERTY__PROPERTY_TYPE);
        createEAttribute(uiFieldTypePropertyEClass, UI_FIELD_TYPE_PROPERTY__TYPE_NAME);

        componentMappingEClass = createEClass(COMPONENT_MAPPING);
        createEAttribute(componentMappingEClass, COMPONENT_MAPPING__CLASS_NAME);
        createEAttribute(componentMappingEClass, COMPONENT_MAPPING__COMPONENT_ID);
        createEAttribute(componentMappingEClass, COMPONENT_MAPPING__ID);
        createEAttribute(componentMappingEClass, COMPONENT_MAPPING__TYPE_CODE);

        // Create enums
        cardinalityTypeEnumEEnum = createEEnum(CARDINALITY_TYPE_ENUM);
        dbColumnTypeEnumEEnum = createEEnum(DB_COLUMN_TYPE_ENUM);
        fieldTypeEnumEEnum = createEEnum(FIELD_TYPE_ENUM);
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

        // Obtain other dependent packages
        XMLTypePackage theXMLTypePackage = (XMLTypePackage)EPackage.Registry.INSTANCE.getEPackage(XMLTypePackage.eNS_URI);

        // Create type parameters

        // Set bounds for type parameters

        // Add supertypes to classes
        actionEClass.getESuperTypes().add(this.getViewPrimitive());
        aggregationEClass.getESuperTypes().add(this.getAssociation());
        annotationEClass.getESuperTypes().add(this.getModelElement());
        applicationEClass.getESuperTypes().add(this.getViewComponent());
        associationEClass.getESuperTypes().add(this.getAttribute());
        attributeEClass.getESuperTypes().add(this.getModelElement());
        bapEClass.getESuperTypes().add(this.getEditor());
        classOrmEClass.getESuperTypes().add(this.getComponent());
        componentEClass.getESuperTypes().add(this.getModelElement());
        constantEClass.getESuperTypes().add(this.getPrimitive());
        dbColumnEClass.getESuperTypes().add(this.getModelElement());
        dbColumnMapEClass.getESuperTypes().add(this.getPrimitive());
        dbDataSetEClass.getESuperTypes().add(this.getModelElement());
        dbForeignKeyColumnEClass.getESuperTypes().add(this.getModelElement());
        dbJoinKeyEClass.getESuperTypes().add(this.getModelElement());
        dbJoinParameterEClass.getESuperTypes().add(this.getModelElement());
        dbRelationEClass.getESuperTypes().add(this.getModelElement());
        dbTableEClass.getESuperTypes().add(this.getDbDataSet());
        dbViewEClass.getESuperTypes().add(this.getDbDataSet());
        dbViewColumnEClass.getESuperTypes().add(this.getModelElement());
        discriminatorKeyEClass.getESuperTypes().add(this.getModelElement());
        domainAttributeEClass.getESuperTypes().add(this.getModelElement());
        domainClassEClass.getESuperTypes().add(this.getModelElement());
        editorEClass.getESuperTypes().add(this.getViewComponent());
        fieldEClass.getESuperTypes().add(this.getViewPrimitive());
        fieldExtensionEClass.getESuperTypes().add(this.getField());
        fieldGroupEClass.getESuperTypes().add(this.getViewComponent());
        fieldGroupExtensionEClass.getESuperTypes().add(this.getFieldGroup());
        lookupEClass.getESuperTypes().add(this.getReference());
        modelEClass.getESuperTypes().add(this.getModelElement());
        primitiveEClass.getESuperTypes().add(this.getAttribute());
        primitiveExtensionEClass.getESuperTypes().add(this.getPrimitive());
        propertyEClass.getESuperTypes().add(this.getModelElement());
        referenceEClass.getESuperTypes().add(this.getAssociation());
        summaryTableEClass.getESuperTypes().add(this.getViewComponent());
        tableColumnEClass.getESuperTypes().add(this.getViewPrimitive());
        tagEClass.getESuperTypes().add(this.getModelElement());
        typeExtensionEClass.getESuperTypes().add(this.getModelElement());
        viewComponentEClass.getESuperTypes().add(this.getViewElement());
        viewElementEClass.getESuperTypes().add(this.getModelElement());
        viewPrimitiveEClass.getESuperTypes().add(this.getViewElement());

        // Initialize classes, features, and operations; add parameters
        initEClass(actionEClass, Action.class, "Action", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getAction_AccessKey(), theXMLTypePackage.getString(), "accessKey", null, 1, 1, Action.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getAction_Url(), theXMLTypePackage.getString(), "url", null, 1, 1, Action.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(aggregationEClass, Aggregation.class, "Aggregation", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getAggregation_Component(), this.getComponent(), null, "component", null, 1, 1, Aggregation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(annotationEClass, Annotation.class, "Annotation", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getAnnotation_Tags(), this.getTag(), null, "tags", null, 0, -1, Annotation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(applicationEClass, Application.class, "Application", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getApplication_Baps(), this.getBap(), null, "baps", null, 0, -1, Application.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(associationEClass, Association.class, "Association", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getAssociation_Cardinality(), this.getCardinalityTypeEnum(), "cardinality", null, 0, 1, Association.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getAssociation_RelatedClassOrm(), this.getClassOrm(), null, "relatedClassOrm", null, 0, 1, Association.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(attributeEClass, Attribute.class, "Attribute", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(bapEClass, Bap.class, "Bap", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getBap_ComponentMapping(), this.getComponentMapping(), null, "componentMapping", null, 0, -1, Bap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(classOrmEClass, ClassOrm.class, "ClassOrm", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getClassOrm_Aggregations(), this.getAggregation(), null, "aggregations", null, 0, -1, ClassOrm.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getClassOrm_DataSet(), this.getDbDataSet(), null, "dataSet", null, 1, 1, ClassOrm.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getClassOrm_Key(), this.getDiscriminatorKey(), null, "key", null, 0, 1, ClassOrm.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getClassOrm_References(), this.getReference(), null, "references", null, 0, -1, ClassOrm.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(componentEClass, Component.class, "Component", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getComponent_Lookups(), this.getLookup(), null, "lookups", null, 0, -1, Component.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getComponent_Primitives(), this.getPrimitive(), null, "primitives", null, 0, -1, Component.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(constantEClass, Constant.class, "Constant", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getConstant_Type(), theXMLTypePackage.getString(), "type", null, 0, 1, Constant.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getConstant_Value(), theXMLTypePackage.getString(), "value", null, 0, 1, Constant.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(dbColumnEClass, DbColumn.class, "DbColumn", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getDbColumn_Default(), theXMLTypePackage.getString(), "default", null, 0, 1, DbColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDbColumn_KeySeq(), theXMLTypePackage.getInt(), "keySeq", null, 0, 1, DbColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDbColumn_Length(), theXMLTypePackage.getInt(), "length", null, 0, 1, DbColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDbColumn_Nullable(), ecorePackage.getEString(), "nullable", null, 0, 1, DbColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDbColumn_Scale(), theXMLTypePackage.getInt(), "scale", null, 0, 1, DbColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDbColumn_Seq(), theXMLTypePackage.getInt(), "seq", null, 0, 1, DbColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDbColumn_Size(), theXMLTypePackage.getInt(), "size", null, 0, 1, DbColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDbColumn_Type(), this.getDbColumnTypeEnum(), "type", null, 0, 1, DbColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getDbColumn_Table(), this.getDbTable(), this.getDbTable_Columns(), "table", null, 1, 1, DbColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(dbColumnMapEClass, DbColumnMap.class, "DbColumnMap", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getDbColumnMap_DbColumnName(), ecorePackage.getEString(), "dbColumnName", null, 1, 1, DbColumnMap.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(dbDataSetEClass, DbDataSet.class, "DbDataSet", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(dbForeignKeyColumnEClass, DbForeignKeyColumn.class, "DbForeignKeyColumn", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getDbForeignKeyColumn_KeySeq(), theXMLTypePackage.getInt(), "keySeq", null, 0, 1, DbForeignKeyColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDbForeignKeyColumn_Seq(), theXMLTypePackage.getInt(), "seq", null, 0, 1, DbForeignKeyColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(dbJoinKeyEClass, DbJoinKey.class, "DbJoinKey", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(dbJoinParameterEClass, DbJoinParameter.class, "DbJoinParameter", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(dbRelationEClass, DbRelation.class, "DbRelation", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getDbRelation_ForeignKeyColumns(), this.getDbForeignKeyColumn(), null, "foreignKeyColumns", null, 0, -1, DbRelation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getDbRelation_RefTable(), this.getDbDataSet(), null, "refTable", null, 0, 1, DbRelation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getDbRelation_Table(), this.getDbDataSet(), null, "table", null, 0, 1, DbRelation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(dbTableEClass, DbTable.class, "DbTable", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getDbTable_Columns(), this.getDbColumn(), this.getDbColumn_Table(), "columns", null, 1, -1, DbTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(dbViewEClass, DbView.class, "DbView", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getDbView_JoinKeys(), this.getDbJoinKey(), null, "joinKeys", null, 0, -1, DbView.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getDbView_JoinParameters(), this.getDbJoinParameter(), null, "joinParameters", null, 0, -1, DbView.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getDbView_ViewColumns(), this.getDbViewColumn(), null, "viewColumns", null, 0, -1, DbView.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(dbViewColumnEClass, DbViewColumn.class, "DbViewColumn", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(discriminatorKeyEClass, DiscriminatorKey.class, "DiscriminatorKey", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getDiscriminatorKey_DiscriminatorValue(), theXMLTypePackage.getString(), "discriminatorValue", null, 0, 1, DiscriminatorKey.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(domainAttributeEClass, DomainAttribute.class, "DomainAttribute", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getDomainAttribute_DataType(), this.getDomainClass(), null, "dataType", null, 1, 1, DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDomainAttribute_Multivalued(), theXMLTypePackage.getBoolean(), "multivalued", null, 0, 1, DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDomainAttribute_Reference(), theXMLTypePackage.getBoolean(), "reference", null, 0, 1, DomainAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(domainClassEClass, DomainClass.class, "DomainClass", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getDomainClass_Superclass(), this.getDomainClass(), null, "superclass", null, 1, 1, DomainClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getDomainClass_PlatformClass(), this.getDomainClass(), null, "platformClass", null, 1, 1, DomainClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getDomainClass_Attributes(), this.getDomainAttribute(), null, "attributes", null, 0, -1, DomainClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDomainClass_Abstract(), theXMLTypePackage.getBoolean(), "abstract", null, 0, 1, DomainClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getDomainClass_Primitive(), theXMLTypePackage.getBoolean(), "primitive", null, 0, 1, DomainClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(editorEClass, Editor.class, "Editor", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getEditor_Editors(), this.getEditor(), null, "editors", null, 0, -1, Editor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getEditor_FieldGroups(), this.getFieldGroup(), null, "fieldGroups", null, 0, -1, Editor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getEditor_SummaryTables(), this.getSummaryTable(), null, "summaryTables", null, 0, -1, Editor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getEditor_PresentInStaging(), ecorePackage.getEString(), "presentInStaging", null, 0, 1, Editor.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(fieldEClass, Field.class, "Field", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getField_Mandatory(), theXMLTypePackage.getBoolean(), "mandatory", null, 0, 1, Field.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getField_MaxLength(), theXMLTypePackage.getInt(), "maxLength", null, 0, 1, Field.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getField_ReferenceListName(), theXMLTypePackage.getString(), "referenceListName", null, 0, 1, Field.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getField_RowIndex(), theXMLTypePackage.getInt(), "rowIndex", null, 0, 1, Field.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getField_SelectDisabled(), theXMLTypePackage.getString(), "selectDisabled", null, 0, 1, Field.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getField_SelectHeaderValue(), theXMLTypePackage.getString(), "selectHeaderValue", null, 0, 1, Field.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getField_Size(), theXMLTypePackage.getInt(), "size", null, 0, 1, Field.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getField_PresentInStaging(), ecorePackage.getEString(), "presentInStaging", null, 0, 1, Field.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getField_Comments(), ecorePackage.getEString(), "comments", null, 0, 1, Field.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getField_Mapping(), ecorePackage.getEString(), "mapping", null, 0, 1, Field.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(fieldExtensionEClass, FieldExtension.class, "FieldExtension", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getFieldExtension_Extension(), this.getTypeExtension(), null, "extension", null, 0, 1, FieldExtension.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(fieldGroupEClass, FieldGroup.class, "FieldGroup", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getFieldGroup_Actions(), this.getAction(), null, "actions", null, 0, -1, FieldGroup.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getFieldGroup_Fields(), this.getField(), null, "fields", null, 0, -1, FieldGroup.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getFieldGroup_DisplayOrder(), ecorePackage.getEInt(), "displayOrder", null, 0, 1, FieldGroup.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getFieldGroup_PresentInStaging(), ecorePackage.getEString(), "presentInStaging", null, 0, 1, FieldGroup.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(fieldGroupExtensionEClass, FieldGroupExtension.class, "FieldGroupExtension", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getFieldGroupExtension_Extension(), this.getTypeExtension(), null, "extension", null, 0, 1, FieldGroupExtension.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(lookupEClass, Lookup.class, "Lookup", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(modelEClass, Model.class, "Model", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getModel_Elements(), this.getModelElement(), null, "elements", null, 0, -1, Model.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(modelElementEClass, ModelElement.class, "ModelElement", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getModelElement_Annotations(), this.getAnnotation(), null, "annotations", null, 0, -1, ModelElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getModelElement_Name(), theXMLTypePackage.getString(), "name", null, 0, 1, ModelElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(primitiveEClass, Primitive.class, "Primitive", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(primitiveExtensionEClass, PrimitiveExtension.class, "PrimitiveExtension", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getPrimitiveExtension_Extension(), this.getTypeExtension(), null, "extension", null, 0, 1, PrimitiveExtension.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(propertyEClass, Property.class, "Property", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getProperty_Value(), ecorePackage.getEObject(), null, "value", null, 0, 1, Property.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(referenceEClass, Reference.class, "Reference", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getReference_Relation(), this.getDbRelation(), null, "relation", null, 0, 1, Reference.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(summaryTableEClass, SummaryTable.class, "SummaryTable", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getSummaryTable_Columns(), this.getTableColumn(), null, "columns", null, 0, -1, SummaryTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getSummaryTable_DomainWrapper(), ecorePackage.getEString(), "domainWrapper", null, 0, 1, SummaryTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getSummaryTable_BapName(), ecorePackage.getEString(), "bapName", null, 0, 1, SummaryTable.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(tableColumnEClass, TableColumn.class, "TableColumn", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getTableColumn_HeaderValue(), theXMLTypePackage.getString(), "headerValue", null, 0, 1, TableColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTableColumn_Index(), theXMLTypePackage.getInt(), "index", null, 0, 1, TableColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTableColumn_Property(), ecorePackage.getEString(), "property", null, 0, 1, TableColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTableColumn_ReferenceListName(), ecorePackage.getEString(), "referenceListName", null, 0, 1, TableColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTableColumn_SelectDisabled(), ecorePackage.getEString(), "selectDisabled", null, 0, 1, TableColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTableColumn_Comments(), ecorePackage.getEString(), "comments", null, 0, 1, TableColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTableColumn_Mapping(), ecorePackage.getEString(), "mapping", null, 0, 1, TableColumn.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(tagEClass, Tag.class, "Tag", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getTag_Type(), theXMLTypePackage.getAnySimpleType(), "type", null, 0, 1, Tag.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTag_Value(), theXMLTypePackage.getString(), "value", null, 0, 1, Tag.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(typeExtensionEClass, TypeExtension.class, "TypeExtension", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getTypeExtension_DomainClass(), this.getDomainClass(), null, "domainClass", null, 1, 1, TypeExtension.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getTypeExtension_Properties(), this.getProperty(), null, "properties", null, 0, -1, TypeExtension.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(viewComponentEClass, ViewComponent.class, "ViewComponent", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getViewComponent_ComponentId(), ecorePackage.getEString(), "componentId", null, 0, 1, ViewComponent.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(viewElementEClass, ViewElement.class, "ViewElement", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getViewElement_Id(), theXMLTypePackage.getString(), "id", null, 0, 1, ViewElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getViewElement_Label(), theXMLTypePackage.getString(), "label", null, 0, 1, ViewElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getViewElement_Type(), theXMLTypePackage.getString(), "type", null, 0, 1, ViewElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(viewPrimitiveEClass, ViewPrimitive.class, "ViewPrimitive", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(uiFieldTypeEClass, UiFieldType.class, "UiFieldType", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getUiFieldType_DataType(), ecorePackage.getEString(), "dataType", null, 0, 1, UiFieldType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getUiFieldType_HtmlType(), ecorePackage.getEString(), "htmlType", null, 0, 1, UiFieldType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getUiFieldType_OnScreenValidationMethod(), ecorePackage.getEString(), "onScreenValidationMethod", null, 0, 1, UiFieldType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEReference(getUiFieldType_Properties(), this.getUiFieldTypeProperty(), null, "properties", null, 0, -1, UiFieldType.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(uiFieldTypePropertyEClass, UiFieldTypeProperty.class, "UiFieldTypeProperty", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getUiFieldTypeProperty_PropertyName(), ecorePackage.getEString(), "propertyName", null, 0, 1, UiFieldTypeProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getUiFieldTypeProperty_PropertyType(), ecorePackage.getEString(), "propertyType", null, 0, 1, UiFieldTypeProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getUiFieldTypeProperty_TypeName(), ecorePackage.getEString(), "typeName", null, 0, 1, UiFieldTypeProperty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(componentMappingEClass, ComponentMapping.class, "ComponentMapping", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getComponentMapping_ClassName(), ecorePackage.getEString(), "className", null, 0, 1, ComponentMapping.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getComponentMapping_ComponentId(), ecorePackage.getEString(), "componentId", null, 0, 1, ComponentMapping.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getComponentMapping_Id(), ecorePackage.getEString(), "id", null, 0, 1, ComponentMapping.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getComponentMapping_TypeCode(), ecorePackage.getEString(), "typeCode", null, 0, 1, ComponentMapping.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        // Initialize enums and add enum literals
        initEEnum(cardinalityTypeEnumEEnum, CardinalityTypeEnum.class, "CardinalityTypeEnum");
        addEEnumLiteral(cardinalityTypeEnumEEnum, CardinalityTypeEnum.ONE);
        addEEnumLiteral(cardinalityTypeEnumEEnum, CardinalityTypeEnum.MANY);

        initEEnum(dbColumnTypeEnumEEnum, DbColumnTypeEnum.class, "DbColumnTypeEnum");
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.BIGINT);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.BLOB);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.CHAR);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.CHARACTER);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.DATE);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.INTEGER);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.SHORT);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.SMALLINT);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.TIME);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.TIMESTAMP);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.VARCHAR);
        addEEnumLiteral(dbColumnTypeEnumEEnum, DbColumnTypeEnum.DECIMAL);

        initEEnum(fieldTypeEnumEEnum, FieldTypeEnum.class, "FieldTypeEnum");
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.SELECTABLE);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.DATE);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.TEXT);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.MAP);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.TEXT_DECIMAL);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.ZIP);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.TEXT_INTEGER);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.LINKED_OBJECT);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.LABEL);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.MONTH);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.ZIP_EXTENSION);
        addEEnumLiteral(fieldTypeEnumEEnum, FieldTypeEnum.ROW_TEXT);

        // Create resource
        createResource(eNS_URI);

        // Create annotations
        // http:///org/eclipse/emf/ecore/util/ExtendedMetaData
        createExtendedMetaDataAnnotations();
    }

	/**
     * Initializes the annotations for <b>http:///org/eclipse/emf/ecore/util/ExtendedMetaData</b>.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected void createExtendedMetaDataAnnotations() {
        String source = "http:///org/eclipse/emf/ecore/util/ExtendedMetaData";	
        addAnnotation
          (this, 
           source, 
           new String[] {
             "qualified", "false"
           });	
        addAnnotation
          (actionEClass, 
           source, 
           new String[] {
             "name", "action",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getAction_AccessKey(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "key",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getAction_Url(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "url",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (aggregationEClass, 
           source, 
           new String[] {
             "name", "aggregation",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getAggregation_Component(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "component",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (annotationEClass, 
           source, 
           new String[] {
             "name", "annotation",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getAnnotation_Tags(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "tags",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (applicationEClass, 
           source, 
           new String[] {
             "name", "application",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getApplication_Baps(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "baps",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (associationEClass, 
           source, 
           new String[] {
             "name", "association",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getAssociation_Cardinality(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "cardinality",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (attributeEClass, 
           source, 
           new String[] {
             "name", "attribute",
             "kind", "elementOnly"
           });	
        addAnnotation
          (bapEClass, 
           source, 
           new String[] {
             "name", "bap",
             "kind", "elementOnly"
           });	
        addAnnotation
          (cardinalityTypeEnumEEnum, 
           source, 
           new String[] {
             "name", "CardinalityTypeEnum"
           });	
        addAnnotation
          (classOrmEClass, 
           source, 
           new String[] {
             "name", "classOrm",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getClassOrm_Aggregations(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "aggregations",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getClassOrm_DataSet(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "dataSet",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getClassOrm_Key(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "key",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getClassOrm_References(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "references",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (componentEClass, 
           source, 
           new String[] {
             "name", "component",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getComponent_Lookups(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "lookups",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getComponent_Primitives(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "primitives",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (constantEClass, 
           source, 
           new String[] {
             "name", "constant",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getConstant_Type(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "type",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getConstant_Value(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "value",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (dbColumnEClass, 
           source, 
           new String[] {
             "name", "dbColumn",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getDbColumn_Default(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "default",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbColumn_KeySeq(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "keySeq",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbColumn_Length(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "length",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbColumn_Nullable(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "nullable",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbColumn_Scale(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "scale",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbColumn_Seq(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "seq",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbColumn_Size(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "size",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbColumn_Type(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "type",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (dbColumnMapEClass, 
           source, 
           new String[] {
             "name", "dbColumnMap",
             "kind", "elementOnly"
           });	
        addAnnotation
          (dbColumnTypeEnumEEnum, 
           source, 
           new String[] {
             "name", "dbColumnTypeEnum"
           });	
        addAnnotation
          (dbDataSetEClass, 
           source, 
           new String[] {
             "name", "dbDataSet",
             "kind", "elementOnly"
           });	
        addAnnotation
          (dbForeignKeyColumnEClass, 
           source, 
           new String[] {
             "name", "dbForeignKeyColumn",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getDbForeignKeyColumn_KeySeq(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "keySeq",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbForeignKeyColumn_Seq(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "seq",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (dbJoinKeyEClass, 
           source, 
           new String[] {
             "name", "dbJoinKey",
             "kind", "elementOnly"
           });	
        addAnnotation
          (dbJoinParameterEClass, 
           source, 
           new String[] {
             "name", "dbJoinParameter",
             "kind", "elementOnly"
           });	
        addAnnotation
          (dbRelationEClass, 
           source, 
           new String[] {
             "name", "dbRelation",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getDbRelation_ForeignKeyColumns(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "foreignKeyColumns",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbRelation_RefTable(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "refTable",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbRelation_Table(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "table",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (dbTableEClass, 
           source, 
           new String[] {
             "name", "dbTable",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getDbTable_Columns(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "columns",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (dbViewEClass, 
           source, 
           new String[] {
             "name", "dbView",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getDbView_JoinKeys(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "joinKeys",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbView_JoinParameters(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "joinParameters",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDbView_ViewColumns(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "viewColumns",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (dbViewColumnEClass, 
           source, 
           new String[] {
             "name", "dbViewColumn",
             "kind", "elementOnly"
           });	
        addAnnotation
          (discriminatorKeyEClass, 
           source, 
           new String[] {
             "name", "discriminatorKey",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getDiscriminatorKey_DiscriminatorValue(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "discriminatorValue",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (domainAttributeEClass, 
           source, 
           new String[] {
             "name", "domainAttribute",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getDomainAttribute_DataType(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "dataType",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDomainAttribute_Multivalued(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "multivalued",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDomainAttribute_Reference(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "reference",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (domainClassEClass, 
           source, 
           new String[] {
             "name", "domainClass",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getDomainClass_Superclass(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "superclass",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDomainClass_PlatformClass(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "platformClass",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDomainClass_Attributes(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "attributes",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDomainClass_Abstract(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "abstract",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getDomainClass_Primitive(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "primitive",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (editorEClass, 
           source, 
           new String[] {
             "name", "editor",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getEditor_Editors(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "editors",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getEditor_FieldGroups(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "fieldGroups",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getEditor_SummaryTables(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "summaryTables",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (fieldEClass, 
           source, 
           new String[] {
             "name", "field",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getField_Mandatory(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "mandatory",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getField_MaxLength(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "maxLength",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getField_ReferenceListName(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "referenceListName",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getField_RowIndex(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "rowIndex",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getField_SelectDisabled(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "selectDisabled",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getField_SelectHeaderValue(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "selectHeaderValue",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getField_Size(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "size",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (fieldExtensionEClass, 
           source, 
           new String[] {
             "name", "fieldExtension",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getFieldExtension_Extension(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "extension",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (fieldGroupEClass, 
           source, 
           new String[] {
             "name", "fieldGroup",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getFieldGroup_Actions(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "actions",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getFieldGroup_Fields(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "fields",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (fieldGroupExtensionEClass, 
           source, 
           new String[] {
             "name", "fieldGroupExtension",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getFieldGroupExtension_Extension(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "extension",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (fieldTypeEnumEEnum, 
           source, 
           new String[] {
             "name", "FieldTypeEnum"
           });	
        addAnnotation
          (lookupEClass, 
           source, 
           new String[] {
             "name", "lookup",
             "kind", "elementOnly"
           });	
        addAnnotation
          (modelEClass, 
           source, 
           new String[] {
             "name", "model",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getModel_Elements(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "elements",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (modelElementEClass, 
           source, 
           new String[] {
             "name", "modelElement",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getModelElement_Annotations(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "annotations",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getModelElement_Name(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "name",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (primitiveEClass, 
           source, 
           new String[] {
             "name", "primitive",
             "kind", "elementOnly"
           });	
        addAnnotation
          (primitiveExtensionEClass, 
           source, 
           new String[] {
             "name", "primitiveExtension",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getPrimitiveExtension_Extension(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "extension",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (propertyEClass, 
           source, 
           new String[] {
             "name", "property",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getProperty_Value(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "value",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (referenceEClass, 
           source, 
           new String[] {
             "name", "reference",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getReference_Relation(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "relation",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (summaryTableEClass, 
           source, 
           new String[] {
             "name", "summaryTable",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getSummaryTable_Columns(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "columns",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (tableColumnEClass, 
           source, 
           new String[] {
             "name", "tableColumn",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getTableColumn_HeaderValue(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "headerValue",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getTableColumn_Index(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "index",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (tagEClass, 
           source, 
           new String[] {
             "name", "tag",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getTag_Type(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "type",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getTag_Value(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "value",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (typeExtensionEClass, 
           source, 
           new String[] {
             "name", "typeExtension",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getTypeExtension_DomainClass(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "domainClass",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getTypeExtension_Properties(), 
           source, 
           new String[] {
             "kind", "element",
             "name", "properties",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (viewComponentEClass, 
           source, 
           new String[] {
             "name", "viewComponent",
             "kind", "elementOnly"
           });	
        addAnnotation
          (viewElementEClass, 
           source, 
           new String[] {
             "name", "viewElement",
             "kind", "elementOnly"
           });	
        addAnnotation
          (getViewElement_Id(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "id",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getViewElement_Label(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "label",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (getViewElement_Type(), 
           source, 
           new String[] {
             "kind", "attribute",
             "name", "type",
             "namespace", "##targetNamespace"
           });	
        addAnnotation
          (viewPrimitiveEClass, 
           source, 
           new String[] {
             "name", "viewPrimitive",
             "kind", "elementOnly"
           });
    }

} //Model2PackageImpl
