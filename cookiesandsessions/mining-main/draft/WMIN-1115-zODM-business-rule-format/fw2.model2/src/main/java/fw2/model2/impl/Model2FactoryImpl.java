/**
 */
package fw2.model2.impl;

import fw2.model2.*;

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
public class Model2FactoryImpl extends EFactoryImpl implements Model2Factory {
	/**
     * Creates the default factory implementation.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public static Model2Factory init() {
        try {
            Model2Factory theModel2Factory = (Model2Factory)EPackage.Registry.INSTANCE.getEFactory(Model2Package.eNS_URI);
            if (theModel2Factory != null) {
                return theModel2Factory;
            }
        }
        catch (Exception exception) {
            EcorePlugin.INSTANCE.log(exception);
        }
        return new Model2FactoryImpl();
    }

	/**
     * Creates an instance of the factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Model2FactoryImpl() {
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
            case Model2Package.ACTION: return createAction();
            case Model2Package.AGGREGATION: return createAggregation();
            case Model2Package.ANNOTATION: return createAnnotation();
            case Model2Package.APPLICATION: return createApplication();
            case Model2Package.BAP: return createBap();
            case Model2Package.CLASS_ORM: return createClassOrm();
            case Model2Package.COMPONENT: return createComponent();
            case Model2Package.CONSTANT: return createConstant();
            case Model2Package.DB_COLUMN: return createDbColumn();
            case Model2Package.DB_COLUMN_MAP: return createDbColumnMap();
            case Model2Package.DB_FOREIGN_KEY_COLUMN: return createDbForeignKeyColumn();
            case Model2Package.DB_JOIN_KEY: return createDbJoinKey();
            case Model2Package.DB_JOIN_PARAMETER: return createDbJoinParameter();
            case Model2Package.DB_RELATION: return createDbRelation();
            case Model2Package.DB_TABLE: return createDbTable();
            case Model2Package.DB_VIEW: return createDbView();
            case Model2Package.DB_VIEW_COLUMN: return createDbViewColumn();
            case Model2Package.DISCRIMINATOR_KEY: return createDiscriminatorKey();
            case Model2Package.DOMAIN_ATTRIBUTE: return createDomainAttribute();
            case Model2Package.DOMAIN_CLASS: return createDomainClass();
            case Model2Package.EDITOR: return createEditor();
            case Model2Package.FIELD: return createField();
            case Model2Package.FIELD_EXTENSION: return createFieldExtension();
            case Model2Package.FIELD_GROUP: return createFieldGroup();
            case Model2Package.FIELD_GROUP_EXTENSION: return createFieldGroupExtension();
            case Model2Package.LOOKUP: return createLookup();
            case Model2Package.MODEL: return createModel();
            case Model2Package.PRIMITIVE: return createPrimitive();
            case Model2Package.PRIMITIVE_EXTENSION: return createPrimitiveExtension();
            case Model2Package.PROPERTY: return createProperty();
            case Model2Package.REFERENCE: return createReference();
            case Model2Package.SUMMARY_TABLE: return createSummaryTable();
            case Model2Package.TABLE_COLUMN: return createTableColumn();
            case Model2Package.TAG: return createTag();
            case Model2Package.TYPE_EXTENSION: return createTypeExtension();
            case Model2Package.UI_FIELD_TYPE: return createUiFieldType();
            case Model2Package.UI_FIELD_TYPE_PROPERTY: return createUiFieldTypeProperty();
            case Model2Package.COMPONENT_MAPPING: return createComponentMapping();
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
            case Model2Package.CARDINALITY_TYPE_ENUM:
                return createCardinalityTypeEnumFromString(eDataType, initialValue);
            case Model2Package.DB_COLUMN_TYPE_ENUM:
                return createDbColumnTypeEnumFromString(eDataType, initialValue);
            case Model2Package.FIELD_TYPE_ENUM:
                return createFieldTypeEnumFromString(eDataType, initialValue);
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
            case Model2Package.CARDINALITY_TYPE_ENUM:
                return convertCardinalityTypeEnumToString(eDataType, instanceValue);
            case Model2Package.DB_COLUMN_TYPE_ENUM:
                return convertDbColumnTypeEnumToString(eDataType, instanceValue);
            case Model2Package.FIELD_TYPE_ENUM:
                return convertFieldTypeEnumToString(eDataType, instanceValue);
            default:
                throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
        }
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Action createAction() {
        ActionImpl action = new ActionImpl();
        return action;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Aggregation createAggregation() {
        AggregationImpl aggregation = new AggregationImpl();
        return aggregation;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Annotation createAnnotation() {
        AnnotationImpl annotation = new AnnotationImpl();
        return annotation;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Application createApplication() {
        ApplicationImpl application = new ApplicationImpl();
        return application;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Bap createBap() {
        BapImpl bap = new BapImpl();
        return bap;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public ClassOrm createClassOrm() {
        ClassOrmImpl classOrm = new ClassOrmImpl();
        return classOrm;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Component createComponent() {
        ComponentImpl component = new ComponentImpl();
        return component;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Constant createConstant() {
        ConstantImpl constant = new ConstantImpl();
        return constant;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbColumn createDbColumn() {
        DbColumnImpl dbColumn = new DbColumnImpl();
        return dbColumn;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbColumnMap createDbColumnMap() {
        DbColumnMapImpl dbColumnMap = new DbColumnMapImpl();
        return dbColumnMap;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbForeignKeyColumn createDbForeignKeyColumn() {
        DbForeignKeyColumnImpl dbForeignKeyColumn = new DbForeignKeyColumnImpl();
        return dbForeignKeyColumn;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbJoinKey createDbJoinKey() {
        DbJoinKeyImpl dbJoinKey = new DbJoinKeyImpl();
        return dbJoinKey;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbJoinParameter createDbJoinParameter() {
        DbJoinParameterImpl dbJoinParameter = new DbJoinParameterImpl();
        return dbJoinParameter;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbRelation createDbRelation() {
        DbRelationImpl dbRelation = new DbRelationImpl();
        return dbRelation;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbTable createDbTable() {
        DbTableImpl dbTable = new DbTableImpl();
        return dbTable;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbView createDbView() {
        DbViewImpl dbView = new DbViewImpl();
        return dbView;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbViewColumn createDbViewColumn() {
        DbViewColumnImpl dbViewColumn = new DbViewColumnImpl();
        return dbViewColumn;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DiscriminatorKey createDiscriminatorKey() {
        DiscriminatorKeyImpl discriminatorKey = new DiscriminatorKeyImpl();
        return discriminatorKey;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DomainAttribute createDomainAttribute() {
        DomainAttributeImpl domainAttribute = new DomainAttributeImpl();
        return domainAttribute;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DomainClass createDomainClass() {
        DomainClassImpl domainClass = new DomainClassImpl();
        return domainClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Editor createEditor() {
        EditorImpl editor = new EditorImpl();
        return editor;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Field createField() {
        FieldImpl field = new FieldImpl();
        return field;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public FieldExtension createFieldExtension() {
        FieldExtensionImpl fieldExtension = new FieldExtensionImpl();
        return fieldExtension;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public FieldGroup createFieldGroup() {
        FieldGroupImpl fieldGroup = new FieldGroupImpl();
        return fieldGroup;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public FieldGroupExtension createFieldGroupExtension() {
        FieldGroupExtensionImpl fieldGroupExtension = new FieldGroupExtensionImpl();
        return fieldGroupExtension;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Lookup createLookup() {
        LookupImpl lookup = new LookupImpl();
        return lookup;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Model createModel() {
        ModelImpl model = new ModelImpl();
        return model;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Primitive createPrimitive() {
        PrimitiveImpl primitive = new PrimitiveImpl();
        return primitive;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public PrimitiveExtension createPrimitiveExtension() {
        PrimitiveExtensionImpl primitiveExtension = new PrimitiveExtensionImpl();
        return primitiveExtension;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Property createProperty() {
        PropertyImpl property = new PropertyImpl();
        return property;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Reference createReference() {
        ReferenceImpl reference = new ReferenceImpl();
        return reference;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public SummaryTable createSummaryTable() {
        SummaryTableImpl summaryTable = new SummaryTableImpl();
        return summaryTable;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public TableColumn createTableColumn() {
        TableColumnImpl tableColumn = new TableColumnImpl();
        return tableColumn;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Tag createTag() {
        TagImpl tag = new TagImpl();
        return tag;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public TypeExtension createTypeExtension() {
        TypeExtensionImpl typeExtension = new TypeExtensionImpl();
        return typeExtension;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public UiFieldType createUiFieldType() {
        UiFieldTypeImpl uiFieldType = new UiFieldTypeImpl();
        return uiFieldType;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public UiFieldTypeProperty createUiFieldTypeProperty() {
        UiFieldTypePropertyImpl uiFieldTypeProperty = new UiFieldTypePropertyImpl();
        return uiFieldTypeProperty;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public ComponentMapping createComponentMapping() {
        ComponentMappingImpl componentMapping = new ComponentMappingImpl();
        return componentMapping;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public CardinalityTypeEnum createCardinalityTypeEnumFromString(EDataType eDataType, String initialValue) {
        CardinalityTypeEnum result = CardinalityTypeEnum.get(initialValue);
        if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
        return result;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String convertCardinalityTypeEnumToString(EDataType eDataType, Object instanceValue) {
        return instanceValue == null ? null : instanceValue.toString();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DbColumnTypeEnum createDbColumnTypeEnumFromString(EDataType eDataType, String initialValue) {
        DbColumnTypeEnum result = DbColumnTypeEnum.get(initialValue);
        if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
        return result;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String convertDbColumnTypeEnumToString(EDataType eDataType, Object instanceValue) {
        return instanceValue == null ? null : instanceValue.toString();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public FieldTypeEnum createFieldTypeEnumFromString(EDataType eDataType, String initialValue) {
        FieldTypeEnum result = FieldTypeEnum.get(initialValue);
        if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
        return result;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String convertFieldTypeEnumToString(EDataType eDataType, Object instanceValue) {
        return instanceValue == null ? null : instanceValue.toString();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public Model2Package getModel2Package() {
        return (Model2Package)getEPackage();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @deprecated
     * @generated
     */
	@Deprecated
	public static Model2Package getPackage() {
        return Model2Package.eINSTANCE;
    }

} //Model2FactoryImpl
