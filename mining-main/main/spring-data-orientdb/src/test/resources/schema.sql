--############################### Create Database and Connect ###################################
CREATE DATABASE remote:localhost/spring-data-orientdb root Worx2000
CONNECT remote:localhost/spring-data-orientdb root Worx2000

--################################################################################################
-- Sequences
--################################################################################################
CREATE SEQUENCE Employee_Sequence TYPE ORDERED START 1 INCREMENT 1;
CREATE SEQUENCE Customer_Sequence TYPE ORDERED START 0 INCREMENT 1;
CREATE SEQUENCE Product_Sequence TYPE ORDERED START 0 INCREMENT 1;
CREATE SEQUENCE Reviewed_Sequence TYPE ORDERED START 0 INCREMENT 1;
CREATE SEQUENCE Reference_Sequence TYPE ORDERED START 0 INCREMENT 1;

--################################################################################################
-- Employee
--################################################################################################
-- Create Employee Class
CREATE CLASS Employee EXTENDS V
CREATE PROPERTY Employee.id LONG (NOTNULL, MANDATORY TRUE)
CREATE PROPERTY Employee.firstName STRING (NOTNULL, MANDATORY TRUE)
CREATE PROPERTY Employee.lastName STRING (NOTNULL, MANDATORY FALSE)
CREATE PROPERTY Employee.emailId STRING (NOTNULL, MANDATORY TRUE)
CREATE PROPERTY Employee.age INTEGER (NOTNULL, MANDATORY FALSE)
CREATE PROPERTY Employee.isActive BOOLEAN (NOTNULL, MANDATORY FALSE)
CREATE PROPERTY Employee.secretCode BINARY (NOTNULL, MANDATORY FALSE)

--##################################################################################################
-- Project
--##################################################################################################
-- Create Project Class
CREATE CLASS Project EXTENDS V
CREATE PROPERTY Project.name STRING (NOTNULL, MANDATORY TRUE)

--################################################################################################
-- Edges
--################################################################################################
CREATE CLASS Employee_coaches EXTENDS E
CREATE PROPERTY Employee_coaches.in Link Employee (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Employee_coaches.out Link Employee (NOTNULL, MANDATORY TRUE);
CREATE CLASS Employee_projects EXTENDS E
CREATE PROPERTY Employee_projects.in Link Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Employee_projects.out Link Employee (NOTNULL, MANDATORY TRUE);
CREATE CLASS Employee_projectStatus EXTENDS E
CREATE PROPERTY Employee_projectStatus.in Link Project (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Employee_projectStatus.out Link Employee (NOTNULL, MANDATORY TRUE);
CREATE CLASS Employee_subProjects EXTENDS E
CREATE CLASS HasReportsTo EXTENDS E
CREATE PROPERTY HasReportsTo.in Link Employee (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY HasReportsTo.out Link Employee (NOTNULL, MANDATORY TRUE);

CREATE PROPERTY EMPLOYEE.in_HasReportsTo LinkList HasReportsTo;
CREATE PROPERTY EMPLOYEE.out_HasReportsTo LinkList HasReportsTo;
CREATE PROPERTY EMPLOYEE.out_Employee_projects LinkList Employee_projects;
CREATE PROPERTY EMPLOYEE.in_Employee_coaches LinkList Employee_coaches;
CREATE PROPERTY EMPLOYEE.out_Employee_coaches LinkList Employee_coaches;

--################################################################################################
-- Product
--################################################################################################
-- Create Product Class
CREATE CLASS Product EXTENDS V;
CREATE PROPERTY Product.productId LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Product.productName STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Product.productImage BINARY (NOTNULL, MANDATORY FALSE);
CREATE INDEX Product_id_idx ON Product (productId) UNIQUE;

--################################################################################################
-- Address
--################################################################################################
-- Create Address Class
CREATE CLASS Address;
CREATE PROPERTY Address.doorNo STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Address.streetName STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Address.city STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Address.pincode STRING (NOTNULL, MANDATORY TRUE);

--################################################################################################
-- Account
--################################################################################################
-- Create Account Class
CREATE CLASS Account EXTENDS V;
CREATE PROPERTY Account.cardNumber STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Account.cardName STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Account.expiryDate DATE (NOTNULL, MANDATORY TRUE);

--################################################################################################
-- Customer
--################################################################################################
-- Create Customer Class
CREATE CLASS Customer EXTENDS V;
CREATE PROPERTY Customer.customerId LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Customer.customerName STRING (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Customer.products LINKLIST Product (NOTNULL, MANDATORY FALSE);
CREATE PROPERTY Customer.address EMBEDDED Address (NOTNULL, MANDATORY FALSE);
CREATE PROPERTY Customer.subscribed LINK Product (NOTNULL, MANDATORY FALSE);
CREATE PROPERTY Customer.account LINK Account;
CREATE PROPERTY Customer.embeddedSet EMBEDDEDSET STRING;
CREATE PROPERTY Customer.embeddedMap EMBEDDEDMAP STRING;
CREATE INDEX Customer_id_idx ON Customer (customerId) UNIQUE;

--################################################################################################
-- Edge class from customer to product - Reviewed
--################################################################################################
CREATE CLASS RatingEnum EXTENDS V;
CREATE PROPERTY RatingEnum.name STRING (NOTNULL, MANDATORY TRUE);
CREATE INDEX RatingEnum_name_idx ON RatingEnum (name) UNIQUE;

INSERT INTO RatingEnum SET name="ONE";
INSERT INTO RatingEnum SET name="TWO";
INSERT INTO RatingEnum SET name="THREE";
INSERT INTO RatingEnum SET name="FOUR";
INSERT INTO RatingEnum SET name="FIVE";

CREATE CLASS reviewed EXTENDS E;
CREATE PROPERTY reviewed.id LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY reviewed.out LINK Customer (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY reviewed.in LINK Product (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Customer.out_reviewed LINKLIST reviewed;
CREATE PROPERTY Product.in_reviewed LINKLIST reviewed (MIN 0, MAX 1);
CREATE PROPERTY reviewed.properties EMBEDDEDMAP STRING;
CREATE PROPERTY reviewed.sellerAddress EMBEDDED Address;
CREATE PROPERTY reviewed.rating LINKLIST RatingEnum;


--################################################################################################
-- Edge class from customer to product - Odered by
--################################################################################################
CREATE CLASS orderedBy EXTENDS E;
CREATE PROPERTY orderedBy.out LINK Customer (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY orderedBy.in LINK Product (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Product.customer LINK orderedBy;

--################################################################################################
-- Add custom Properties
--################################################################################################
CREATE CLASS AdditionalInfo EXTENDS V ABSTRACT;
CREATE CLASS HasAdditionalInfo EXTENDS E;
CREATE PROPERTY AdditionalInfo.in_HasAdditionalInfo LINKLIST HasAdditionalInfo;
 
CREATE CLASS CustomProperties EXTENDS AdditionalInfo ABSTRACT;
 
CREATE CLASS MiningEntity EXTENDS V ABSTRACT;
CREATE PROPERTY MiningEntity.out_HasAdditionalInfo LINKLIST HasAdditionalInfo;
 
CREATE PROPERTY HasAdditionalInfo.out LINK MiningEntity (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY HasAdditionalInfo.in LINK AdditionalInfo (NOTNULL, MANDATORY TRUE);
 
ALTER CLASS Customer SUPERCLASS MiningEntity;
ALTER CLASS Product SUPERCLASS MiningEntity;

CREATE CLASS CustomerCustomProperties EXTENDS CustomProperties;

CREATE PROPERTY CustomerCustomProperties.customCustomerMandatoryProperty IF NOT EXISTS String
ALTER PROPERTY CustomerCustomProperties.customCustomerMandatoryProperty CUSTOM label="Custom Customer Mandatory Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerMandatoryProperty CUSTOM pluginVisible=true

CREATE PROPERTY CustomerCustomProperties.customCustomerDateProperty IF NOT EXISTS Date
ALTER PROPERTY CustomerCustomProperties.customCustomerDateProperty CUSTOM label="Custom Customer Date Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerDateProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerDateProperty description "A custom Date property for the Customer class"

CREATE PROPERTY CustomerCustomProperties.customCustomerListPropertyWithMax IF NOT EXISTS LINKLIST Customer
ALTER PROPERTY CustomerCustomProperties.customCustomerListPropertyWithMax CUSTOM label="Custom Customer List PropertyWithMax"
ALTER PROPERTY CustomerCustomProperties.customCustomerListPropertyWithMax CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerListPropertyWithMax description "A custom List property with max validation for the Customer class"
ALTER PROPERTY CustomerCustomProperties.customCustomerListPropertyWithMax MAX 2

CREATE PROPERTY CustomerCustomProperties.customCustomerDatetimeProperty IF NOT EXISTS Datetime
ALTER PROPERTY CustomerCustomProperties.customCustomerDatetimeProperty CUSTOM label="Custom Customer Datetime Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerDatetimeProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerDatetimeProperty description "A custom Datetime property for the Customer class"

CREATE PROPERTY CustomerCustomProperties.customCustomerByteProperty IF NOT EXISTS Byte
ALTER PROPERTY CustomerCustomProperties.customCustomerByteProperty CUSTOM label="Custom Customer Byte Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerByteProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerByteProperty description "A custom Byte property for the Customer class"

CREATE PROPERTY CustomerCustomProperties.customCustomerDecimalProperty IF NOT EXISTS Decimal
ALTER PROPERTY CustomerCustomProperties.customCustomerDecimalProperty CUSTOM label="Custom Customer Decimal Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerDecimalProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerDecimalProperty description "A custom Decimal property for the Customer class"

CREATE PROPERTY CustomerCustomProperties.customCustomerReferenceProperty IF NOT EXISTS LINK Customer
ALTER PROPERTY CustomerCustomProperties.customCustomerReferenceProperty CUSTOM label="Custom Customer Reference Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerReferenceProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerReferenceProperty description "A custom Reference property for the Customer class"

CREATE PROPERTY CustomerCustomProperties.customCustomerBooleanProperty IF NOT EXISTS Boolean
ALTER PROPERTY CustomerCustomProperties.customCustomerBooleanProperty CUSTOM label="Custom Customer Boolean Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerBooleanProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerBooleanProperty description "A custom Boolean property for the Customer class"

CREATE PROPERTY CustomerCustomProperties.customCustomerIntegerProperty IF NOT EXISTS Integer
ALTER PROPERTY CustomerCustomProperties.customCustomerIntegerProperty CUSTOM label="Custom Customer Integer Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerIntegerProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerIntegerProperty description "A custom Integer property for the Customer class"

CREATE PROPERTY CustomerCustomProperties.customCustomerShortProperty IF NOT EXISTS Short
ALTER PROPERTY CustomerCustomProperties.customCustomerShortProperty CUSTOM label="Custom Customer Short Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerShortProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerShortProperty description "A custom Short property for the Customer class"

CREATE PROPERTY CustomerCustomProperties.customCustomerLongProperty IF NOT EXISTS Long
ALTER PROPERTY CustomerCustomProperties.customCustomerLongProperty CUSTOM label="Custom Customer Long Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerLongProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerLongProperty description "A custom Long property for the Customer class"

CREATE PROPERTY CustomerCustomProperties.customCustomerFloatProperty IF NOT EXISTS Float
ALTER PROPERTY CustomerCustomProperties.customCustomerFloatProperty CUSTOM label="Custom Customer Float Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerFloatProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerFloatProperty description "A custom Float property for the Customer class"

CREATE PROPERTY CustomerCustomProperties.customCustomerDoubleProperty IF NOT EXISTS Double
ALTER PROPERTY CustomerCustomProperties.customCustomerDoubleProperty CUSTOM label="Custom Customer Double Property"
ALTER PROPERTY CustomerCustomProperties.customCustomerDoubleProperty CUSTOM pluginVisible=true
ALTER PROPERTY CustomerCustomProperties.customCustomerDoubleProperty description "A custom Double property for the Customer class"

CREATE CLASS ProductCustomProperties EXTENDS CustomProperties;

CREATE PROPERTY ProductCustomProperties.customProductStringProperty IF NOT EXISTS String
ALTER PROPERTY ProductCustomProperties.customProductStringProperty CUSTOM label="Custom String Property"
ALTER PROPERTY ProductCustomProperties.customProductStringProperty CUSTOM pluginVisible=TRUE
ALTER PROPERTY ProductCustomProperties.customProductStringProperty description "A custom String property for the Product class"

CREATE PROPERTY ProductCustomProperties.customProductIntegerProperty IF NOT EXISTS Integer
ALTER PROPERTY ProductCustomProperties.customProductIntegerProperty CUSTOM label="Custom Integer Property"
ALTER PROPERTY ProductCustomProperties.customProductIntegerProperty CUSTOM pluginVisible=TRUE
ALTER PROPERTY ProductCustomProperties.customProductIntegerProperty description "A custom Integer property for the Product class"

--################################################################################################
-- Enum classes
--################################################################################################
CREATE CLASS Designation EXTENDS V;
INSERT INTO Designation set name="CONSULTANT";
INSERT INTO Designation set name="MANAGER";
CREATE PROPERTY Employee.designation LINK Designation

CREATE CLASS Access EXTENDS V;
INSERT INTO Access set name="ADMIN";
INSERT INTO Access set name="USER";
INSERT INTO Access set name="DEVELOPER";
INSERT INTO Access set name="TESTER";
CREATE PROPERTY Employee.access LINKLIST Access;

--################################################################################################
-- Class to test invalid sequence name
--################################################################################################
CREATE CLASS InvalidSequence Extends V;
CREATE PROPERTY InvalidSequence.id LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY InvalidSequence.name STRING (NOTNULL, MANDATORY TRUE);

--################################################################################################
-- Inheritance with edges
--################################################################################################

CREATE CLASS ModuleLocation EXTENDS V;
CREATE PROPERTY ModuleLocation.offset INTEGER (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY ModuleLocation.length INTEGER (NOTNULL, MANDATORY TRUE);

CREATE CLASS Reference EXTENDS E ABSTRACT;
CREATE PROPERTY Reference.id LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Reference.fromModuleLocationLink LINK ModuleLocation;
CREATE PROPERTY Reference.toModuleLocationLink LINK ModuleLocation;
CREATE PROPERTY Reference.properties EMBEDDEDMAP STRING;
CREATE INDEX Reference_id_idx ON Reference (id) UNIQUE;

CREATE CLASS Calls EXTENDS Reference;
CREATE PROPERTY Customer.out_Calls LINKLIST Calls;
CREATE PROPERTY Customer.in_Calls LINKLIST Calls;
CREATE PROPERTY Calls.linklist LINKLIST Employee;
CREATE PROPERTY Calls.out LINK Customer (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Calls.in LINK Customer (NOTNULL, MANDATORY TRUE);
CREATE INDEX Calls_in_out_idx ON Calls (in, out) NOTUNIQUE;
CREATE INDEX Calls_out_idx ON Calls (out) NOTUNIQUE;

CREATE CLASS Includes EXTENDS Reference;
CREATE PROPERTY Customer.out_Includes LINKLIST Includes;
CREATE PROPERTY Customer.in_Includes LINKLIST Includes;
CREATE PROPERTY Includes.out LINK Customer (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY Includes.in LINK Customer (NOTNULL, MANDATORY TRUE);
CREATE INDEX Includes_in_out_idx ON Includes (in, out) NOTUNIQUE;
CREATE INDEX Includes_out_idx ON Includes (out) NOTUNIQUE;

CREATE PROPERTY Calls.customCallsProperty IF NOT EXISTS String
ALTER PROPERTY Calls.customCallsProperty CUSTOM label="Custom Calls Mandatory Property"
ALTER PROPERTY Calls.customCallsProperty CUSTOM pluginVisible=true

--################################################################################################
-- Document Classes
--################################################################################################
CREATE CLASS EmployeePerformanceSheet;
CREATE PROPERTY EmployeePerformanceSheet.employeeLink LINK Employee (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EmployeePerformanceSheet.performance STRING;
CREATE INDEX EmployeePerformanceSheet_employeeLink_idx ON EmployeePerformanceSheet (employeeLink) NOTUNIQUE;

--################################################################################################
-- Entities for testing persistence constructor
--################################################################################################
CREATE CLASS EntityWithCollectionParams EXTENDS V;
CREATE PROPERTY EntityWithCollectionParams.primitiveList EMBEDDEDLIST STRING;
CREATE PROPERTY EntityWithCollectionParams.primitiveSet EMBEDDEDSET LONG;
CREATE PROPERTY EntityWithCollectionParams.primitiveMap EMBEDDEDMAP STRING;

CREATE CLASS EntityWithDefaultConstructor EXTENDS V;
CREATE PROPERTY EntityWithDefaultConstructor.id LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EntityWithDefaultConstructor.booleanVal BOOLEAN (NOTNULL, MANDATORY TRUE);

CREATE CLASS EntityWithCollectionLink EXTENDS V;
CREATE PROPERTY EntityWithCollectionLink.employees LINKLIST Employee (NOTNULL, MANDATORY TRUE);

CREATE CLASS EntityWithEmbeddedParam EXTENDS V;
CREATE PROPERTY EntityWithEmbeddedParam.address EMBEDDED Address (NOTNULL, MANDATORY FALSE);

CREATE CLASS EntityWithEnumParams EXTENDS V;
CREATE CLASS EnumData EXTENDS V;
CREATE PROPERTY EnumData.name STRING (NOTNULL, MANDATORY TRUE);
INSERT INTO EnumData set name="ENUM1";
INSERT INTO EnumData set name="ENUM2";
INSERT INTO EnumData set name="ENUM3";
CREATE INDEX EnumData_name_idx ON EnumData (name) UNIQUE;
CREATE PROPERTY EntityWithEnumParams.enumData LINK EnumData (NOTNULL, MANDATORY TRUE);

CREATE CLASS EntityWithLinkParam EXTENDS V;
CREATE PROPERTY EntityWithLinkParam.employee LINK Employee (NOTNULL, MANDATORY TRUE);

CREATE CLASS EntityWithPrimitiveValues EXTENDS V;
CREATE PROPERTY EntityWithPrimitiveValues.bytes BYTE (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EntityWithPrimitiveValues.booleanValue BOOLEAN (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EntityWithPrimitiveValues.wrappedBooleanValue BOOLEAN (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EntityWithPrimitiveValues.wrappedByteValue BYTE (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EntityWithPrimitiveValues.doubleValue DOUBLE (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EntityWithPrimitiveValues.floatValue FLOAT (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EntityWithPrimitiveValues.intValue INTEGER (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EntityWithPrimitiveValues.longValue LONG (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EntityWithPrimitiveValues.shortValue SHORT (NOTNULL, MANDATORY TRUE);
CREATE PROPERTY EntityWithPrimitiveValues.decimalValue DECIMAL (NOTNULL, MANDATORY TRUE);
