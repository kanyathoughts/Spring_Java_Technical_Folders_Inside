/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.api.lang.NonNullByDefault;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataPointIgnore;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;

/**
 * Model class for {@link MiningDataPointSourceReflectionTest}
 */
@NonNullByDefault(false) /* disable null checks for this test model class */
public class TestModelClass {

	@MiningDataPoint(displayName = "Display Name", description = "Custom Description")
	private String stringProp;
	private int intProp;
	private long longProp;
	private float floatProp;
	private double doubleProp;
	
	@MiningDataPointIgnore
	private String ignoredProp;
	
	private TestOtherModelClass refProp;
	private TestOtherModelClass2 refProp2;
	private TestEnum enumProp;
	
	private List<Boolean> listBoolProp;
	private Set<String> setStringProp;
	private List<TestOtherModelClass> listRefProp;
	private Set<TestOtherModelClass2> setRefProp;
	
	private Map<String, String> mapStringStringProp;
	private Map<Integer, TestOtherModelClass> mapIntRefProp;
	
	private Map<String, Set<String>> mapStringWithCollectionType;
	private Map<String, Set<TestOtherModelClass>> mapStringWithCollectionReferenceType;
	
	@Usage("foo")
	private String propWithSingleUsage;
	
	@Usage(value = "bar", attributes = {
			@UsageAttribute(key = "the key", value = "the value"),
			@UsageAttribute(key = "the key2", value = "the value2")
	})
	@Usage("baz")
	private String propWithMultiUsage;

	@MiningDataPoint(name = "propWithCustomName")
	private String shouldNotSeeThis; /* test that data point is created with name from Annotation, not property name */

	@MiningDataPoint(type = TestEnum.class)
	private String stringAsEnumProp; /* test that data point is created with type from Annotation */

	@MiningDataPoint(referenceTypeName = "TestEnumWithCustomName")
	private String stringAsEnumProp2; /* same as above but using GraphQL schema type name instead of Class */

	@MiningDataPoint(scalarType = ScalarType.JSON)
	private Map<String, String> mapAsJsonProp; /* same as above but using a ScalarType */

	private Optional<String> optionalProp;
	
	//TODO: usages
	
	public String getStringProp() {
		return stringProp;
	}
	
	public void setStringProp(String stringProp) {
		this.stringProp = stringProp;
	}
	
	public int getIntProp() {
		return intProp;
	}
	
	public void setIntProp(int intProp) {
		this.intProp = intProp;
	}
	
	public long getLongProp() {
		return longProp;
	}
	
	public void setLongProp(long longProp) {
		this.longProp = longProp;
	}
	
	public float getFloatProp() {
		return floatProp;
	}
	
	public void setFloatProp(float floatProp) {
		this.floatProp = floatProp;
	}
	
	public double getDoubleProp() {
		return doubleProp;
	}
	
	public void setDoubleProp(double doubleProp) {
		this.doubleProp = doubleProp;
	}
	
	public String getIgnoredProp() {
		return ignoredProp;
	}
	
	public void setIgnoredProp(String ignoredProp) {
		this.ignoredProp = ignoredProp;
	}
	
	public TestOtherModelClass getRefProp() {
		return refProp;
	}
	
	public void setRefProp(TestOtherModelClass refProp) {
		this.refProp = refProp;
	}
	
	public TestOtherModelClass2 getRefProp2() {
		return refProp2;
	}
	
	public void setRefProp2(TestOtherModelClass2 refProp2) {
		this.refProp2 = refProp2;
	}
	
	public TestEnum getEnumProp() {
		return enumProp;
	}
	
	public void setEnumProp(TestEnum enumProp) {
		this.enumProp = enumProp;
	}

	
	public List<Boolean> getListBoolProp() {
		return listBoolProp;
	}

	
	public void setListBoolProp(List<Boolean> listBoolProp) {
		this.listBoolProp = listBoolProp;
	}

	
	public Set<String> getSetStringProp() {
		return setStringProp;
	}

	
	public void setSetStringProp(Set<String> setStringProp) {
		this.setStringProp = setStringProp;
	}

	
	public List<TestOtherModelClass> getListRefProp() {
		return listRefProp;
	}

	
	public void setListRefProp(List<TestOtherModelClass> listRefProp) {
		this.listRefProp = listRefProp;
	}

	
	public Set<TestOtherModelClass2> getSetRefProp() {
		return setRefProp;
	}

	
	public void setSetRefProp(Set<TestOtherModelClass2> setRefProp) {
		this.setRefProp = setRefProp;
	}

	
	public Map<String, String> getMapStringStringProp() {
		return mapStringStringProp;
	}

	
	public void setMapStringStringProp(Map<String, String> mapStringStringProp) {
		this.mapStringStringProp = mapStringStringProp;
	}

	
	public Map<Integer, TestOtherModelClass> getMapIntRefProp() {
		return mapIntRefProp;
	}

	
	public void setMapIntRefProp(Map<Integer, TestOtherModelClass> mapIntRefProp) {
		this.mapIntRefProp = mapIntRefProp;
	}
	
	public Map<String, Set<String>> getMapStringWithCollectionType() {
		return mapStringWithCollectionType;
	}

	public void setMapStringWithCollectionType(Map<String, Set<String>> mapStringWithCollectionType) {
		this.mapStringWithCollectionType = mapStringWithCollectionType;
	}
	
	public Map<String, Set<TestOtherModelClass>> getMapStringWithCollectionReferenceType() {
		return mapStringWithCollectionReferenceType;
	}

	public void setMapStringWithCollectionReferenceType(Map<String, Set<TestOtherModelClass>> mapStringWithCollectionReferenceType) {
		this.mapStringWithCollectionReferenceType = mapStringWithCollectionReferenceType;
	}

	public String getPropWithSingleUsage() {
		return propWithSingleUsage;
	}

	
	public void setPropWithSingleUsage(String propWithSingleUsage) {
		this.propWithSingleUsage = propWithSingleUsage;
	}

	
	public String getPropWithMultiUsage() {
		return propWithMultiUsage;
	}

	
	public void setPropWithMultiUsage(String propWithMultiUsage) {
		this.propWithMultiUsage = propWithMultiUsage;
	}

	public String getShouldNotSeeThis() {
		return shouldNotSeeThis;
	}

	public void setShouldNotSeeThis(final String shouldNotSeeThis) {
		this.shouldNotSeeThis = shouldNotSeeThis;
	}

	public String getStringAsEnumProp() {
		return stringAsEnumProp;
	}

	public void setStringAsEnumProp(final String stringAsEnumProp) {
		this.stringAsEnumProp = stringAsEnumProp;
	}

	public String getStringAsEnumProp2() {
		return stringAsEnumProp2;
	}

	public void setStringAsEnumProp2(final String stringAsEnumProp2) {
		this.stringAsEnumProp2 = stringAsEnumProp2;
	}

	public Map<String, String> getMapAsJsonProp() {
		return mapAsJsonProp;
	}

	public void setMapAsJsonProp(final Map<String, String> mapAsJsonProp) {
		this.mapAsJsonProp = mapAsJsonProp;
	}

	public Optional<String> getOptionalProp() {
		return optionalProp;
	}

	public void setOptionalProp(final Optional<String> optionalProp) {
		this.optionalProp = optionalProp;
	}
}
