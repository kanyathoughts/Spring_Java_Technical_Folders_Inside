package com.example.unitTesting.UnitTesting.business;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.example.unitTesting.UnitTesting.business.SomeBusinessImpl;
import com.example.unitTesting.UnitTesting.data.SomeDataService;

class SomeDataServiceImpl implements SomeDataService {

	@Override
	public int[] retrieveAllData() {
		return new int[] {1,2,3};
	}
	
}

class SomeDataServiceEmptyImpl implements SomeDataService {

	@Override
	public int[] retrieveAllData() {
		return new int[] {};
	}
	
}

class SomeDataServiceSimgleImpl implements SomeDataService {

	@Override
	public int[] retrieveAllData() {
		return new int[] {5};
	}
	
}

public class SomeBusinessTestUsingSomeDataService {
	
	@Test
	void calculateSum_Basic() {
		SomeBusinessImpl business = new SomeBusinessImpl();
		business.setSomeDataService(new SomeDataServiceImpl());
		int actualResult = business.calculateSumUsingSomeDataService();
		int expectedResult = 6;
		assertEquals(expectedResult, actualResult);
	}
	
	@Test
	void calculateSum_Empty() {
		SomeBusinessImpl business = new SomeBusinessImpl();
		business.setSomeDataService(new SomeDataServiceEmptyImpl());
		int actualResult = business.calculateSumUsingSomeDataService();
		int expectedResult = 0;
		assertEquals(expectedResult, actualResult);
	}
	
	@Test
	void calculateSum_OneValue() {
		SomeBusinessImpl business = new SomeBusinessImpl();
		business.setSomeDataService(new SomeDataServiceSimgleImpl());
		int actualResult = business.calculateSumUsingSomeDataService();
		int expectedResult = 5;
		assertEquals(expectedResult, actualResult);
	}

}
