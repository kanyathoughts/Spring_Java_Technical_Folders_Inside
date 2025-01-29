package com.example.unitTesting.UnitTesting.business;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.*;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.exceptions.base.MockitoException;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.context.event.annotation.BeforeTestMethod;

import com.example.unitTesting.UnitTesting.data.SomeDataService;

@ExtendWith(MockitoExtension.class)
public class SomeBusinessMockTest {
	
	@InjectMocks
	SomeBusinessImpl business;
	
	@Mock
	SomeDataService someDataServiceMock = mock(SomeDataService.class);
	
//	@BeforeEach
//	public void before() {
//		business.setSomeDataService(someDataServiceMock);
//	}
	
	@Test
	void calculateSum_Basic() {
//		SomeBusinessImpl business = new SomeBusinessImpl();
		// Creating mock object
//		SomeDataService someDataServiceMock = mock(SomeDataService.class);
		// setting mock when particular method is called then return something
		when(someDataServiceMock.retrieveAllData()).thenReturn(new int[] {1,2,3});
		// setting up the mock object inside the other class for usage
//		business.setSomeDataService(someDataServiceMock);
		int actualResult = business.calculateSumUsingSomeDataService();
		int expectedResult = 6;
		assertEquals(expectedResult, actualResult);
	}
	
	@Test
	void calculateSum_Empty() {
//		SomeBusinessImpl business = new SomeBusinessImpl();
		// Creating mock object
//		SomeDataService someDataServiceMock = mock(SomeDataService.class);
		// setting mock when particular method is called then return something
		when(someDataServiceMock.retrieveAllData()).thenReturn(new int[] {});
		// setting up the mock object inside the other class for usage
//		business.setSomeDataService(someDataServiceMock);
		int actualResult = business.calculateSumUsingSomeDataService();
		int expectedResult = 0;
		assertEquals(expectedResult, actualResult);
	}
	
	@Test
	void calculateSum_OneValue() {
//		SomeBusinessImpl business = new SomeBusinessImpl();
		// Creating mock object
//		SomeDataService someDataServiceMock = mock(SomeDataService.class);
		// setting mock when particular method is called then return something
		when(someDataServiceMock.retrieveAllData()).thenReturn(new int[] {5});
		// setting up the mock object inside the other class for usage
//		business.setSomeDataService(someDataServiceMock);
		int actualResult = business.calculateSumUsingSomeDataService();
		int expectedResult = 5;
		assertEquals(expectedResult, actualResult);
	}

}
