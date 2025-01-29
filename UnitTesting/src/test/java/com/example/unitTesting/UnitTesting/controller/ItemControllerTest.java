package com.example.unitTesting.UnitTesting.controller;

import org.assertj.core.util.Arrays;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.MockBeans;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.RequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import com.example.unitTesting.UnitTesting.business.ItemBusinessService;
import com.example.unitTesting.UnitTesting.model.Item;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@WebMvcTest
public class ItemControllerTest {

	@Autowired
	private MockMvc mockMVC;

	// It will create mock object for the ItemBusinessService
	// We are mocking the item business service out
	@MockitoBean
	private ItemBusinessService itemBusinessServiceMock;

	// After creating the ItemBusinessService mock object this annotation injects
	// this dependency into ItemController
	@InjectMocks
	private ItemController itemController;

	@Test
	public void dummyItemTest() throws Exception {
		MvcResult result = mockMVC.perform(MockMvcRequestBuilders.get("/dummy-item").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				.andExpect(content().json("{id:1,name:ball,price:10,quantity:20}")).andReturn();
	}

	// Here we are only checking the ItemController
	// Note: If service layer is not working also we won't get issue here as this
	// controller is independent
	@Test
	public void itemFromBusinessServiceTest() throws Exception {
		// if we don't add this step then by default mock objects will return null so
		// test will fail
		// here mock object is itemBusinessServiceMock and which is the mock object of
		// real itemBusinessService object
		// whenever you mock the object real behavior will be gone

		// Arrange
		Item mockItem = new Item(3, "towel", 12, 50);
		Mockito.when(itemBusinessServiceMock.retrieveHardcodedItemFromBusinessService()).thenReturn(mockItem);

		// Act and assert
		MvcResult result = mockMVC
				.perform(MockMvcRequestBuilders.get("/item-from-business-service").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk()).andExpect(content().json("{id:3,name:towel,price:12,quantity:50}"))
				.andReturn();
	}

	// This is unit test for controller layer where service layer is talking to data layer but it doesn't care we are mocking out service layer
	@Test
	public void retrieveAllItemsTest() throws Exception {
		// if we don't add this step then by default mock objects will return null so
		// test will fail
		// here mock object is itemBusinessServiceMock and which is the mock object of
		// real itemBusinessService object
		// whenever you mock the object real behavior will be gone

		// Arrange
		Item mockItem1 = new Item(1, "ball", 12, 50);
		Item mockItem2 = new Item(2, "bat", 100, 50);
		Item mockItem3 = new Item(3, "towel", 50, 50);
		List<Item> mockItems = new ArrayList<>();
		mockItems.add(mockItem1);
		mockItems.add(mockItem2);
		mockItems.add(mockItem3);
		Mockito.when(itemBusinessServiceMock.retrieveAllItems()).thenReturn(mockItems);

		// Act and assert
		MvcResult result = mockMVC
				.perform(MockMvcRequestBuilders.get("/all-items-from-database").accept(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk())
				// if you observer carefully we are not giving "value" field because we don't want to check but you can add it if you want to check
				// I have added 0 because for this value field we are calculating inside the business service but here we are mocking that out so this value will be 0
				.andExpect(content().json("[{id:1,name:ball,price:12,quantity:50,value:0},{id:2,name:bat,price:100"
						+ ",quantity:50},{id:3,name:towel,price:50,quantity:50}]"))
				// Here if you don't want to check 2nd and 3rd items then you can leave them as empty 
				// JSONAssert will not check strictly
				// But when you change values then test will fail
				.andExpect(content().json("[{id:1,name:ball,price:12,quantity:50},{},{}]"))
				.andReturn();
	}

}
