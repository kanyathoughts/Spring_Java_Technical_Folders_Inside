package com.example.unitTesting.UnitTesting.controller;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONException;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyscreamer.jsonassert.JSONAssert;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import com.example.unitTesting.UnitTesting.data.ItemRepository;
import com.example.unitTesting.UnitTesting.model.Item;

// This annotation is very powerful annotation which launches the entire application and in-memory database also
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class ItemControllerITTest {
	
	// It is same as "Rest Template" which is useful for making api requests
	@Autowired
	private TestRestTemplate testRestTemplate;
	
	
	// Mocking out Data layer here
	@MockitoBean
	private ItemRepository itemRepository;
	
	@Test
	public void contextLoads() throws JSONException {
		// Arrange
		Item mockItem1 = new Item(1, "ball", 12, 50);
		Item mockItem2 = new Item(2, "bat", 100, 50);
		Item mockItem3 = new Item(3, "towel", 50, 50);
		List<Item> mockItems = new ArrayList<>();
		mockItems.add(mockItem1);
		mockItems.add(mockItem2);
		mockItems.add(mockItem3);
		Mockito.when(itemRepository.findAll()).thenReturn(mockItems);
		
		// Act
		String response = testRestTemplate.getForObject("/all-items-from-database", String.class);
		
		// Assert
		JSONAssert.assertEquals("[{id:1},{id:2},{id:3}]", response, false);
	}

}
