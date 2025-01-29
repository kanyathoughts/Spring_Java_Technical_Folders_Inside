package com.example.unitTesting.UnitTesting.business;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import com.example.unitTesting.UnitTesting.data.ItemRepository;
import com.example.unitTesting.UnitTesting.model.Item;


// Here we are checking the business/service layer in the app and we are just using the mockito framework 
@ExtendWith(MockitoExtension.class)
public class ItemBusinessServiceTest {
	
	// We have to mock out the data layer here as it is dependent on data layer
	
	@Mock
	ItemRepository itemRepositoryMock;
	
	
	// We have to inject data layer Dependencies in service layer
	@InjectMocks
	ItemBusinessService itemBusinessService;
	
	
	// This method is for testing the service layer method
	@Test
	public void ItemBusinessServiceTest() {
		Item mockItem1 = new Item(1, "ball", 12, 50);
		Item mockItem2 = new Item(2, "bat", 100, 50);
		Item mockItem3 = new Item(3, "towel", 50, 50);
		List<Item> mockItems = new ArrayList<>();
		mockItems.add(mockItem1);
		mockItems.add(mockItem2);
		mockItems.add(mockItem3);
		// Arrange
		Mockito.when(itemRepositoryMock.findAll()).thenReturn(mockItems);
		
		// Act
		List<Item> items = itemBusinessService.retrieveAllItems();
		
		// Assert
		assertEquals(mockItems, items);
		// Here we are checking if business logic is working fine
		// Inside the retrieveAllItems method, after getting values from database we are setting the value for "value" field 
		// so that logic should be working properly and we have to check that logic testing
		assertEquals(600, items.get(0).getValue());
		assertEquals(5000, items.get(1).getValue());
		assertEquals(2500, items.get(2).getValue());
	}

}
