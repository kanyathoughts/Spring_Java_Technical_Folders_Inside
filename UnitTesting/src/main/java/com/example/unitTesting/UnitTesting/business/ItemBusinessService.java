package com.example.unitTesting.UnitTesting.business;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.example.unitTesting.UnitTesting.data.ItemRepository;
import com.example.unitTesting.UnitTesting.model.Item;

@Component
public class ItemBusinessService {

	@Autowired
	ItemRepository itemRepository;
	
	public Item retrieveHardcodedItemFromBusinessService() {
		return new Item(2, "bat", 100, 33);
	}
	
	public List<Item> retrieveAllItems() {
		// Adding application related business logic here
		List<Item> items = itemRepository.findAll();
		for (Item item : items) {
			item.setValue(item.getPrice() * item.getQuantity());
		}
		return items;
	}
}
