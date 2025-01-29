package com.example.unitTesting.UnitTesting.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.example.unitTesting.UnitTesting.business.ItemBusinessService;
import com.example.unitTesting.UnitTesting.model.Item;

@RestController
public class ItemController {
	
	@Autowired
	private ItemBusinessService itemBusinessService;
	
	@GetMapping("/dummy-item")
	public Item dummyItem() {
		return new Item(1, "ball", 10, 20);
	}
	
	@GetMapping("/item-from-business-service")
	public Item itemFromBusinessService() {
		return itemBusinessService.retrieveHardcodedItemFromBusinessService();
	}
	
	@GetMapping("/all-items-from-database")
	public List<Item> retrieveAllItems() {
		return itemBusinessService.retrieveAllItems();
	}

}
