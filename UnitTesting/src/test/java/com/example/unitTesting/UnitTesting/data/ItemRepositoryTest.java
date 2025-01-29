package com.example.unitTesting.UnitTesting.data;

import java.util.List;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;

import com.example.unitTesting.UnitTesting.model.Item;

// This annotations is helpful to launch in-memory database and create tables and load the data inside the tables
// If you are using mysql or postgresql then you have to keep the data.sql file in src/test/resources then it would automatically connect to in-memory database
@DataJpaTest
public class ItemRepositoryTest {

	@Autowired
	private ItemRepository itemRepository;
	
	@Test
	public void testFindAll() {
		List<Item> items = itemRepository.findAll();
		assertEquals(3, items.size());
		
	}
	
}
