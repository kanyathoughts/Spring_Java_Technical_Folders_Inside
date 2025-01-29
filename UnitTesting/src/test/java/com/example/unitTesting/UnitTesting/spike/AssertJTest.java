package com.example.unitTesting.UnitTesting.spike;

import static org.hamcrest.CoreMatchers.everyItem;
import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.lessThan;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

// Hamcrest framework provides lot of matchers which really helps to assert the things in great way
public class AssertJTest {

	@Test
	public void learning() {
		List<Integer> numbers = Arrays.asList(12, 20, 45);
		
		
		
//		assertThat(numbers, hasSize(3));
//		assertThat(numbers, hasItems(12,20));
//		assertThat(numbers, everyItem(greaterThan(10)));
//		assertThat(numbers, everyItem(lessThan(100)));
		
	}
}
