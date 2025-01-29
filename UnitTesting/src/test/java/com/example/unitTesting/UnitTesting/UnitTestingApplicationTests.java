package com.example.unitTesting.UnitTesting;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;

@SpringBootTest
@TestConfiguration("classpath:configutaion.properties")
class UnitTestingApplicationTests {

	@Test
	void contextLoads() {
	}

}
