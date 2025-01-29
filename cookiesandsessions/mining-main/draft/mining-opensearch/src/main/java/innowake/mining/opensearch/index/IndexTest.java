package innowake.mining.opensearch.index;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.opensearch.controller.model.aggregations.AggregationRequestString;
import innowake.mining.shared.model.aggregations.AggregationOperator;

import static org.junit.jupiter.api.Assertions.assertTrue;


/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
/**
 * FIXME: description
 */
class IndexTest {
	
	@Autowired
	IndexService indexService = new IndexService(null, null, null, 1);

	@Test
	void test() {
		final AggregationRequestString agr = new AggregationRequestString();
		agr.setFields(Map.of("LINES_OF_CODE", AggregationOperator.SUM));
		agr.setOrderBy(List.of("LINES_OF_CODE"));
		agr.setGroupBy(Set.of("TECHNOLOGY", "TYPE"));
		
		indexService.queryIndex("blah", 2L, agr);
		assertTrue(true);
	}
	
	@Test
	void test2() {
		final AggregationRequestString agr = new AggregationRequestString();
		agr.setFields(Map.of("LINES_OF_CODE", AggregationOperator.SUM));
		agr.setOrderBy(List.of("LINES_OF_CODE"));
		agr.setGroupBy(Set.of("TECHNOLOGY", "TYPE"));
		
		System.out.println(indexService.queryIndex("moduletable", 2L, agr));
		assertTrue(true);
	}
	
//	@Test
//	void test3() {
//		indexService.textSearch("aa", 2L);
//		System.out.println("haa");
//		assertTrue(true);
//	}
}
