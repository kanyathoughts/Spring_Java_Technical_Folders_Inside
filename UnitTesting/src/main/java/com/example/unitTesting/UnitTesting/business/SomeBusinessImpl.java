package com.example.unitTesting.UnitTesting.business;

import java.util.Arrays;

import com.example.unitTesting.UnitTesting.data.SomeDataService;

public class SomeBusinessImpl {

	SomeDataService someDataService;
	
	public void setSomeDataService(SomeDataService someDataService) {
		this.someDataService = someDataService;
	}

	public int calculateSum(int[] data) {
		/*
		 * A reduction operation (also called a fold) takes a sequence of input elements and combines them into a single summary result by repeated application of a combining operation, such as finding the sum or maximum of a set of numbers, or accumulating elements into a list. The streams classes have multiple forms of general reduction operations, called reduce() and collect(), as well as multiple specialized reduction forms such as sum(), max(), or count().
Of course, such operations can be readily implemented as simple sequential loops, as in:


    int sum = 0;
    for (int x : numbers) {
       sum += x;
    }
 
However, there are good reasons to prefer a reduce operation over a mutative accumulation such as the above. Not only is a reduction "more abstract" -- it operates on the stream as a whole rather than individual elements -- but a properly constructed reduce operation is inherently parallelizable, so long as the function(s) used to process the elements are associative and stateless. For example, given a stream of numbers for which we want to find the sum, we can write:

    int sum = numbers.stream().reduce(0, (x,y) -> x+y);
 
or:

    int sum = numbers.stream().reduce(0, Integer::sum);
 
These reduction operations can run safely in parallel with almost no modification:


    int sum = numbers.parallelStream().reduce(0, Integer::sum);
		 */
		return Arrays.stream(data).reduce(Integer::sum).orElse(0);
//		int sum = 0;
//		for (int value : data) {
//			sum += value;
//		}
//		return sum;
	}
	
	public int calculateSumUsingSomeDataService() {
		int sum = 0;
		int[] data = someDataService.retrieveAllData();
		for (int value : data) {
			sum += value;
		}
		return sum;
	}
}
