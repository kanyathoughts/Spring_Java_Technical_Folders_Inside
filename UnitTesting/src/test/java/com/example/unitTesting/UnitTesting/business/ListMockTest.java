package com.example.unitTesting.UnitTesting.business;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

class ListMockTest {
	List<String> mock = mock(List.class);

	@Test
	void size_basic() {
		when(mock.size()).thenReturn(5);
		assertEquals(5, mock.size());
	}
	
	// Returning multiple values
	@Test
	public void returnMultipleValues() {
		when(mock.size()).thenReturn(56).thenReturn(34);
		assertEquals(56, mock.size()); // first return 56
		assertEquals(34, mock.size()); // 2nd time return 34
	}
	
	@Test
	public void returnWithParameters() {
		when(mock.get(0)).thenReturn("kanya");
		assertEquals("kanya", mock.get(0));
		assertEquals(null, mock.get(1)); 
		// we have specified "kanya" value at 0 position and remaining all will return default values
	}
	
	@Test
	public void returnWithAnyParameters() {
		// inside Mockito class we have anyInt class where if we access any integer then it will return "kanya" only
		when(mock.get(anyInt())).thenReturn("kanya");
		assertEquals("kanya", mock.get(0));
		assertEquals("kanya", mock.get(1)); 
	}
	
	@Test
	public void verification_Basics() {
		String value1 = mock.get(0);
		String value2 = mock.get(1);
		
		// Here we are verifying whether get method is called with value 0 on mock or not
		verify(mock).get(0);
		// Here we are verifying get method with any integer is called twice on mock
		verify(mock, times(2)).get(anyInt());
		// Here we are verifying get method with any integer is called at least once on mock
		verify(mock, atLeastOnce()).get(anyInt());
		// Here we are verifying get method with any integer is called at most twice on mock
		verify(mock, atMost(2)).get(anyInt());
		// Here we are verifying get method is never called with value 2
		verify(mock, never()).get(2);
	}
	
	@Test
	public void argumentCapturing() {
		// Here if you want to know which argument is passed to method on mock
		mock.add("Something");
		ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
		verify(mock).add(captor.capture());
		System.out.println("captured value is: " + captor.getValue());
		
		
		Student studentMock = mock(Student.class);
		studentMock.setName("Akhil");
		studentMock.setAge(27);
		studentMock.setGender("M");
		
		// Create ArgumentCaptors for each method call
        ArgumentCaptor<String> nameCaptor = ArgumentCaptor.forClass(String.class);
        ArgumentCaptor<Integer> ageCaptor = ArgumentCaptor.forClass(Integer.class);
        ArgumentCaptor<String> genderCaptor = ArgumentCaptor.forClass(String.class);

        // Verify the interactions and capture the arguments
        verify(studentMock).setName(nameCaptor.capture());
        verify(studentMock).setAge(ageCaptor.capture());
        verify(studentMock).setGender(genderCaptor.capture());

        // Verify the state of the captured arguments
        System.out.println("Name: " + nameCaptor.getValue());
        System.out.println("Age: " + ageCaptor.getValue());
        System.out.println("Gender: " + genderCaptor.getValue());
        
	}
	
	@Test
	public void multipleArgumentsCapturing() {
		// Here if you want to know which argument is passed to method on mock
		mock.add("Something1");
		mock.add("Something2");
		ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
		verify(mock, times(2)).add(captor.capture());
		System.out.println("captured values are: " + captor.getAllValues());
	}
	
	@Test
	public void mocking() {
		List mock = mock(ArrayList.class);
		System.out.println(mock.get(0)); //null
		System.out.println(mock.size()); //0
		mock.add("test1");
		mock.add("test2");
		System.out.println(mock.size()); //0
		when(mock.size()).thenReturn(5);
		System.out.println(mock.size()); //5
		mock.add("test2");
		System.out.println(mock.size()); //5
	}
	
	/*
	 * Difference between mocking and spying 1: Mocking will not retain original
	 * behavior of real object 2: Spying will retain original behavior of real
	 * object when we use spy is, when we don't want mocked dependencies and we
	 * would want to use real type of dependencies then spy is helpful.
	 */
	
	@Test
	public void spying() {
		List spy = spy(ArrayList.class);
//		System.out.println(spy.get(0)); 
		//indexOutOfBoundException because it will have original behavior of class so as no elements present inside list we get this exception
		System.out.println(spy.size()); //0
		spy.add("test1");
		spy.add("test2");
		System.out.println(spy.size()); //2
		when(spy.size()).thenReturn(5);
		System.out.println(spy.size()); //5
		spy.add("test3");
		System.out.println(spy.size()); //5 
		// Even after adding elements still you can see the same size as 5 because we have set this size only
		// Once you set the value only that value will be returned from that method in both mocking and spying
	}
	
	
	
	class Student {
		private String name;
		private int age;
		private String gender;
		
		public String getGender() {
			return gender;
		}
		public void setGender(String gender) {
			this.gender = gender;
		}
		public String getName() {
			return name;
		}
		public void setName(String name) {
			this.name = name;
		}
		public int getAge() {
			return age;
		}
		public void setAge(int age) {
			this.age = age;
		}
		@Override
		public String toString() {
			return "Student [name=" + name + ", age=" + age + ", gender=" + gender + "]";
		}
		public Student(String name, int age, String gender) {
			super();
			this.name = name;
			this.age = age;
			this.gender = gender;
		}
	}

}
