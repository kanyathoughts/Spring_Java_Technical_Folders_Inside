package business_logic;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import entity.Employee;

public class HighestSalriedEmployee {
	
	public static void main(String[] args) {
		
		List<Employee> employeeList = new ArrayList<Employee>();
		
		employeeList.add(new Employee("kanya", "kumari", 25, 56000.00));
		employeeList.add(new Employee("jashu", "Manigandla", 05, 6000.00));
		employeeList.add(new Employee("Thanvitha", "Manigandla", 01, 60000.00));
		
		Optional<Employee> first = employeeList.stream().sorted(Comparator.comparingDouble(Employee::getSalary).reversed()).findFirst();
		
		Optional<Employee> first2 = employeeList.stream().sorted(Comparator.comparingDouble(v -> ((Employee) v).getSalary()).reversed()).findFirst();
		System.out.println(first2);
		
		/*Understanding the difference between method referencing and method invocation in Java is crucial for writing clear and effective code. Here’s a breakdown of both concepts:

			Method Invocation
			Method invocation is the process of calling a method on an object or a class. This action executes the method's code and returns a result if the method is designed to do so. Method invocation is an active process where the method's functionality is carried out immediately at the point of the call.

			Syntax Examples:

			Instance method invocation: object.methodName(arguments);
			Static method invocation: ClassName.methodName(arguments);
			Example:

			Java
			String example = "Hello, World!";
			int length = example.length(); // This invokes the length method on the string object.
			In this example, .length() is invoked on example, and it actively calculates and returns the length of the string.

			Method Reference
			Method referencing, on the other hand, is a feature introduced in Java 8 as part of its functional programming capabilities. It refers to methods without invoking them. Method references are used primarily to pass a reference of a method as an argument to another method, often a higher-order function such as those found in streams or with functional interfaces.

			Method references are a way to point directly to a method without executing it. They provide a way to simplify expressions where a method is being used as a lambda expression.

			Syntax Examples:

			Reference to a static method: ClassName::methodName
			Reference to an instance method of a particular object: instance::methodName
			Reference to an instance method of an arbitrary object of a particular type: ClassName::methodName
			Reference to a constructor: ClassName::new
			Example:

			Java
			List<String> messages = Arrays.asList("Hello", "World", "!");
			messages.forEach(System.out::println); // Method reference
			Here, System.out::println is a method reference that points to the println method of the System.out object. It does not invoke the println method directly; instead, it passes a reference to this method to the forEach function, which then handles the invocation for each element in the messages list.

			Key Differences
			Execution: Method invocation executes a method and uses the result immediately, while method referencing passes the method as a value (a reference) without executing it.
			Purpose: Invocation is used when you want to perform the action of a method. Referencing is used when you want to pass the method as behavior to another method or when the method execution is to be deferred.
			Context: Invocation is straightforward and direct. Referencing is typically used in functional programming contexts within Java, such as in stream operations or with APIs that accept functional interfaces.
			Understanding these differences helps in leveraging Java’s functional programming features effectively, enhancing both the readability and flexibility of your code. */
		
	}

}
