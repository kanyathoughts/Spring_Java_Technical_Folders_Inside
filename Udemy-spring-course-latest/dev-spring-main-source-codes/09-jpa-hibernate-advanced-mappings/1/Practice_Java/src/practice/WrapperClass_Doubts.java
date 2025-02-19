package practice;

public class WrapperClass_Doubts {
	
	public static void main(String[] args) {
		
		Integer num1 = 100;
		Integer num2 = 100;
		
		Integer num3 = 1000;
		Integer num4 = 1000;
		
		
		System.out.println((num1 == num2) + " " + (num1.equals(num2))); //true true
		System.out.println((num3 == num4) + " " + (num3.equals(num4))); //false true
		
		/*
		 * The output for the first System.out.println is true true. 
		 * This is because both num1 and num2 are assigned the value 100, 
		 * which falls within the range of Java’s integer cache (-128 to 127). 
		 * Java utilizes this cache for autoboxing of integer values within this range, 
		 * so both num1 and num2 point to the same cached Integer object.
		 * 
		 * The output for the second System.out.println is false true. 
		 * The values 1000 assigned to num3 and num4 do not fall within the cached range, 
		 * so they are stored as separate Integer objects. Therefore, num3 == num4 is false (different memory addresses), 
		 * but num3.equals(num4) is true (same integer value).
		 */
		
		/*
		 * == operator is used for primitive data types and object references
		 * so in primitive data types, 5 == 5 returns true which checks for the content
		 * in object references, it checks for memory address, if memory address is same then it will return true otherwise false
		 * 
		 * .equals() basically used for checking contents of objects instead of memory address.
		 */
		
		String s1 = new String("hello");
		String s2 = new String("hello");

		// Using == operator
		System.out.println(s1 == s2);  // Output: false because s1 and s2 are different objects in memory.

		// Using .equals() method
		System.out.println(s1.equals(s2));  // Output: true because s1 and s2 have the same characters in the same order.
		
		Float f1 = 100.6f;
		Float f2 = 100.6f;
		
		
		Float f3 = 1000.6f;
		Float f4 = 1000.6f;
		
		//Float doesn't have caching concept in java
		// it always creates new object
		
		System.out.println((f1 == f2) + " " + f1.equals(f2)); //false true
		System.out.println((f3 == f4) + " " + f3.equals(f4)); //false true
		
		/*public class StringCachingExample {
		    public static void main(String[] args) {
		        // Example with String literals
		        String s1 = "Deloitte";  // String literal
		        String s2 = "Deloitte";  // Reuses the string from the pool

		        System.out.println(s1 == s2);  // Outputs true because both refer to the same instance

		        // Example with String objects created with 'new' keyword
		        String s3 = new String("Deloitte");
		        String s4 = new String("Deloitte");

		        System.out.println(s3 == s4);  // Outputs false because different objects are created

		        // Interning the strings manually
		        s3 = s3.intern();  // This moves or finds the string in the pool
		        s4 = s4.intern();  // This also moves or finds the string in the pool

		        System.out.println(s3 == s4);  // Outputs true because both now refer to the same instance from the pool
		    }
		} */
		
		//Double doesn't have caching concept in java
		// it always creates new object
		Double d1 = 100.4d;
		Double d2 = 100.4d;
		Double d3 = 1000.4d;
		Double d4 = 1000.4d;
		
		System.out.println((d1 == d2) + " " + d1.equals(d2)); //false true
		System.out.println((d3 == d4) + " " + d3.equals(d4)); //false true
		
		//Byte
		Byte b1 = 100;
		Byte b2 = 100;
		
		Byte b3 = 127;
		Byte b4 = 127;
		
		System.out.println((b1 == b2) + " " + b1.equals(b2)); 
		System.out.println((b3 == b4) + " " + b3.equals(b4)); 
		
		//short
		
		// Short also has caching capability between -128 and 127
		// Within this range only memory address will be same as new object will not be created.
		Short short1 = -129;
		Short short2 = -129;
		Short short3 = 1000;
		Short short4 = 1000;
		
		System.out.println((short1 == short2) + " " + short1.equals(short2));
		System.out.println((short3 == short4) + " " + short3.equals(short4)); 
		
		
		
		
	}

}
