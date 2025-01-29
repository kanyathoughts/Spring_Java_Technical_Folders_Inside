package entity;

public class Student implements Comparable<Student>{
	
	private String name;
	private int age;
	
	
	public Student(String name, int age) {
		super();
		this.name = name;
		this.age = age;
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
	public int compareTo(Student o) {
		if (this.age > o.age) {
			return 1; //1 means swap to get ascending order
		} else if (this.age < o.age) {
			return -1; // -1 means not swap
		} else {
			return 0;
		}
	}
	
	

}
