enum Student
{
	John(11), Bella(10), Sam(13);
	private int age;                   //variable defined in enum Student
	int getage() { return age; }  //method defined in enum Student
	private Student(int age)  //constructor defined in enum Student
	{
		this.age= age;
	}
}
class EnumDemo
{
	public static void main( String args[] )
	{
		Student S;
		System.out.println("Age of Sam is " +Student.Sam.getage()+ " years");
	}
}