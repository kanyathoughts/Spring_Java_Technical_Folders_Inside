package prototype_package_another_example;

public class Student implements Prototype_Example {

    private int rollNo;
    private String name;
    private int age;

    public int getRollNo() {
        return rollNo;
    }

    public void setRollNo(int rollNo) {
        this.rollNo = rollNo;
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

    public Student(int rollNo, String name, int age) {
        this.rollNo = rollNo;
        this.name = name;
        this.age = age;
    }

    @Override
    public String toString() {
        return "Student [rollNo=" + rollNo + ", name=" + name + ", age=" + age + "]";
    }

    // Here I have one doubt, if Student class is expensive class and creating
    // object of it is very expensive then why are we creating again
    // May be these values will be passed from original object
    @Override
    public Prototype_Example clone_copy() {
        System.out.println(rollNo);
        System.out.println(name);
        System.out.println(age);
        return new Student(rollNo, name, age);
    }

}
