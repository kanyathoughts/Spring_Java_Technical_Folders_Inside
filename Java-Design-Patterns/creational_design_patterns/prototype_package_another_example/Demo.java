package prototype_package_another_example;

public class Demo {
    public static void main(String[] args) throws CloneNotSupportedException {
        System.out.println("===================================================");
        Student s1 = new Student(1, "kanya", 25);
        System.out.println(s1.toString());

        Student s2 = (Student) s1.clone_copy();
        s1.setAge(30);
        System.out.println(s1.toString());
        System.out.println(s2.toString());
        s2.setAge(40);

        Student s3 = (Student) s2.clone_copy();

        Teacher t1 = new Teacher("Jashu", "Maths");
        Teacher t2 = (Teacher) t1.clone_copy();
        System.out.println(t1.toString());
        System.out.println(t2.toString());

        System.out.println("===================================================");
        t1.setSubject("Physics");
        System.out.println(t1.toString());
        System.out.println(t2.toString());

        System.out.println("===================================================");

        Principal p1 = new Principal("Akhil", "Sarthak college");
        Principal p2 = (Principal) p1.clone();
        System.out.println("p1: " + p1.toString());
        System.out.println("p2: " + p2.toString());

        // updating p1 name
        p1.setName("Sai Sri Ram");
        System.out.println("===================================================");

        // After updating printing p1 and p2
        System.out.println("p1: " + p1.toString());
        System.out.println("p2: " + p2.toString());

    }

}
