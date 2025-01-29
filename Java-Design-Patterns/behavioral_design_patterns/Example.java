import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

class Student {
    String name;
    int age;

    public Student(String name, int age) {
        this.name = name;
        this.age = age;
    }

    @Override
    public String toString() {
        return "Student [name=" + name + ", age=" + age + "]";
    }

}

public class Example {
    public static void main(String[] args) {

        List<Integer> list = new ArrayList<>();
        list.add(1);
        list.add(2);
        list.add(3);

        Iterator<Integer> it = list.iterator();
        while (it.hasNext()) {
            System.out.print(it.next() + " ");
        }

        Set<Student> studentsSet = new HashSet<>();
        studentsSet.add(new Student("kanya", 25));
        studentsSet.add(new Student("akhil", 27));

        Iterator<Student> its = studentsSet.iterator();
        while (its.hasNext()) {
            System.out.println(its.next().toString());
        }

    }

}
