package entity;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ComparablePractice {
	
	public static void main(String[] args) {
		List<Student> list = new ArrayList<Student>();
		list.add(new Student("Kanya", 25));
		list.add(new Student("Jashu", 5));
		list.add(new Student("Tattu", 1));
		list.add(new Student("Akhil", 27));
		
		list.forEach(v -> System.out.println(v.getName() + " " + v.getAge()));
		
		Collections.sort(list);
		System.out.println("=================After sorting==================");
		list.forEach(v -> System.out.println(v.getName() + " " + v.getAge()));
				
		System.out.println("===================== After sorting using Age comparator=====================");
		Collections.sort(list, new AgeComparator());
		list.forEach(v -> System.out.println(v.getName() + " " + v.getAge()));
		
		System.out.println("===================== After sorting using Name comparator=====================");
		Collections.sort(list, new NameComparator());
		list.forEach(v -> System.out.println(v.getName() + " " + v.getAge()));
		
		System.out.println("===================== reverse sorting using Name comparator =====================");
		Collections.sort(list, new NameComparator().reversed());
		list.forEach(v -> System.out.println(v.getName() + " " + v.getAge()));
	}

}
