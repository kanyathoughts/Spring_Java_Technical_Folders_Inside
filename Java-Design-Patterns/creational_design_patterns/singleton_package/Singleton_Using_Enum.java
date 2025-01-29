package singleton_package;

public class Singleton_Using_Enum {

    public static void main(String[] args) {
        // we have set the i value to 5 in obj1
        A obj1 = A.INSTANCE;
        obj1.i = 5;
        obj1.show();
        // we have set the i value to 6 in obj2 but when you print i value using obj1
        // then it displays 6 because we have only one object here so i value got
        // updated
        A obj2 = A.INSTANCE;
        obj2.i = 6;
        obj1.show();
    }

}

/*
 * By default all the constants in enum are objects
 * By default enum supports private constructor beacuse we are creating objects
 * inside the enum it self (which are constants here)
 */
enum A {
    INSTANCE;

    // This INSTANCE object refers to below method we can assume
    // public static A getInstance() {

    // }

    int i;

    public void show() {
        System.out.println(i);
    }
}
