package singleton_package;

public class SingletonBillPuge_Test {

    public static void main(String[] args) {
        SingletonBillPuge s1 = SingletonBillPuge.getInstance();
        SingletonBillPuge s2 = SingletonBillPuge.getInstance();

        System.out.println(s1);
        System.out.println(s2);
    }

}
