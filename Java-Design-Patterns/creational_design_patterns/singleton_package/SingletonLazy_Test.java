package singleton_package;

public class SingletonLazy_Test {

    public static void main(String[] args) {
        SingletonLazy s1 = SingletonLazy.getInstance();
        SingletonLazy s2 = SingletonLazy.getInstance();

        System.out.println(s1);
        System.out.println(s2);
    }

}
