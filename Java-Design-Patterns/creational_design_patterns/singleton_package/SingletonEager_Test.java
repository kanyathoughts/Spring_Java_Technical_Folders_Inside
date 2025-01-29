package singleton_package;

public class SingletonEager_Test {
    public static void main(String[] args) {
        SingletonEager s1 = SingletonEager.getInstance();
        SingletonEager s2 = SingletonEager.getInstance();

        System.out.println(s1);
        System.out.println(s2);

    }

}
