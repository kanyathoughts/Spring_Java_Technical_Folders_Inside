package singleton_package;

public class SingletonStaticBlock_Test {
    public static void main(String[] args) {
        SingletonStaticBlock s1 = SingletonStaticBlock.getInstance();
        SingletonStaticBlock s2 = SingletonStaticBlock.getInstance();

        System.out.println(s1);
        System.out.println(s2);
    }

}
