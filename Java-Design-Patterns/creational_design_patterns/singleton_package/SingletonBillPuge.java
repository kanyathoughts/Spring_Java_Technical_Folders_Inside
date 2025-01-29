package singleton_package;

public class SingletonBillPuge {
    private SingletonBillPuge() {

    }

    private static class InnerStaticClass {
        private static final SingletonBillPuge instance = new SingletonBillPuge();
    }

    public static SingletonBillPuge getInstance() {
        return InnerStaticClass.instance;
    }

}