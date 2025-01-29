package singleton_package;

public class SingletonThreadSafe_Test {
    public static void main(String[] args) {
        Thread t1 = new Thread(new Runnable() {

            @Override
            public void run() {
                SingletonThreadSafe s1 = SingletonThreadSafe.getInstance();
            }

        });
        Thread t2 = new Thread(new Runnable() {

            @Override
            public void run() {
                SingletonThreadSafe s1 = SingletonThreadSafe.getInstance();
            }

        });

        t1.start();
        t2.start();
    }

}
