package singleton_package;

public class SingletonThreadSafe {
    private static SingletonThreadSafe threadSafeInstance;

    private SingletonThreadSafe() {
        System.out.println("instance created");

    }

    // using synchronized block
    // This is preferrable
    public static SingletonThreadSafe getInstance() {
        synchronized (SingletonThreadSafe.class) {
            if (threadSafeInstance == null) {
                threadSafeInstance = new SingletonThreadSafe();
            }
        }
        return threadSafeInstance;
    }

    /*
     * This is called double checked locking
     * we are not making entire method as synchronized because it will reduce
     * performance
     * we are first time checking if object is null then in the synchronized block
     * we are again checking 2nd time if the object is null
     * That is why we call this concept as double checked locking
     * if for example 2 threads are running at the same time and they both entered
     * getInstance method and they both checks that object is null and one thread
     * will access synchronized block at a time
     * so let say thread1 accessed it and created object
     * so then thread2 will access synchronized block then again check if object is
     * null or not since thread1 is already created we don't create object again
     */
    // public static SingletonThreadSafe getInstance() {
    // if (threadSafeInstance == null) {
    // synchronized (SingletonThreadSafe.class) {
    // if (threadSafeInstance == null) {
    // threadSafeInstance = new SingletonThreadSafe();
    // }
    // }

    // }

    // return threadSafeInstance;
    // }

    // using synchronized keyword for the entire method
    // public synchronized static SingletonThreadSafe getInstance() {
    // if (threadSafeInstance == null) {
    // threadSafeInstance = new SingletonThreadSafe();
    // }
    // return threadSafeInstance;
    // }

}
