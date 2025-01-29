package singleton_package;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

public class Breaking_Singleton_Design_Pattern {
    public static void main(String[] args) throws CloneNotSupportedException, IOException, ClassNotFoundException,
            NoSuchMethodException, SecurityException, InstantiationException, IllegalAccessException,
            IllegalArgumentException, InvocationTargetException {
        B obj1 = B.getInstance();
        B obj2 = B.getInstance();
        System.out.println("obj1 hashcode: " + obj1.hashCode());
        System.out.println("obj2 hashcode: " + obj2.hashCode());

        // Break singleton design pattern using cloning
        // when you clone object then new object will be created in different memory
        // location
        // so you have broken the concept of singleton design pattern
        B clonedObject = (B) obj1.clone();
        System.out.println("clonedObject hashcode: " + clonedObject.hashCode());

        // Break singleton design pattern using serialization
        File file = new File("./file.ser");
        FileOutputStream fos = new FileOutputStream(file);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(obj1);
        oos.close();

        FileInputStream fis = new FileInputStream(file);
        ObjectInputStream ois = new ObjectInputStream(fis);
        B deserializedObject = (B) ois.readObject();
        ois.close();

        System.out.println("deserializedObject hashcode: " + deserializedObject.hashCode());

        // Break singleton design pattern using Reflection
        Class<?> newB = Class.forName("singleton_package.B");
        @SuppressWarnings("unchecked")
        Constructor<B> constructor = (Constructor<B>) newB.getDeclaredConstructor();
        constructor.setAccessible(true);
        B reflectedObject = constructor.newInstance();

        System.out.println("reflectedObject hashcode: " + reflectedObject.hashCode());
    }

}

class B implements Cloneable, Serializable {

    @Override
    protected Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    private static B instance;

    private B() {

    }

    public static B getInstance() {
        if (instance == null) {
            instance = new B();
        }
        return instance;
    }
}
