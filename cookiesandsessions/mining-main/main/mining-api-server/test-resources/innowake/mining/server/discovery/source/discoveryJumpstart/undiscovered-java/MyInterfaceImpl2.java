
/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
/**
 * Example MyInterfaceImpl
 */
import com.package1.MyInterface;
import com.package2.MyOtherInterface;

public class MyInterfaceImpl2 extends A implements IB, IC {
    private static class B extends BAbstractHelper {
        private A a;
        public B(A a, int size) {
            super(size);
            this.a = a;
        }

        @Override
        public boolean foo(int x, int y) {
            return a.foo(x, y);
        }
    }
    private static class C extends CAbstractHelper {
    }


    private B b;
    private C c;
    private int range;

    @Override
    public boolean foo(int x, int y) {
        return x*x + y*y <= range*range;
    }

    @Override
    public float bar(float x, int y, String s) {
        return b.bar(x,y,s);
    }

}

