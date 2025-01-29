package factory_package;

import static org.junit.Assert.assertEquals;

import org.testng.annotations.Test;

public class FactoryMethodTest {

    Shape shape1 = ShapeFactory.getFood("Round");
    Shape shape2 = ShapeFactory.getFood("Cylinder");

    @Test
    void testShape() {
        assertEquals("Round", shape1.getShape());
        assertEquals("Cylinder", shape2.getShape());

    }

}
