package bridge.implementor;

public class WaterBreathImplementation implements BreathImplementor {

    @Override
    public void breath() {
        System.out.println("Breath through gills");
        System.out.println("Inhals o2 and exhales co2");
    }

}
