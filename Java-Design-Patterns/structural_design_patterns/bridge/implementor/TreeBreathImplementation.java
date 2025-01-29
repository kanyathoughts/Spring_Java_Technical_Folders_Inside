package bridge.implementor;

public class TreeBreathImplementation implements BreathImplementor {

    @Override
    public void breath() {
        System.out.println("Breath through Leaves");
        System.out.println("Inhals co2 and exhales o2");
    }

}
