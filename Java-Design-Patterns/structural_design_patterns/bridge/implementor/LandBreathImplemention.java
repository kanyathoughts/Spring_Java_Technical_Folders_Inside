package bridge.implementor;

public class LandBreathImplemention implements BreathImplementor {

    @Override
    public void breath() {
        System.out.println("Breath through nose");
        System.out.println("Inhals o2 and exhales co2");
    }

}
