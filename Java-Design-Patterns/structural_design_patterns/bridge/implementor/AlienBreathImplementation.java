package bridge.implementor;

public class AlienBreathImplementation implements BreathImplementor {

    @Override
    public void breath() {
        System.out.println("It doesn't breath");
    }

}
