package bridge.abstractor;

import bridge.implementor.BreathImplementor;

public class Fish extends LivingThings {

    public Fish(BreathImplementor breathImplementor) {
        super(breathImplementor);
    }

    @Override
    public void breathingProcess() {
        breathImplementor.breath();
    }

}
