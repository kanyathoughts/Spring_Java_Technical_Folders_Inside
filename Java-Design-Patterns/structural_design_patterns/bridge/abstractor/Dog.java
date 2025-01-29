package bridge.abstractor;

import bridge.implementor.BreathImplementor;

public class Dog extends LivingThings {

    public Dog(BreathImplementor breathImplementor) {
        super(breathImplementor);
    }

    @Override
    public void breathingProcess() {
        breathImplementor.breath();
    }

}
