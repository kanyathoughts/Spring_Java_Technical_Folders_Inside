package bridge.abstractor;

import bridge.implementor.BreathImplementor;

public abstract class LivingThings {

    BreathImplementor breathImplementor;

    LivingThings(BreathImplementor breathImplementor) {
        this.breathImplementor = breathImplementor;
    }

    public abstract void breathingProcess();

}
