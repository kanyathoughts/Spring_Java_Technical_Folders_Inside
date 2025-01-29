package bridge.abstractor;

import bridge.implementor.BreathImplementor;

public class Tree extends LivingThings {

    Tree(BreathImplementor breathImplementor) {
        super(breathImplementor);
    }

    @Override
    public void breathingProcess() {
        breathImplementor.breath();
    }

}
