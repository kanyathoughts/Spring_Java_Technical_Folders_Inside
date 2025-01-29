package bridge.client;

import bridge.abstractor.Dog;
import bridge.abstractor.Fish;
import bridge.abstractor.LivingThings;
import bridge.implementor.AlienBreathImplementation;
import bridge.implementor.WaterBreathImplementation;

public class Demo {

    public static void main(String[] args) {
        LivingThings fishObject = new Fish(new WaterBreathImplementation());
        fishObject.breathingProcess();

        System.out.println("========================================");

        LivingThings alienObject = new Dog(new AlienBreathImplementation());
        alienObject.breathingProcess();
    }

}
