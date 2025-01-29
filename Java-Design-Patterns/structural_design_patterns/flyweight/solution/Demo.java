package flyweight.solution;

import flyweight.problemStatement.Robot;

public class Demo {

    public static void main(String[] args) {

        int x = 0;
        int y = 0;

        // Object will be created only once and will be used for the rest
        for (int i = 0; i < 1_00_00_000; i++) {
            IRobot humanoidRobot = RoboticFactory.createRobot("HUMANOID");
            humanoidRobot.display(x + i, y + i);
        }
        // Object will be created only once and will be used for the rest
        for (int i = 0; i < 1_00_00_000; i++) {
            IRobot roboticDog = RoboticFactory.createRobot("ROBOTICDOG");
            roboticDog.display(x + i, y + i);
        }

    }

}
