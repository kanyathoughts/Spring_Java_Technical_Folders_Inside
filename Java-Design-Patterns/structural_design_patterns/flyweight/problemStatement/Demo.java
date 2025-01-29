package flyweight.problemStatement;

public class Demo {

    public static void main(String[] args) {
        int x = 0;
        int y = 0;

        for (int i = 0; i < 1_00_00_000; i++) {
            Sprites humonoidSprites = new Sprites();
            Robot humanoidRobot = new Robot(x + i, y + i, "Humanoid", humonoidSprites);
        }
        for (int i = 0; i < 1_00_00_000; i++) {
            Sprites dogRobotSprites = new Sprites();
            Robot DogRobot = new Robot(x + i, y + i, "DogRobot", dogRobotSprites);
        }
    }

}
