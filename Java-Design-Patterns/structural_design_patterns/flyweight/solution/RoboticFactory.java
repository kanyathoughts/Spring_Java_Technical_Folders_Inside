package flyweight.solution;

import java.util.HashMap;
import java.util.Map;

public class RoboticFactory {

    public static Map<String, IRobot> robotcache = new HashMap<>();

    public static IRobot createRobot(String type) {
        if (robotcache.containsKey(type)) {
            return robotcache.get(type);
        } else {
            if (type.equals("HUMANOID")) {
                Sprites humanoidSprites = new Sprites();
                IRobot humanoidRobot = new HumanoidRobot(type, humanoidSprites);
                robotcache.put(type, humanoidRobot);
                return humanoidRobot;
            } else if (type.equals("ROBOTICDOG")) {
                Sprites roboticDogSprites = new Sprites();
                IRobot roboticDog = new RoboticDog(type, roboticDogSprites);
                robotcache.put(type, roboticDog);
                return roboticDog;
            }
        }
        return null;
    }

}
