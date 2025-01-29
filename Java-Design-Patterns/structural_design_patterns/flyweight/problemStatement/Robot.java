package flyweight.problemStatement;

public class Robot {

    int x;
    int y;
    String type;
    Sprites body; // 2d image for robot

    public Robot(int x, int y, String type, Sprites body) {
        this.x = x;
        this.y = y;
        this.type = type;
        this.body = body;
    }

}
