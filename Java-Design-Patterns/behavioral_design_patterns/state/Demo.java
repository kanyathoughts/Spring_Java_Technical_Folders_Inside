package state;

public class Demo {

    public static void main(String[] args) {
        TrafficLight tl = new TrafficLight();

        for (int i = 0; i < 6; i++) {
            tl.displayState();
            tl.nextTransition();
        }
    }

}