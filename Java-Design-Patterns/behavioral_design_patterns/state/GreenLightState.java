package state;

public class GreenLightState implements TrafficLightState {

    @Override
    public void display() {
        System.out.println("Green Light -> you can go");
    }

    @Override
    public void next(TrafficLight trafficLight) {
        trafficLight.setState(new YellowLightState());
    }

}
