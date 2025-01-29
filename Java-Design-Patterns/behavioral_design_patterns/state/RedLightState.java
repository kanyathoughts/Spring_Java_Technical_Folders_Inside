package state;

public class RedLightState implements TrafficLightState {

    @Override
    public void display() {
        System.out.println("Red Light -> Stop!");
    }

    @Override
    public void next(TrafficLight trafficLight) {
        trafficLight.setState(new GreenLightState());
    }

}
