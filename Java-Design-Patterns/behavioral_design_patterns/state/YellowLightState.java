package state;

public class YellowLightState implements TrafficLightState {

    @Override
    public void display() {
        System.out.println("Yellow Light -> Prepare to stop");
    }

    @Override
    public void next(TrafficLight trafficLight) {
        trafficLight.setState(new RedLightState());
    }

}
