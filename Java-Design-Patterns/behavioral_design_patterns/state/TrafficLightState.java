package state;

public interface TrafficLightState {
    public void display();

    public void next(TrafficLight trafficLight);

}