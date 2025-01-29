package state;

public class TrafficLight {

    TrafficLightState currentState;

    public TrafficLight() {
        this.currentState = new RedLightState();
    }

    public void setState(TrafficLightState currentState) {
        this.currentState = currentState;
    }

    public void displayState() {
        currentState.display();
    }

    public void nextTransition() {
        currentState.next(this);
    }

}