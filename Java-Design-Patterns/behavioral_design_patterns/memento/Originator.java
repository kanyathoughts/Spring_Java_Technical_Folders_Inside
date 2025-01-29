package memento;

public class Originator {
    private String state;

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
        System.out.println("Current state is: " + state);
    }

    // Here we are creating new memento and setting up the state value in it.
    public Memento setStateToMemento() {
        return new Memento(state);
    }

    // We are getting state value from memento and setting up the value to the state
    // instance variable
    public void getStateFromMemento(Memento memento) {
        this.state = memento.getState();
        System.out.println("State from memento is: " + state);
    }

}
