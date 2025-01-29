package memento;

public class Memento {
    private String state; // state of the originator

    public Memento(String state) {
        this.state = state; // storing state value
    }

    public String getState() {
        return state;
    }

}
