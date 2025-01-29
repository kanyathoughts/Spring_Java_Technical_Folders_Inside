package memento;

public class Demo {
    public static void main(String[] args) {
        Originator originator = new Originator();
        CareTaker careTaker = new CareTaker();

        originator.setState("State 1");
        careTaker.addMemento(originator.setStateToMemento());

        originator.setState("State 2");
        careTaker.addMemento(originator.setStateToMemento());

        originator.setState("State 3");
        careTaker.addMemento(originator.setStateToMemento());

        originator.getStateFromMemento(careTaker.getMemento(0));
        originator.getStateFromMemento(careTaker.getMemento(1));

        System.out.println("Final state is: " + originator.getState());

    }
}
