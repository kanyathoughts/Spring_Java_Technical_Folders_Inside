package memento;

import java.util.ArrayList;
import java.util.List;

public class CareTaker {
    // Care taker will keep track of all the mementos and it will add all the
    // mementos and we can get whichever memento we need from this caretaker
    List<Memento> mementoList = new ArrayList<>();

    public void addMemento(Memento memento) {
        mementoList.add(memento);
    }

    public Memento getMemento(int index) {
        return mementoList.get(index);
    }
}
