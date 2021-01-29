package MVC.Observer;

import java.util.ArrayList;
import java.util.List;

public abstract class Observable {
    private List<Observer> observers;

    public Observable() {
        this.observers = new ArrayList<>();
    }

    public void addObserver(Observer o) {
        this.observers.add(o);
    }

    public void notifyObservers(Object value) {
        this.observers.forEach(o -> o.update(this, value));
    }
}