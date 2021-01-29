package Model;

import java.util.List;

public interface Observable {

    List<Observer> getObservers();

    void addObserver(Observer o);

    void notifyObserver(Observer obs, String label, List<Object> data);


}
