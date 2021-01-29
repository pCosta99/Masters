package API;

import java.util.ArrayList;
import java.util.List;

public class Clock {
    List<Integer> putsDone;

    public Clock() {
        this.putsDone = new ArrayList<>();
    }

    void setDone(Integer id) {
        this.putsDone.add(id);
    }

    public boolean isDone(Integer id) {
        return this.putsDone.contains(id);
    }
}
