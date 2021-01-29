package Model;

import java.util.List;

public interface Observer {

    void update(Observable obs, String label, List<Object> data);

}
