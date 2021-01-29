import Controller.TrazAquiController;
import View.TrazAquiView;

import java.io.IOException;
import java.io.Serializable;

public class Main implements Serializable {
    public static void main (String[] args) throws InterruptedException, IOException, ClassNotFoundException {
        TrazAquiView view = new TrazAquiView();
        TrazAquiController f  = new TrazAquiController();
        f.interpretador(view);
    }
}