import Controller.Controller;
import Model.Sistema;
import View.View;
import View.ViewMain;

public class TestePrograma {

    public static void main(String[] args) {
        Sistema sistema = new Sistema();
        Controller cntrl = new Controller(sistema);
        cntrl.loadFromLog("../db/log.txt");
        //System.out.println(sistema);
        View v = new View(cntrl);
        v.runLogin();
    }

}
