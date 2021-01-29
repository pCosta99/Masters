package MVC;

import MVC.Controller.Controller;
import MVC.Models.Model;
import MVC.Views.MessageView;

public class SistemaApp {
    public static void main(String[] args){
        IView view;
        Model model = new Model();
        if(model==null){
            view = new MessageView();
            view.show("Error loading default model");
            System.exit(-1);
        }
        Controller cont = new Controller();
        cont.setModel(model);
        cont.start();
        System.exit(0);
    }
}
