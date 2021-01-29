import javax.swing.SwingUtilities;
import MVC.Model.Model;
import MVC.Controller.Menu.Menu;
import MVC.View.View;
import Parser.GestVendas;
import Parser.Logparser;
import MVC.Controller.Controller;
import MVC.Controller.ControllerLoja;
import MVC.Controller.ControllerTransportadora;
import MVC.Controller.ControllerUtilizador;
import MVC.Controller.ControllerVoluntario;

public class Main {
    public static void main(String[] args) {
        Model model;


        if (args.length > 0){
            if(args[0].equals("save.dat")) {
                model = GestVendas.read("save.dat");
            } else {
                Logparser logs = new Logparser();
                logs.parse(args[0]);
                model = logs.getParsed();
            }
        }
        else{
            model = new Model();
        }

        
        int option = -1;
        String[] mainOptions = { "Menu", "Utilizador", "Loja", "Voluntario", "Transportadora", "Top10EmpresasKm", "Top10Utilizadores", "Save" };
        Menu m = new Menu(mainOptions);

        while (option != 0) {
            System.out.println(m.toString());
            option = m.lerOpcao();
            final int innerop = Integer.valueOf(option);
            SwingUtilities.invokeLater(new Runnable(){
            
                @Override
                public void run() {
                    switch (innerop) {
                         case 0:
                            System.out.println("See you again!");
                            break;
                         case 1:
                            Controller controller = new ControllerUtilizador(model);
                            View viewU = new View(controller);
                            viewU.run();
                            break;
                         case 2:
                             controller = new ControllerLoja(model);
                             View viewL = new View(controller);
                             viewL.run();
                             break;
                         case 3:
                             controller = new ControllerVoluntario(model);
                             View viewV = new View(controller);
                             viewV.run();
                             break;
                         case 4:
                            controller = new ControllerTransportadora(model); 
                            View viewT = new View(controller);
                            viewT.run();
                             break;
                         case 5:
                            for (String string : model.top10Empresas()) {
                                System.out.println(string);
                            }
                            break;
                        case 6:
                            for (String string : model.top10Utilizadores()) {
                                System.out.println(string);
                            }
                            break;
                        case 7:
                            GestVendas.save("save.dat", model);
                            break;
                         default:
                            System.out.println("No Valid Input Given!");
                             break;
                    }
                }
            });
        }
    }
}