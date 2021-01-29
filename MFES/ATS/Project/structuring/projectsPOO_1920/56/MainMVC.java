import Controller.*;
import Model.*;
import Controller.IUserController;
import Controller.UserController;
import View.*;

public class MainMVC {

    public static void main(String[] args) {

        IAppView view = new AppView();
        ISistema sistema = new Sistema();
        IAppController controller = new AppController();

        IUserController userController = new UserController();

        ILojaController lojaController = new LojaController();

        IVoluntarioController volController = new VoluntarioController();

        IEmpresaController empresaController = new EmpresaController();

        controller.setSistema(sistema);
        controller.setAppView(view);

        sistema = controller.runController();

        controller.setSistema(sistema);
        controller.setAppView(view);

        userController.setSistema(sistema);
        userController.setAppView(view);

        lojaController.setSistema(sistema);
        lojaController.setView(view);

        volController.setSistema(sistema);
        volController.setAppView(view);

        empresaController.setSistema(sistema);
        empresaController.setView(view);

        int res=0;
        char login = controller.signUp();
        while(login != '0') {
            if (login == 'u') {
                System.out.println(sistema.toString());
                res = userController.userMode();
                //if(res==1) login = 'l';
            }
            if (login == 'l') {
                System.out.println(sistema.toString());
                lojaController.lojaMode();
            }
            if(login == 'v'){
                System.out.println(sistema.toString());
                volController.VoluntarioMode();
            }
            if(login == 't'){
                System.out.println(sistema.toString());
                empresaController.mode();
            }
            login = controller.signUp();
        }
    }
}