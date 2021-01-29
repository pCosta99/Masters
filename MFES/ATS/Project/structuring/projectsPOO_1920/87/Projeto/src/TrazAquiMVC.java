import Controladores.MainController;
import Controladores.TrazAquiController;
import Exceptions.AlreadyEvaluatedException;
import Exceptions.ProdutoInexistenteException;
import Exceptions.UserInexistenteException;
import Modelos.TrazAqui;
import Modelos.TrazAquiModel;
import Readers.Parser;
import Views.LoginView;
import Views.TrazAquiView;

import java.io.IOException;

public class TrazAquiMVC {

    private static TrazAquiModel createData(){
        TrazAquiModel tam = new TrazAqui();
        Parser.parse(tam);
        return tam;
    }

    public static void main(String[] args) throws IOException, ClassNotFoundException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        TrazAquiController control = new MainController();
        TrazAquiView view = new LoginView();

        TrazAquiModel model = createData();

        control.setModel(model);
        control.setView(view);
        control.start();

    }


}
