import java.io.IOException;
import Controller.Controller;
import Model.TrazAqui;
import Exceptions.*;

public class TestaAplicacao{

    public static void main(String[] args) {


        TrazAqui model = new TrazAqui();

        try {
            model = model.read("Guardado.CSV");
        }
        catch (IOException | ClassNotFoundException e) {
            System.out.println("Vou carregar a partir de txt!");
            try{
                model.fazParse("LOG1.txt");
            }
            catch(UserJaExisteException | JaExisteLojaException |JaExisteVoluntarioException | JaExisteEmpresaException | JaExisteEncomendaException | JaExisteEncomendaAceiteException u){
                System.out.println("Algo correu mal");
            }
        }

        new Controller(model).run();
        try {
            model.save("Guardado.CSV");
        }
        catch (IOException eio) {
            System.out.println("Erro a guardar!");
        }


        System.out.println("Aplicacao Fechada");
    }
    
}