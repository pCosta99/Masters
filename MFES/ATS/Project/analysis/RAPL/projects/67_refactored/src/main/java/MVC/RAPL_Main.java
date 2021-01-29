package MVC;

import MVC.Exceptions.NaoExisteException;
import MVC.Models.Model;

import java.nio.file.Files;
import java.nio.file.Paths;

public class RAPL_Main {
    public static void main(String[] args) throws NaoExisteException {
        if(!Files.exists(Paths.get("logs.txt"))) System.err.println("Couldn't find log file!");
        Model model = new Model();
        model.carregaLog();
        model.getTop10Transportadoras();
        model.getTop10Utilizadores();
    }
}
