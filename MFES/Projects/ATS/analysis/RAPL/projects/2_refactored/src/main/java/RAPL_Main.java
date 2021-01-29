import Exceptions.EncomendaJaExisteException;
import Perfis.ComparatorUser;
import Perfis.Utilizador;
import Utils.DataBase;
import Utils.Parser;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Comparator;

public class RAPL_Main {
    public static void main(String[] args) throws EncomendaJaExisteException {
        if(!Files.exists(Paths.get("logs.txt"))) System.err.println("Couldn't find log file!");
        DataBase db = new DataBase();
        Parser parser = new Parser();
        parser.parse(db);
        Comparator<Utilizador> c = new ComparatorUser();
        db.ordenarUsers(c);
    }
}
