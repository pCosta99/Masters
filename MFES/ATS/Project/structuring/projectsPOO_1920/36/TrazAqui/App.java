/**
 *
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.util.*;
import java.io.*;
import java.text.SimpleDateFormat; 
import java.text.ParseException;  

public class App {

    static GestaoApp gestApp = new GestaoApp();

    public static void main(String[] args) {

        String[] opcoes = new String[] { "Parse", "Login", "Registo", "Top 10 utilizadores", "Top 10 transportadores" };
        Menu menu = new Menu(opcoes, "Menu");
        Parser parser = new Parser();
        gestApp = gestApp.readData();

        do {
            menu.executa();
            int op = menu.getOpcao();
            switch(op) {
                case 1:
                    gestApp = parser.parse();
                    gestApp.saveData();
                    break;
                case 2:
                    gestApp.login();
                    break;
                case 3:
                    gestApp.registo();
                    break;
                case 4:
                    System.out.println(gestApp.top10Utilizadores().toString());
                    break;
                case 5:
                    System.out.println(gestApp.top10Transportadores().toString());
                    break;
            }
        } while (menu.getOpcao() != 0);
    }
    
}
