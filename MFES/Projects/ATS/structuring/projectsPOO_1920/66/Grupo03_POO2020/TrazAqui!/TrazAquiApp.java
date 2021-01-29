import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Scanner;

/**
 * Classe principal da aplicação
 */
public class TrazAquiApp {

    /**
     * Função que inicia a aplicação
     * @param args String[]
     */
    public static void main(String[] args) {
        try {
            run();
        } catch (InterruptedException | IOException e) {
            System.out.println("Algo correu mal! Pedimos desculpa pelo inconveniente. :(");
        }

    }

    /**
     * Função que inicializa o fluxo da aplicação
     * @throws InterruptedException Exceção
     * @throws IOException Exceção
     */
    public static void run() throws InterruptedException, IOException {
        Registo r = new Registo();
        Logs l = new Logs();
        try {
            r = Registo.carregaEstado("Registo.dat");
        }
        catch (IOException | ClassNotFoundException e){
            File f = new File("Registo.dat");
            System.out.println("Ficheiro de registos criado!\n");
        }
        l.parse(r);
        System.out.println("Ficheiro Logs.txt lido com sucesso!\n");
        Thread.sleep(1000);
        Scanner s = new Scanner(System.in);
        Controller controller = new Controller(r);
        View view = new View(s, controller);
        View.clearScreen();
        view.run();
        r = controller.getRegisto();
        r.guardaEstado("Registo.dat");
        System.out.println("Ficheiro Registo.dat atualizado com sucesso!\n");
    }
}
