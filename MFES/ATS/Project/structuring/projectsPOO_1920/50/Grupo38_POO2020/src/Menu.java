import java.util.List;
import static java.lang.System.out;

public class Menu {
    private List<String> options;
    private int o;

    public Menu(List<String> options) {
        this.options = options;
        this.o = 0;
    }

    /** Método que faz a execução básica de um menu*/
    public void executa(){
        do{
            showMenu();
            this.o = Input.lerInt();
        } while (this.o == -1);
    }

    /** Método que mostra o menu*/
    private void showMenu(){
        for (int i=0; i<this.options.size(); i++) {
            out.print(i+1);
            out.print(" - ");
            out.println(this.options.get(i));
        }
        out.println("0) Sair\n");
    }

    public int opcao(){
        return this.o;
    }
}
