import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import static java.lang.System.out;

/**
 * Classe que auxilia a interação com o utilizador.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class Menu implements Serializable {
    private final List<String> opcoes;
    private int op;

    /**
     * Construtor do Menu.
     * @param opcoes, Opçoes do Menu a construir.
     */
    public Menu(String[] opcoes){
        this.opcoes = Arrays.asList(opcoes);
        this.op = 0;
    }

    /**
     * Função que executa uma dada opção escolhida pelo utilizador.
     */
    public void executa(){
        this.showMenu();
        do this.op = Input.lerInt("Opção: ","Opção inválida!");
        while(this.op < 0 || this.op > this.opcoes.size());
    }

    /**
     * Função que executa uma dada opção escolhida pelo utilizador.
     */
    public void executa(String saida){
        this.showMenu(saida);
        do this.op = Input.lerInt("Opção: ","Opção inválida!");
        while(this.op < 0 || this.op > this.opcoes.size());
    }

    /**
     * Função que apresenta um conjunto de opções disponíveis.
     */
    public void showMenu(){
        for(int i = 0; i < this.opcoes.size(); i++)
            out.printf("%d - %s.\n",i+1,this.opcoes.get(i));
        out.println("0 - Sair.");
    }

    /**
     * Função que apresenta um conjunto de opções disponíveis, sendo que a opção 0 é a opção com a String fornecida.
     * @param saida, String que será a opção 0.
     */
    public void showMenu(String saida){
        for(int i = 0; i < this.opcoes.size(); i++)
            out.printf("%d - %s.\n",i+1,this.opcoes.get(i));
        out.println("0 - " + saida);
    }

    /**
     * Função que retorna a opção escolhida.
     * @return Opção escolhida.
     */
    public int getOption(){
        return this.op;
    }
}
