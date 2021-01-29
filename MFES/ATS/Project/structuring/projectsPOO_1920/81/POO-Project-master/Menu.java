import java.util.*;
/**
 * Escreva a descrição da classe Menu aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Menu {
    // variáveis de instância
    private List<String> opcoes;
    private int op;

    /**
     * Construtor para objetos da classe Menu.
     */
    public Menu(String[] opcoes) {
        this.opcoes = Arrays.asList(opcoes);
        this.op = 0;
    }

    /**
     * Metodo que apresenta o menu.
     */
    private void mostraMenu() {
        System.out.println("\n *** TRAZAQUI! *** ");
        for (int i=0; i < this.opcoes.size(); i++) {
            System.out.print(i+1);
            System.out.print(" - ");
            System.out.println(this.opcoes.get(i));
        }
        System.out.println("0 - Sair");
    }

    /**
     * Metodo que le uma opçao do menu.
     *
     * @return opcao lida
     */
    public int leMenu() {
        int opcao;
        Scanner in = new Scanner(System.in);

        System.out.print("Opção: ");
        try {
            opcao = in.nextInt();
        }
        catch (InputMismatchException e) { // Não foi inscrito um inteiro
            opcao = -1;
        }
        if (opcao < 0 || opcao > this.opcoes.size()) {
            System.out.println("Opção Inválida!");
            opcao = -1;
        }
        return opcao;
    }

    /**
     * Método para apresentar o menu e ler uma opção.
     *
     * @return opcao escolhida
     */
    public int executa() {
        mostraMenu();
        int opcao = leMenu();
        if(opcao == -1)
            executa();
        return opcao;
    }

}
