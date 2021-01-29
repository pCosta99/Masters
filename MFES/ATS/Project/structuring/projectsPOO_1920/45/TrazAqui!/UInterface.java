import java.io.Serializable;
import java.util.*;

/**
 * Write a description of class UInterface here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class UInterface implements Serializable
{
    // variáveis de instância
    private List<String> opcoes;
    
    /**
     * Construtor para objetos da classe Menu.
     */
    public UInterface(String[] opcoes) {
        this.opcoes = Arrays.asList(opcoes);
    }
    
    /** 
     * Metodo que apresenta o menu.
     */
    public void showUI() {
        //System.out.println("\n **TrazAqui** ");
        for (int i=0; i < this.opcoes.size(); i++) {
            System.out.print(i+1);
            System.out.print(" - ");
            System.out.println(this.opcoes.get(i));
        }
        System.out.println("0 - Sair");
    }
    
    /** 
     * Metodo que lê o que o user escolheu.
     * 
     * @return opcao lida
     */
    public int readUI() {
        int opcao = - 1;
        Scanner input = new Scanner(System.in);
        
        System.out.print("Opcao: ");
        if(input.hasNextInt()) {
            try {
                opcao = input.nextInt();
            } catch (InputMismatchException e) { // Não foi escolhido um inteiro
                opcao = -1;
            }
            if (opcao < 0 || opcao > this.opcoes.size()) {
                System.out.println("Valor Invalido!");
                opcao = -1;
            }
        }
        return opcao;
    }
    
    public int exec(){
        showUI();
        int opcao1 = readUI();
        if(opcao1 == -1)
            exec();
        return opcao1;
    }
}
