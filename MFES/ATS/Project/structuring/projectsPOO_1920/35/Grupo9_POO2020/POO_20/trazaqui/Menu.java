import java.util.ArrayList;
import java.util.List;
import java.util.InputMismatchException;
import java.util.Scanner;
import java.io.Serializable;

public class Menu{
    
    private String titulo;
    private List<String> opcoes;
    private int op;    
    
    /**
     * Construtor para objetos da classe Menu
     */
    public Menu(String[] opcoes){
        this.opcoes = new ArrayList<String>();
        for(String op: opcoes)
            this.opcoes.add(op);
        this.op = 0;
        this.titulo="Menu";
    }

    
    /**
     * Construtor para objetos da classe Menu
     */
    public Menu(String titulo, String[] opcoes){
        this.opcoes = new ArrayList<String>();
        for(String op: opcoes)
            this.opcoes.add(op);
        this.op = 0;
        this.titulo=titulo;
    }
    
    /**
     * Função para executar o menu.
     */
    public void executa(){
        do {
            showMenu();
            this.op = lerOpcao();
        }
        while(this.op == -1);
    }
    
    /**
     * Função para mostrar o menu.
     */
    private void showMenu() {
        System.out.println("\n*******************"+ this.titulo +"*******************");
        System.out.println("Escreva o número de uma das seguintes opções: ");
        for (int i=0; i<this.opcoes.size(); i++) {
            System.out.print("   "+(i+1));
            System.out.print(" - ");
            System.out.println(this.opcoes.get(i));
        }
        System.out.println("   0 - Sair");
        System.out.println("*********************************************");
    }
    
    /**
     * Função ler uma opção do menu.
     */
    private int lerOpcao() {
        int op; 
        Scanner is = new Scanner(System.in);
        
        System.out.print("Opção: ");
        try {
            op = is.nextInt();  
            //Para limpar a consola;
            System.out.print ('\f');
        }
        catch (InputMismatchException e) { // Não foi inscrito um int
            op = -1;
        }
        if (op<0 || op>this.opcoes.size()) {
            System.out.println("Opção Inválida!");
            op = -1;
        }
        return op;
    }
    
    /**
     * Obter opção selecionada.
     * @return 
     */
    public int getOpcao() {
        return this.op;
    }
}