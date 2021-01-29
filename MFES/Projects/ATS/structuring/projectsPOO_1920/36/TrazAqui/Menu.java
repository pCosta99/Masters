
/**
 * 
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */

import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;

public class Menu {
    private List<String> opcoes;
    private int op;
    private String nomeMenu;
    
    public Menu(String[] opcoes, String nomeMenu) {
        this.opcoes = new ArrayList<String>();
        for (String op : opcoes)
            this.opcoes.add(op);
        this.op = 0;
        this.nomeMenu = nomeMenu;
    }
    
    public void executa() {
        do {
            showMenu();
            this.op = lerOpcao();
        } while (this.op == -1);
    }
    
    private void showMenu() {
        System.out.println("\n *** " + this.getNomeMenu() + " *** ");
        for (int i=0; i<this.opcoes.size(); i++) {
            System.out.print(i+1);
            System.out.print(" - ");
            System.out.println(this.opcoes.get(i));
        }
        System.out.println("0 - Sair");
    }
    
    private int lerOpcao() {
        int op; 
        Scanner is = new Scanner(System.in);
        
        System.out.print("Opção: ");
        
        // no caso de imput não ser numerico enviar mensagem de erro
        if(is.hasNextInt()) {
            op = is.nextInt();
        } else {
            op = -1;
        }

        if (op<0 || op>this.opcoes.size()) {
            System.out.println("Opção Inválida!");
            op = -1;
        }
        return op;
    }
    
    public int getOpcao() {
        return this.op;
    }
    
    public String getNomeMenu() {
        return this.nomeMenu;
    }
}
