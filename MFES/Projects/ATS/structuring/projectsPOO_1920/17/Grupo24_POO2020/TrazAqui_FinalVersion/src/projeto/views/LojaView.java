package projeto.views;

import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

import projeto.model.Entidade;
import projeto.model.Gestao;


public class LojaView {

	private List<String> opcoes;
	private int op;
	private Gestao g;
	
	
	//Construtor
	public LojaView(String[] opcoes){  //passar as op��es v�lidas como argumento
		this.opcoes = Arrays.asList(opcoes);
		this.op = 0;
	}
	
	public LojaView() {
		this.op = 0;
	}
	
	
	

	public void executaInitLojas() {
		do {
			mostrarMenuLojas();
			this.op = lerOpcao();
		} while(this.op == -1);
	}
	
	public void executaL() {
		do {
			mostrarMenuL();
			this.op = lerOpcao();
		}while(this.op == -1);
	}
	
	
	
    public void mostrarMenuLojas() {
    	System.out.println("********* BEM VINDO *********");
		System.out.println(" -- Escolhe uma op��o para continuar --");
		System.out.println(" ---> Sinalizar (1)");
		System.out.println(" ---> Ver Hist�rico (2)");
		System.out.println(" ---> Informa��es gerais (3) ");
		System.out.println(" ---> Mudar Tempo M�dio de Fila (4)");
		
    }
    
    public static void mostrarMenuL() {
		System.out.println("********* BEM VINDO *********");
		System.out.println(" -- Escolhe uma op��o para continuar --");
		System.out.println(" ---> Criar uma conta (1)");
		System.out.println(" ---> Entrar (2)");
		System.out.println(" ---> Sair (0)");
		
	}
    
	

	
	/** Ler uma op��o v�lida */
    private int lerOpcao() {
        int op; 
        Scanner sc = new Scanner(System.in);
        
        System.out.print("Op��o: ");
        try {
            op = sc.nextInt();
        }
        catch (InputMismatchException e) { // N�o foi inscrito um int
            op = -1;
        }
        if (op < 0 || op > this.opcoes.size()+2) {
            System.out.println("Op��o Inv�lida!");
            op = -1;
        }
        
        
        //sc.close();
        
        return op;
        
        
    }
		
    
    public int getOpcao() {
        return this.op;
    }
    
  // ----------------------------------------------------------------------------------------------\\ 
    
    /* Entrar e cadastrar */
    
    
    
    
    
  // ----------------------------------------------------------------------------------------------\\ 
	

}
