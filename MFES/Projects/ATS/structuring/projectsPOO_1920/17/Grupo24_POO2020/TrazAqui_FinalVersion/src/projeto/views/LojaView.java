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
	public LojaView(String[] opcoes){  //passar as opções válidas como argumento
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
		System.out.println(" -- Escolhe uma opção para continuar --");
		System.out.println(" ---> Sinalizar (1)");
		System.out.println(" ---> Ver Histórico (2)");
		System.out.println(" ---> Informações gerais (3) ");
		System.out.println(" ---> Mudar Tempo Médio de Fila (4)");
		
    }
    
    public static void mostrarMenuL() {
		System.out.println("********* BEM VINDO *********");
		System.out.println(" -- Escolhe uma opção para continuar --");
		System.out.println(" ---> Criar uma conta (1)");
		System.out.println(" ---> Entrar (2)");
		System.out.println(" ---> Sair (0)");
		
	}
    
	

	
	/** Ler uma opção válida */
    private int lerOpcao() {
        int op; 
        Scanner sc = new Scanner(System.in);
        
        System.out.print("Opção: ");
        try {
            op = sc.nextInt();
        }
        catch (InputMismatchException e) { // Não foi inscrito um int
            op = -1;
        }
        if (op < 0 || op > this.opcoes.size()+2) {
            System.out.println("Opção Inválida!");
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
