package projeto.views;

import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

public class VoluntarioView {

	private List<String> opcoes;
	private int op;
	//private Gestao g;
	
	
	//Construtor
	public VoluntarioView(String[] opcoes){  //passar as op��es v�lidas como argumento
		this.opcoes = Arrays.asList(opcoes);
		this.op = 0;
	}
	
	
	
	

	public void executaVol() {
		do {
			mostrarMenuV();
			this.op = lerOpcao();
		} while(this.op == -1);
	}
	
	public void executaInit() {
		do {
			menuVoluntario();
			this.op = lerOpcao();
		}while(this.op == -1);
	}
	
	
	
    public void menuVoluntario() {
    	System.out.println("********* BEM VINDO *********");
		System.out.println(" -- Escolhe uma op��o para continuar --");
		System.out.println(" ---> Posso fazer entregas (1)");
		System.out.println(" ---> Ver Hist�rico (2)");
		System.out.println(" ---> Ver Classifica��o (3)");
		System.out.println(" ---> Ver Informa��es Gerais (4)");
		System.out.println(" ---> Sair (0)");
    }
    
    public static void mostrarMenuV() {
		
		Scanner sc = new Scanner(System.in);
		
		System.out.println("********* BEM VINDO *********");
		System.out.println(" -- Escolhe uma op��o para continuar --");
		System.out.println(" ---> Criar uma conta (1)");
		System.out.println(" ---> Entrar (2)");
		System.out.println(" ---> Sair (0)");
		
	}
    
	

	
	/** Ler uma op��o v�lida */
    private int lerOpcao() {
        int op = 0; 
        Scanner sc = new Scanner(System.in);
        
        System.out.print("Op��o: ");
        try {
            op = sc.nextInt();
        }
        catch (InputMismatchException e) { // N�o foi inscrito um int
            op = -1;
        }
        if (op < 0 || op > this.opcoes.size()) {
            System.out.println("Op��o Inv�lida!");
            op = -1;
        }
        
        
        
        
        return op;
        
        
    }
		
    
    public int getOpcao() {
        return this.op;
    }

}
