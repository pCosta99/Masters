package projeto.views;

import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

import projeto.model.Entidade;
import projeto.model.Gestao;

public class EmpresaView {

	private List<String> opcoes;
	private int op;
	//private Gestao g;
	
	
	//Construtor
	public EmpresaView(String[] opcoes){  //passar as opções válidas como argumento
		this.opcoes = Arrays.asList(opcoes);
		this.op = 0;
	}
	
	
	
	

	public void executaEmp() {
		do {
			mostrarMenuE();
			this.op = lerOpcao();
		} while(this.op == -1);
	}
	
	public void executaInit() {
		do {
			menuTransportadora();
			this.op = lerOpcao();
		}while(this.op == -1);
	}
	
	
	
    public void menuTransportadora() {
    	System.out.println("********* BEM VINDO *********");
		System.out.println(" -- Escolhe uma opção para continuar --");
		System.out.println(" ---> Posso fazer entregas (1)");
		System.out.println(" ---> Ver Histórico (2)");
		System.out.println(" ---> Ver Classificação (3)");
		System.out.println(" ---> Ver Informações Gerais --> Faturamento, kms percorridos(4)");
		System.out.println(" ---> TOP 10 Transportadoras (5)");
		System.out.println(" ---> Sair (0)");
    }
    
    public static void mostrarMenuE() {
		System.out.println("********* BEM VINDO *********");
		System.out.println(" -- Escolhe uma opção para continuar --");
		System.out.println(" ---> Criar uma conta (1)");
		System.out.println(" ---> Entrar (2)");
		System.out.println(" ---> Sair (0)");
		
	}
    
	

	
	/** Ler uma opção válida */
    private int lerOpcao() {
        int op = 0; 
        Scanner sc = new Scanner(System.in);
        
        System.out.print("Opção: ");
        try {
            op = sc.nextInt();
        }
        catch (InputMismatchException e) { // Não foi inscrito um int
            op = -1;
        }
        if (op < 0 || op > this.opcoes.size()) {
            System.out.println("Opção Inválida!");
            op = -1;
        }
        
        
        
        
        return op;
        
        
    }
		
    
    public int getOpcao() {
        return this.op;
    }

}
