package projeto.views;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

import projeto.model.Entidade;
import projeto.model.Gestao;
import projeto.model.Utilizador;

public class UtilizadorView implements Serializable{

	private List<String> opcoes;
	private int op;
	private Gestao g;
	
	
	//Construtor
	public UtilizadorView(String[] opcoes){  //passar as opções válidas como argumento
		this.opcoes = Arrays.asList(opcoes);
		this.op = 0;
		g = new Gestao();
	}
	
	public UtilizadorView(int opcoes) {
		this.op = opcoes;
	}
	
	

	public void executa() {
		do {
			mostrarMenu();
			this.op = lerOpcao();
		} while(this.op == -1);
	}
	
	
	
	public static void mostrarMenu() {
		
		Scanner sc = new Scanner(System.in);
		
		System.out.println(" ************* BEM VINDO *************");
		System.out.println(" -- Escolhe uma opção para continuar --");
		System.out.println(" ---> Criar uma conta (1)");
		System.out.println(" ---> Entrar (2)");
		System.out.println(" ---> Sair (0)");
		
	}
	
	public static void mostrarMenuInicial() {
		System.out.println("********* TRAZ AQUI! *********");
		System.out.println(" -- Escolhe uma opção para continuar -- ");
		System.out.println(" ---> Fazer um pedido (1)");
		System.out.println(" ---> Ver Historico (2)");
		System.out.println(" ---> Voltar (0)");
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
		
    
    
    // Função que imprime o nome das lojas disponíveis (associadas a um número)
    public void mostrarLojasDisponiveis() {
    	//int qt = Gestao.lojas.size();
    	int i = 1;
    	System.out.println(" --  ####  --");
    	//for(Entidade l : this.g.lojas.values()) { //TODO : levar isso pro controller
    	//	System.out.println("(" + i + ")" + l.toString());
    		//System.out.println(l.toString() + "\n");
    	//	i++;
    	//}	
    	//System.out.println("Voltar (0)");
    }
    
    // executa o menu principal das lojas
    public void executaLojas() {
		do {
			mostrarLojasDisponiveis();
			this.op = lerOpcao();
		} while(this.op == -1);
	}
    
    public void executaMenuPrincipal() {
    	do {
    		mostrarMenuInicial();
    		this.op = lerOpcao();
    	} while(this.op == -1);
    }
    
		
    
  // ----------------------------------------------------------------------------------------------\\ 
    
    public void historico() {
    	System.out.println(" --- HISTÓRICO DE ENCOMENDAS ---");
    	
    }
    
    
    
    
    
    
    
    
    
    
    
    
	public static boolean writeCsv(String filePath, String Texto) {
		try {
			File file = new File(filePath);
			FileWriter fr = new FileWriter(file, true);
			fr.write("\n" + Texto);
			fr.close();
			System.out.println("Guardado no ficheiro com sucesso!");
			return true;
		}catch(IOException e) {
			System.out.println(e.getMessage());
			return false;
		}
	}
	
		
		
	

}
