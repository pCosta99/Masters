package projeto.views;

import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

public class Menu {

	
	private List<String> opcoes;
	private int op;
	
	//Construtor
	public Menu(String[] opcoes){  //passar as opções válidas como argumento
		this.opcoes = Arrays.asList(opcoes);
		this.op = 0;
	}
	
	public void executa() {
		do {
			mostrarMenu();
			this.op = lerOpcao();
		} while(this.op == -1);
	}
	
	public static void mostrarMenu() {
		
		Scanner sc = new Scanner(System.in);
		
		System.out.println("************* BEM VINDO *************");
		System.out.println(" -- Escolhe uma opção para continuar --");
		System.out.println(" ---> Sou um Utilizador (1)");
		System.out.println(" ---> Sou um Voluntário (2)");
		System.out.println(" ---> Sou uma Empresa de Transporte(3)");
		System.out.println(" ---> Sou uma Loja (4)");
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

    
    
    
    
    
    /*
	 public Entidade signupGeral(Entidade ent) {
		 
		 	Scanner sc = new Scanner(System.in);
			System.out.println("Nome: ");
			String nome = sc.nextLine();
			System.out.println("Email: ");
			String mail = sc.nextLine();
			System.out.println("Password: ");
			String pass = sc.nextLine();
		 
			int f = 1;
			do {
				for(Entidade e : this.g.entidades.values()) {
					if(e.getEmail().equals(mail)) {
						f = -1;
						break;
					}
				}
					if(f == -1) {
						System.out.println("Email já em uso. Conta não foi criada");
						break;
					}
					
					else {
						f = 1;
						System.out.println("Conta criada com sucesso!");
						if(ent instanceof Utilizador) {
							ent = new Utilizador(nome,mail,pass);
							this.g.utilizadores.put(ent.getCodigo(), ent);
						}
						
						else if(ent instanceof Voluntario) {
							ent = new Voluntario(nome,mail,pass);
							this.g.voluntarios.put(ent.getCodigo(),ent);
						}
						else if(ent instanceof Empresa) {
							ent = new Empresa(nome,mail,pass);
							this.g.empresas.put(ent.getCodigo(), ent);	
						}
						else if(ent instanceof Loja) {
							ent = new Loja(nome,mail,pass);
							this.g.lojas.put(ent.getCodigo(), ent);
						}
						
						return ent;
						
					}
			} while(f == -1);
		 return ent;
	 }
	 */
	
	
	
}
