package projeto.controlador;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.stream.Collectors;

import projeto.model.Empresa;
import projeto.model.Encomenda;
import projeto.model.Entidade;
import projeto.model.EntidadeTransportadora;
import projeto.model.Loja;
import projeto.model.Utilizador;
import projeto.model.Voluntario;
import projeto.views.EmpresaView;
import projeto.views.LojaView;
import projeto.views.Menu;
import projeto.views.UtilizadorView;
import projeto.views.VoluntarioView;
import projeto.model.Gestao;
import projeto.model.LinhaEncomenda;

public class Controlador {

	//private String filepath = "C:\\Users\\Cliente\\eclipse-workspace\\TrazAqui\\src\\teste.txt";
	private Gestao g;
	private Utilizador u;
	private Empresa em;
	private Voluntario v;
	private Loja l;
	private UtilizadorView menuU;
	private UtilizadorView menuInicial;
	private UtilizadorView menuLojas;
	private EmpresaView menuInicialE;
	private EmpresaView menuE;
	private VoluntarioView menuInicialV;
	private VoluntarioView menuV;
	private LojaView menuInicialL;
	private LojaView menuL;
	private Menu inicio;
	
	
	
	public static void main(String[] args) {
		
	
		new Controlador().init();
		
	}
	
	

	/**
     * Construtor
     * 
     */
	
	Controlador() {
        // Criar o menu 
		//System.out.println(this.g.getUtilizadores());
        String[] opcoes = {"Criar", "Entrar", "Sair"};
        
        String[] opcoes2 = {"Fazer pedido","Ver histórico", "Voltar"}; //utilizador
        // TODO: ver TOP utilizadores com + encomendas feitas
        String[] opcoes3 = {"Utilizador", "Voluntario",  "Empresa", "Loja"};

        String[] opcoes4 = {" "," ", " ", " ", " ", " ", " ", " ", " "}; //lojas disponíveis
        
        String[] opcoes5 = {"Posso receber pedidos", "Ver histórico", "Ver classificação", "Ver informações", "TOP 10"}; //empresa
        
        String[] opcoes6 = {"Posso receber pedidos", "Ver histórico", "Ver classificação", "Ver informações"};
        
        String[] opcoesLoja = {"Sinalizar", "Histórico","Info", "Tempo Medio Fila"};
        
        this.menuU = new UtilizadorView(opcoes);
        this.menuInicial = new UtilizadorView(opcoes2);
        this.inicio = new Menu(opcoes3);
        this.menuLojas = new UtilizadorView(opcoes4);
        this.menuE = new EmpresaView(opcoes);
        this.menuInicialE = new EmpresaView(opcoes5);
        this.menuInicialV = new VoluntarioView(opcoes6);
        this.menuV = new VoluntarioView(opcoes);
        this.menuL = new LojaView(opcoes);
        this.menuInicialL = new LojaView(opcoesLoja);
    
        
        try {
            this.g = Gestao.carregaEstado("estado.obj");
            System.out.println("sucesso");
            //System.out.println(this.g.entregadores.get("t51"));
            //Entidade teste = this.g.entregadores.get("t51");
            //teste.setPassword("teste");
            //System.out.println(this.g.empresas);
          
            
        }
        catch (FileNotFoundException e) {
            System.out.println("Primeira utilização");  
            this.g = new Gestao();
      
        }
        catch (IOException e) {
            System.out.println("Erro de leitura!");     
            this.g = new Gestao();
        }
        catch (ClassNotFoundException e) {
            System.out.println("Formato de ficheiro de dados errado!");
            this.g = new Gestao();
        }
        
    }
	

	
	
	public void init() {
		do {
			inicio.executa();
            switch (inicio.getOpcao()) {
                case 1: System.out.println("Utilizador");
                		userRun();
                        break;
                case 2: System.out.println("Voluntário");
                		voluntarioRun();
                		break;
                case 3: System.out.println("Empresa");
                		empresaRun();
                		break;
                case 4: System.out.println("Loja");
                		lojaRun();
                		break;
                case 5: System.out.println("Sair");
                		break;
                		
            }
            
        } while (inicio.getOpcao()!=0); 
        
        try {
			this.g.guardaEstado("estado.obj");
			//System.out.println("Sucesso");
		} catch (FileNotFoundException e) {	
			System.out.println("Erro.");
			e.printStackTrace();
		} catch (IOException e) {
			System.out.println("Erro.");
			e.printStackTrace();
		}
        System.out.println("Saindo...");  
	}
	
	
	
	
	public void pagInitTransportadora(Empresa emp) {
		
		do {
			menuInicialE.executaInit();
			switch(menuInicialE.getOpcao()) {
				case 1: String r = estouLivre();
						if(r.equals("s")) {
							emp.setLivre(true);
						}
						else{
							emp.setLivre(false);
						}
						break;
				case 2: System.out.println("Ver histórico de entregas");
        				System.out.println("Entregas efetuadas: " + emp.getEntregasEfetuadas());
        				//System.out.println("Entregas efetuadas: " + encomendasFiltro(emp));
        				//System.out.println("Histórico de entregas: " + user.verHistoricoEntregar());
						break;
				case 3: System.out.println("Classificação");
				        System.out.println(emp.getClassificacao());
						break;
				case 4: System.out.println("Info");
						System.out.println("Faturamento: " + emp.getFaturamento());
						System.out.println("Kms percorridos: " + emp.getKmsPercorridos());
						System.out.println("NIF: " + emp.getNIF());
				        break;
				case 5: System.out.println("TOP 10 por kms percorridos");
						//System.out.println(this.g.empresas);
						System.out.println(this.g.ordenaEmpresa());
				        break;
    		
		}
			
			
			
		}while(menuInicialE.getOpcao()!=0);
		
		
	}
	
	private void pagInit(Utilizador user) {
        do {
            menuInicial.executaMenuPrincipal();
            switch (menuInicial.getOpcao()) {
                case 1: System.out.println("Fazer um pedido à uma loja");
                		todasLojas(user);
                        break;
                case 2: System.out.println("Ver histórico de entregas");
                		System.out.println("Quantidade de encomendas: " + user.getNumEncomendasEfetuadas());
                		System.out.println("Histórico de entregas: " + user.verHistoricoEntregar());
                		break;
                case 3: System.out.println("Sair (0)");
                		break;
                		
                		
            }
            
        } while (menuInicial.getOpcao()!=0);
        
	}
	
	private void pagInitLoja(Loja loja) {
        do {
            menuInicialL.executaInitLojas();
            switch (menuInicialL.getOpcao()) {
                case 1: System.out.println("Sinalização");
                        break;
                case 2: System.out.println("Histórico");
                		System.out.println(loja.getEncomendasEfetuadas());
                		break;
                case 3: System.out.println("Info");
                		System.out.println("Tempo médio de fila atual: " + loja.getTempoMediaFilaMin());
                		System.out.println("Código: " + loja.getCodigo());
                		System.out.println("Email: " + loja.getEmail());
                		System.out.println("Senha: " + loja.getPassword());
                        break;
                case 4: System.out.println("Tempo médio de fila atual: " + loja.getTempoMediaFilaMin());
                		double tempo = tempoFila();
         
                		loja.setTempoMediaFilaMin((int) tempo);
                		break;
                		
                		
            }
            
        } while (menuInicialL.getOpcao()!=0);
        
	}
	
	
	
	private void pagInitVoluntario(Voluntario vol) {
        do {
            menuInicialV.executaInit();
            switch (menuInicialV.getOpcao()) {
                case 1: String r = estouLivre();
						if(r.equals("s")) {
							vol.setLivre(true);
						}
						else{
							vol.setLivre(false);
						}
                        break;
                case 2: System.out.println("Histórico");
                		System.out.println("Entregas efetuadas: " + vol.getEntregasEfetuadas());
                		break;
                case 3: System.out.println("Classificação");
                		System.out.println(vol.getClassificacao());
                		break;
                case 4: System.out.println("Info");
                        System.out.println("Nada por aqui");
                        break;     		
            }
            
        } while (menuInicialV.getOpcao()!=0);
        
	}
	
		
	
	public void lojaRun() {
		do {
			menuL.executaL();
			switch(menuL.getOpcao()) {
				case 1: System.out.println("Criar conta");
						Loja loja = signupLoja();
						if(loja != null) {
							pagInitLoja(loja);
						}
						break;
				case 2: System.out.println("Entrar");
						Loja loja2 = signinLoja();
						if(loja2 != null) {
							pagInitLoja(loja2);
						}
						break;
				
			}
		}while(menuL.getOpcao() != 0);
	}
	
	public void empresaRun() {
		do {
			menuE.executaEmp();
			switch(menuE.getOpcao()) {
				case 1: System.out.println("Criar conta");
						Empresa emp = signupTransportadora();
						if(emp != null) {
							pagInitTransportadora(emp);
						}
						break;
				case 2: System.out.println("Entrar");
						Empresa emp2 = signinTransportadora();
						if(emp2 != null) {
							pagInitTransportadora(emp2);
						}
						break;
				
			}
		}while(menuE.getOpcao() != 0);
	}
	
	public void voluntarioRun() {
		do {
			menuV.executaVol();
			switch(menuV.getOpcao()) {
				case 1: System.out.println("Criar conta");
						Voluntario vol = signupVoluntario();
						if(vol != null) {
							pagInitVoluntario(vol);
						}
						break;
				case 2: System.out.println("Entrar");
						Voluntario vol2 = signinVoluntario();
						if(vol2 != null) {
							pagInitVoluntario(vol2);
						}
						break;
				
			}
		}while(menuV.getOpcao() != 0);
	}
	
	
	
	
	public void userRun() {
        do {
            menuU.executa();
            switch (menuU.getOpcao()) {
                case 1: System.out.println("Criar conta");
                		Utilizador user = signup();		
                		if(user != null) {
                			pagInit(user);
                		}
                        break;
                case 2: System.out.println("Entrar numa conta já existente");
                		Utilizador user2 = signin(); 
                		if(user2 != null) {
                			pagInit(user2);
                		}
                		
                		break;
                //case 3: System.out.println("Ok!");
                		
            }
            
        } while (menuU.getOpcao()!=0); // A opção 0 é usada para sair do menu.
        
  
  
    }
	
	
	
	 
	 
	 
	 
	 
	 public Empresa signupTransportadora() {
	    	Scanner sc = new Scanner(System.in);
	    	System.out.println("Nome: ");
			String nome = sc.nextLine();
			System.out.println("Email: ");
			String mail = sc.nextLine();
			System.out.println("Password: ");
			String pass = sc.nextLine();
			
			int f = 1;
			do {
				for(Entidade u : this.g.empresas.values()) {
					if(u.getEmail().equals(mail)) {
						f = -1;
						break;
					}
				}
		
				if(f == -1) {
					System.out.println("Email já em uso. Conta não foi criada");
					break;
				}
				
				else {
					System.out.println("Conta criada com sucesso!");
					this.em = new Empresa(nome,mail,pass);
					this.g.empresas.put(em.getCodigo(), em);
					f = 1;
				
					return em;
					
				}
			}while(f == -1);

			return null;
	  }
	 
	 
	 public Empresa signinTransportadora() {
	    	Scanner sc = new Scanner(System.in);
	    	System.out.println("Email: ");
			String mail = sc.nextLine();
			System.out.println("Password: ");
			String pass = sc.nextLine();
			
			int f = -1;
			for(Entidade u : this.g.empresas.values()) {
				if(mail.equals(u.getEmail()) && pass.equals(u.getPassword())) {
					System.out.println(u.toString());
					f = 1;
					System.out.println("Estás logado!");
					return (Empresa) u;
				}
			}
			
			if( f == -1 ) {
				System.out.println("Email ou senha inválidos.");
				return null;
			}
			
			return null;
			
	    }
	 
	 
	 public Voluntario signupVoluntario() {
	    	Scanner sc = new Scanner(System.in);
	    	System.out.println("Nome: ");
			String nome = sc.nextLine();
			System.out.println("Email: ");
			String mail = sc.nextLine();
			System.out.println("Password: ");
			String pass = sc.nextLine();
			
			int f = 1;
			do {
				for(Entidade u : this.g.voluntarios.values()) {
					if(u.getEmail().equals(mail)) {
						f = -1;
						break;
					}
				}
		
				if(f == -1) {
					System.out.println("Email já em uso. Conta não foi criada");
					break;
				}
				
				else {
					System.out.println("Conta criada com sucesso!");
					this.v = new Voluntario(nome,mail,pass);
					this.g.voluntarios.put(v.getCodigo(), v);
					f = 1;
				
					return v;
					
				}
			}while(f == -1);

			return null;
	  }
	 
	 public Voluntario signinVoluntario() {
	    	Scanner sc = new Scanner(System.in);
	    	System.out.println("Email: ");
			String mail = sc.nextLine();
			System.out.println("Password: ");
			String pass = sc.nextLine();
			
			int f = -1;
			for(Entidade u : this.g.voluntarios.values()) {
				if(mail.equals(u.getEmail()) && pass.equals(u.getPassword())) {
					System.out.println(u.toString());
					f = 1;
					System.out.println("Estás ligado!");
					return (Voluntario) u;
				}
			}
			
			if( f == -1 ) {
				System.out.println("Email ou senha inválidos.");
				return null;
			}
			
			return null;
			
	    }
	 
	 
	 
	 public Loja signupLoja() {
	    	Scanner sc = new Scanner(System.in);
	    	System.out.println("Nome: ");
			String nome = sc.nextLine();
			System.out.println("Email: ");
			String mail = sc.nextLine();
			System.out.println("Password: ");
			String pass = sc.nextLine();
			
			int f = 1;
			do {
				for(Entidade u : this.g.lojas.values()) {
					if(u.getEmail().equals(mail)) {
						f = -1;
						break;
					}
				}
		
				if(f == -1) {
					System.out.println("Email já em uso. Conta não foi criada");
					break;
				}
				
				else {
					System.out.println("Conta criada com sucesso!");
					this.l = new Loja(nome,mail,pass);
					this.g.lojas.put(l.getCodigo(), l);
					f = 1;
				
					return l;
					
				}
			}while(f == -1);

			return null;
	  }
	 
	 
	 public Loja signinLoja() {
	    	Scanner sc = new Scanner(System.in);
	    	System.out.println("Email: ");
			String mail = sc.nextLine();
			System.out.println("Password: ");
			String pass = sc.nextLine();
			
			int f = -1;
			for(Entidade u : this.g.lojas.values()) {
				if(mail.equals(u.getEmail()) && pass.equals(u.getPassword())) {
					System.out.println(u.toString());
					f = 1;
					System.out.println("Estás logado!");
					return (Loja) u;
				}
			}
			
			if( f == -1 ) {
				System.out.println("Email ou senha inválidos.");
				return null;
			}
			
			return null;
			
	    }
	 
	 
	 public Utilizador signup() {
	    	Scanner sc = new Scanner(System.in);
	    	System.out.println("Nome: ");
			String nome = sc.nextLine();
			System.out.println("Email: ");
			String mail = sc.nextLine();
			System.out.println("Password: ");
			String pass = sc.nextLine();
			
			int f = 1;
			do {
				for(Entidade u : this.g.utilizadores.values()) {
					if(u.getEmail().equals(mail)) {
						f = -1;
						break;
					}
				}
		
				if(f == -1) {
					System.out.println("Email já em uso. Conta não foi criada");
					break;
				}
				
				else {
					System.out.println("Conta criada com sucesso!");
					u = new Utilizador(nome,mail,pass);
					this.g.utilizadores.put(u.getCodigo(), u);
					f = 1;
				
					return u;
					
				}
			}while(f == -1);

			return null;
	  }
	 
	 
	 
	 public Utilizador signin() {
	    	Scanner sc = new Scanner(System.in);
	    	System.out.println("Email: ");
			String mail = sc.nextLine();
			System.out.println("Password: ");
			String pass = sc.nextLine();
			
			int f = -1;
			for(Entidade u : this.g.utilizadores.values()) {
				if(mail.equals(u.getEmail()) && pass.equals(u.getPassword())) {
					System.out.println(u.toString());
					f = 1;
					System.out.println("Estás logado!");
					return (Utilizador) u;
				}
			}
			
			if( f == -1 ) {
				System.out.println("Email ou senha inválidos.");
				return null;
			}
			
			return null;
			
	    }
	 
	 
	 
	 
	 

	 private void todasLojas(Utilizador user) {
	    	boolean flag = false;
	    	Object[] lojas = this.g.lojas.values().toArray();
	    	do {
	    		try {
	    			mostrarLojasDisponiveis();
	    			menuLojas.executaLojas();
	                Loja loja = (Loja) lojas[menuLojas.getOpcao() - 1];
	                System.out.println("Escolheu: " + loja);
	                do {
	                	
	                	Encomenda enc = criaEncomenda(loja, user);  
	                	String r = eEncomendaMedica();
	                	EntidadeTransportadora selecionado;
	                	if(r.equals("s")) {
	                		selecionado = buscarPorEntregadorMedico(enc,loja);
	                	}
	                	else {
	                		selecionado = buscarPorEntregador(enc,loja);
	                	}
	                    // se o entregador mais perto for um voluntário, já é automaticamente aceite.
	                    //System.out.println(selecionado);
	            		if(selecionado instanceof Voluntario) {
	            			//System.out.println("É voluntario");
	            			this.g.aceites.put(enc.getCodigo(), enc);
	            			//System.out.println(Gestao.aceites.values());
	            			if(enc != null) {
	            				enc.setEntregador(selecionado.getCodigo());
	            				enc.setLoja(loja.getCodigo());
	            				System.out.println("Encomenda já tem entregador: " + selecionado);
	            				flag = false;
	            				double classificacao = classificacao();
    	            			user.classificar(classificacao, selecionado);
    	            			ArrayList<String> h = selecionado.getEntregasEfetuadas();     	            	    	
    	            	    	h.add(enc.toString());
    	            	    	selecionado.setEntregasEfetuadas(h);
    	            	    	ArrayList<String> encs = loja.getEncomendasEfetuadas();
    	            	    	encs.add(enc.toString());
    	            	    	loja.setEncomendasEfetuadas(encs);
	            				break;
	            			}
	            		}
	            		else {
	            			//System.out.println("Não é voluntário");
	            			if(selecionado instanceof Empresa) {
	            				//System.out.println("É uma empresaaa");
	            				Scanner sc = new Scanner(System.in);
	            		    	System.out.println("Uma empresa está disposta a recolher sua encomenda");
	            		    	System.out.println(selecionado);
	            		    	double dist = selecionado.distanciaAteDestino(loja.getCoordX(), loja.getCoordY());
	            		    	loja.definirPropriedadesEncomenda(enc);
	            		    	System.out.println("Preço encomenda = " + enc.precoFinal() + " + taxa da empresa: ");
	            		    	double preco = ((Empresa) selecionado).definirPrecoEntrega(dist, loja, enc);
	            		    	System.out.println(((Empresa) selecionado).definirPrecoEntrega(dist, loja, enc));
	            		    	//System.out.println(selecionado.definirPrecoEntrega());
	            		    	System.out.println("Escreve OK para aceitar");
	            		    	System.out.println("Escreve NO para rejeitar");
	            		    	String f = sc.nextLine();
	            		    	f = f.toLowerCase();
	            		    	if(f.equals("ok")) {
	            		    		System.out.println("Aceitou a entrega");
	            		    		//System.out.println(selecionado);
	            		    		((Empresa) selecionado).setKmsPercorridos(((Empresa) selecionado).getKmsPercorridos() + dist);
	            		    		((Empresa) selecionado).setFaturamento(((Empresa) selecionado).getFaturamento() + preco - enc.precoFinal());
	            		    		if(enc != null) {
	                    				enc.setEntregador(selecionado.getCodigo());
	                    				this.g.aceites.put(enc.getCodigo(), enc);
	                    				enc.setLoja(loja.getCodigo());
	                    				System.out.println("Encomenda já tem entregador: " + selecionado);
	                    				double classificacao = classificacao(); //classificacao é uma funão auxiliar
	        	            			user.classificar(classificacao, selecionado); // função dentro de Utilizador
	        	            			//selecionado.setQtClassificacoes(selecionado.getQtClassificacoes() + 1);	 	        	            			
	        	            			ArrayList<String> h = selecionado.getEntregasEfetuadas();     	            	    	
	        	            	    	h.add(enc.toString());
	        	            	    	selecionado.setEntregasEfetuadas(h);
	        	            	    	double kms = ((Empresa) selecionado).getKmsPercorridos();
	        	            	    	((Empresa) selecionado).setKmsPercorridos(kms + dist);
	        	            	    	
	        	            	    	//LOJA -> HISTORICO
	        	            	    	ArrayList<String> encs = loja.getEncomendasEfetuadas();
	        	            	    	encs.add(enc.toString());
	        	            	    	loja.setEncomendasEfetuadas(encs);
	        	            	    	//System.out.println(selecionado);
	        	            			break;
	                    			}
	            		    	}
	            		    	else {
	            		    		System.out.println("Novamente");
	            		    		//criaEncomenda(loja,u);
	            		    		flag = true;
	            		    	}
	            				
	            			}
	            			
	            			
	            	    	
	            	    	
	            	    	
	            			
	            			
	            			
	            		}
	            		
	                    
	                    break;
	                	
	                	
	                	
	                }while(flag == true);
	                
	    		}catch(ArrayIndexOutOfBoundsException ops) {
	    			
	    		}
	            
	        } while (menuLojas.getOpcao() != 0 && flag == true); //

	    	System.out.println("voltando...");
	    }
	    

	    
	    public Encomenda criaEncomenda(Loja loja, Utilizador utilizador) {
	    	List<String> listaProdutos = new ArrayList<String>();
	    	ArrayList<LinhaEncomenda> encomenda = new ArrayList<LinhaEncomenda>();
	    	Scanner sc = new Scanner(System.in);
	    	String produto = "";
	    	do {
	    		System.out.println("Escreva o nome do produto: ('ok' para finalizar pedido)");
	    		produto = sc.nextLine();
	    		produto = produto.toLowerCase();
	    		if(!produto.equals("ok")) {
	    			listaProdutos.add(produto);
	    			LinhaEncomenda l = new LinhaEncomenda(produto);
	    			encomenda.add(l);
	    		}
	    		
	    		
	    	}while(!produto.equals("ok"));
	    	
	    	//sc.close();
	    	System.out.println("Carrinho: ");
	    	System.out.println(encomenda);
	    	//System.out.println("Encomenda feita com sucesso!");
	    	Encomenda enc = new Encomenda(utilizador.getCodigo(), loja.getCodigo(), encomenda);
	    	loja.definirPropriedadesEncomenda(enc);
	    	//System.out.println(enc.getPesoKg());
	    	//System.out.println(enc.getData());
	    	this.g.encomendas.put(enc.getCodigo(), enc);
	    	utilizador.setNumEncomendasEfetuadas(utilizador.getNumEncomendasEfetuadas() + 1);
	    	ArrayList<String> h = utilizador.getEntregasEfetuadas();
	    	if(!enc.equals(null)) {
	    		h.add(enc.toString());
	    	}
	    	
	    	utilizador.setEntregasEfetuadas(h);
	    	
	    	return enc;
	    }
	    
	    
	    
	    public double classificacao() {
	    	Scanner sc = new Scanner(System.in);
	    	int f = -1;
	    	do {
	    		System.out.println("Classificar entrega: (0 a 5)");
	    		double nota = sc.nextDouble();
	    		return nota;
	    	}while(f == -1);
	    }
	    
	    public String eEncomendaMedica() {
	    	Scanner sc = new Scanner(System.in);
	    	String resp = "";
	    	do{
		    	System.out.println("É encomenda médica? (S/N)");
		    	resp = sc.nextLine();
		    	resp = resp.toLowerCase();
	    	}while(!resp.equals("s") && !resp.equals("n"));
	    	
	    	return resp;
	    }
	
	
	    
	    // métodos auxiliares para empresa
	    public String estouLivre() {
	    	Scanner sc = new Scanner(System.in);
	    	String resp;
	    	do {	
		    	System.out.println("Está livre para fazer entregas? (S/N)");
		    	resp = sc.nextLine();
		    	resp = resp.toLowerCase();
		    	//sc.close();
	    	}while(!resp.equals("s") && !resp.equals("n"));
	    	
	    	return resp;
	    }
	    
	    
		public EntidadeTransportadora buscarPorEntregador(Encomenda enc, Loja loja) {
			double dist_min = 400;
			double dist = 0;
			EntidadeTransportadora selecionado = null;
			double lojaX = loja.getCoordX();
			double lojaY = loja.getCoordY();
			for(EntidadeTransportadora e : this.g.entregadores.values()) {
				if(e.isLivre()) {
					dist = e.distanciaAteDestino(lojaX, lojaY);
					/*if(dist >= e.getRaio()) {
						System.out.println("");
					}*/
					if(dist <= dist_min) {
						dist_min = dist;
						selecionado = e;
					}
					
				}
			}
					
			return selecionado;
		}
	
		
		public EntidadeTransportadora buscarPorEntregadorMedico(Encomenda enc, Loja loja) {
			double dist_min = 400;
			double dist = 0;
			EntidadeTransportadora selecionado = null;
			double lojaX = loja.getCoordX();
			double lojaY = loja.getCoordY();
			for(EntidadeTransportadora e : this.g.entregadores.values()) {
				if(e.aceitoTransporteMedicamentos()) {
					if(e.isLivre()) {
						dist = e.distanciaAteDestino(lojaX, lojaY);
						if(dist <= dist_min) {
							dist_min = dist;
							selecionado = e;
						}		
					}
				}
			}
			// se o entregador mais perto for um voluntário, já é automaticamente aceite.
			if(selecionado instanceof Voluntario) {
				System.out.println("É voluntario");
				this.g.aceites.put(enc.getCodigo(), enc);
				//System.out.println(Gerador.aceites.values());
			}
			else {
				System.out.println("Não é voluntário");
				// TODO : perguntar ao utilizador para ver se ele aceita
			}
			
			
			if(enc != null && selecionado != null) {
				enc.setEntregador(selecionado.getCodigo());
				System.out.println("Encomenda já tem entregador: " + selecionado);
			}
			
			return selecionado;
			
		}
		
		
		public double tempoFila() {
			Scanner sc = new Scanner(System.in);
			System.out.println("Mudar tempo médio de fila para: ");
			double tempo = sc.nextDouble();
			return tempo;
		}
		
		public void mostrarLojasDisponiveis() {
	    	//int qt = Gestao.lojas.size();
	    	int i = 1;
	    	System.out.println(" --   Lojas   --");
	    	for(Entidade l : this.g.lojas.values()) { //TODO : levar isso pro controller
	    		System.out.println("(" + i + ")" + l.toString());
	    		//System.out.println(l.toString() + "\n");
	    		i++;
	    	}	
	    	System.out.println("Voltar (0)");
	    }
		
		/*
		public List<Encomenda> encomendasFiltro(EntidadeTransportadora emp){
			return this.g.encomendas.values().
					                 stream().
					                 filter(e -> e.getEntregador().equals(emp)).
					                 collect(Collectors.toList());				                
		}*/
	
	
}
