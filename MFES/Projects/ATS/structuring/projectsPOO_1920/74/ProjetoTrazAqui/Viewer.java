import java.util.Scanner;
import java.util.LinkedHashMap;
import java.util.Random;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.AbstractMap.SimpleEntry;
import java.io.Serializable;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.time.LocalDateTime;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.time.DateTimeException;
import java.util.stream.Collectors;

public class Viewer implements IViewer, Serializable{
	private IController controller;
	private String entity;
	private String tipo;
	private boolean disp;
	/**
	 * Construtor por omissao da classe
	 */
	public Viewer(){
		this.controller = new Controller();
		this.entity = "";
	}
	/**
	 * Construtor parametrizado da classe
	 * Aceita como parametros um objecto que implementa a interface IController
	 */
	public Viewer(IController c){
		this.controller = c;
		this.entity = "";
	}
	/**
	 * Construtor parametrizado da classe
	 * Aceita como parametros um objecto que implementa a interface IController e uma String
	 */
	public Viewer(IController c, String t){
		this.controller = c;
		this.entity = t;
	}
	/**
	 * Construtor de copia da classe
	 * Aceita como parametro outro objecto da classe e utiliza os metodos
	 * de acesso aos valores das variaveis de instancia
	 */  
	public Viewer(Viewer c){
		this.controller = c.getController();
		this.entity = c.getEntity();
	}
	/**
	 * Devolve o objecto que implementa a interface IController da classe
	 * @return obejcto que implementa a interface IController
	 */
	public IController getController(){
		return this.controller.clone();
	}
	/**
	 * Atualiza o objecto que implementa a interface IController da classe
	 * @param c novo controller da classe
	 */
	public void setController(IController c){
		this.controller = c.clone(); 
	}
	/**
	 * Devolve a String entity da classe
	 * @return String entity
	 */
	public String getEntity(){
		return this.entity;
	}
	/**
	 * Atualiza a String entity da classe
	 * @param c novo entity da classe
	 */
	public void setEntity(String c){
		this.entity = c; 
	}
	/**
	 * Faz clone da classe
	 * @return o clone da classe
	 */
	public Viewer clone(){
		return new Viewer(this);
	}
	/**
	 * Devolve em forma de String o objecto da classe
	 * @return A String do objecto da classe
	 */
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append(this.controller).append(this.entity);
		return sb.toString();
	}
	/**
	 * Metodo que verifica se dois obectos da classe são iguais
	 * @return boolean resulante da comparaçao
	 */
	public boolean equals(Object o){
		if(o == this)
			return true;
		if(o == null || (o.getClass() != this.getClass()))
			return false;
		Viewer p = (Viewer) o;
		return this.controller.equals(p.getController());
	}
	/**
	 * Metodo que guarda o input do utilizador numa String
	 * @return String resultante
	 */
	public String recebeInput(){
		String op = "";
		Scanner scanner = new Scanner(System.in);
		op = scanner.nextLine();
		return op;
	}
	/** 
	 * Metodo que inicia a execuçao do programa
	 */
	public void runViewer(){
		String [] op = null;
		boolean ret;
		do{
			op = inputMenuInicial();
			switch(op[0]){
				case "1":
					ret = this.inputMenulogin();
					break;
				case "2":
					ret = this.registo();
					break;
				case "3":
					try{
						this.controller.gravarEstado(this.getFileName());
						System.out.println("Ficheiro gravado com sucesso.");
					}catch(FileNotFoundException e){
						System.out.println("Ficheiro não encontrado.");
					}catch(IOException e){
						System.out.println("Ficheiro não encontrado.");
					}
					break;
				case "4":
					try{
						this.controller.carregarEstado(this.getFileName());
						System.out.println("Ficheiro carregado com sucesso.");
					}catch(FileNotFoundException e){
						System.out.println("Ficheiro não encontrado.");
					}catch(IOException e){
						System.out.println("Ficheiro não encontrado.");
					}catch(ClassNotFoundException e){
						System.out.println("Classe não encontrada.");
					}
					break;
				case "5":
					this.controller.getPedidosSistema();
					break;
			}
			if(this.entity != null && this.entity.length() > 0)
				this.inputMenuEntidade();
		}while(!op[0].equals("6"));
	}
	/**
         * Metodo que imprime o menu apresentado ao utilizador quando este pretende grava o estado atual do sistema num ficheiro
         * @return String resultante
         */
	private String getFileName(){
		System.out.println("\n+-----------------------------------------------------------------+");
		System.out.println("| Introduza o nome do ficheiro que pretende utilizar.             |");
		System.out.println("| Pressione enter para gravar no ficheiro default: trazAqui.dat.  |");
		System.out.println("+-----------------------------------------------------------------+");
		this.showInputLine("Ficheiro");
		String input = this.recebeInput();
		if(input.equals(""))
			input = "trazAqui.dat";
		return input;
	}
	/**
	 * Metodo que imprime o Menu Inical ao utilizador
	 */
	private void showMenuInicial(){
		System.out.println("\n+---------------------------------+");
		  System.out.println("|           TrazAqui!             |");
		  System.out.println("+---------------------------------+");
		  System.out.println("| 1 -> Log-in                     |");
		  System.out.println("| 2 -> Novo Registo               |");
		  System.out.println("| 3 -> Gravar em ficheiro         |");
		  System.out.println("| 4 -> Carregar de ficheiro       |");
		  System.out.println("| 5 -> Ver encomendas atuais      |");
		  System.out.println("| 6 -> Sair                       |");
		  System.out.println("+---------------------------------+");
	}	
	/**
	 * Metodo que imprime o menu da opçao LogIn
	 */
	private void showMenulogin(){
		System.out.println("\n+---------------------------------+");
		  System.out.println("|       TrazAqui! - LogIn         |");
		  System.out.println("+---------------------------------+");
		  System.out.println("| Insira o email e password       |");
		  System.out.println("| Insira exit                     |");
		  System.out.println("+---------------------------------+");
	}
	/**
	 * Metodo que imprime o menu da opçao Registo
	 */
	private void showMenuRegisto(){
		System.out.println("\n+-------------------------------------+");
		  System.out.println("|       TrazAqui! - Registo           |");
		  System.out.println("+-------------------------------------+");
		  System.out.println("| Insira exit para cancelar o registo |");
		  System.out.println("+-------------------------------------+");
	}
	/**
	 * Metodo que imprime o menu para que seja escolhido a entidade pretendida
	 */
	private void showMenuEntidade(){
		System.out.println("\n+---------------------------------+");
		  System.out.println("|           TrazAqui!             |");
		  System.out.println("+---------------------------------+");
		  System.out.println("| 1 -> Utilizador                 |");
		  System.out.println("| 2 -> Loja                       |");
		  System.out.println("| 3 -> Voluntário                 |");
		  System.out.println("| 4 -> Transportadora             |");
		  System.out.println("+---------------------------------+");
	}
	/**
	 * Metodo que imprime o menu do Utilizador
	 */
	private void showMenuUtilizador(){
		System.out.println("\n+-------------------------------------------------+");
		  System.out.println("|                   TrazAqui!                     |");
		  System.out.println("+-------------------------------------------------+");
		  System.out.println("| 0 -> Log out                                    |");
		  System.out.println("| 1 -> Solicitar uma encomenda                    |");
		  System.out.println("| 2 -> Ver estado de uma encomenda                |");
		  System.out.println("| 3 -> Verificar os serviços de entrega propostos |");
		  System.out.println("| 4 -> Classificar um serviço                     |");
		  System.out.println("| 5 -> 10 Utilizadores que mais usaram o sistema  |");
		  System.out.println("| 6 -> 10 Empresas que mais utilizam o sistema    |");
		  System.out.println("| 7 -> Registos de encomendas                     |");
		  System.out.println("| 8 -> Alterar opções gerais do perfil            |");
		  System.out.println("+-------------------------------------------------+");
	}
	/**
	 * Metodo que imprime o menu do Voluntario
	 */
	private void showMenuVoluntario(){
		System.out.println("\n+-------------------------------------------------+");
		  System.out.println("|                   TrazAqui!                     |");
		  System.out.println("+-------------------------------------------------+");
		  System.out.println("| 0 -> Log out                                    |");
		  System.out.println("| 1 -> Entregar uma encomenda                     |");
		  System.out.println("| 2 -> Aceitar proposta de serviço                |");
		  System.out.println("| 3 -> 10 Utilizadores que mais usaram o sistema  |");
		  System.out.println("| 4 -> 10 Empresas que mais utilizam o sistema    |");
		  System.out.println("| 5 -> Lista de registos encomendas               |");
		  System.out.println("| 6 -> Ver o registo de transporte num intervalo  |");
		  System.out.println("| 7 -> Alterar opções gerais do perfil            |");
		  System.out.println("+-------------------------------------------------+");
	}
	/**
	 * Metodo que imprime o menu da Transportadora
	 */
	private void showMenuTransportadora(){
		System.out.println("\n+-------------------------------------------------+");
		  System.out.println("|                   TrazAqui!                     |");
		  System.out.println("+-------------------------------------------------+");
		  System.out.println("| 0 -> Log out                                    |");
		  System.out.println("| 1 -> Aceitar Encomendas pendentes               |");
		  System.out.println("| 2 -> Visualizar encomendas por entregar         |");
		  System.out.println("| 3 -> Entregar encomenda                         |");
		  System.out.println("| 4 -> 10 Utilizadores que mais usaram o sistema  |");
		  System.out.println("| 5 -> 10 Empresas que mais utilizam o sistema    |");
		  System.out.println("| 6 -> Lista de registos de encomenda             |");
		  System.out.println("| 7 -> Ganhos num intervalo de tempo              |");
		  System.out.println("| 8 -> Ver o registo de transporte num intervalo  |");
		  System.out.println("| 9 -> Alterar opções gerais do perfil            |");
		  System.out.println("+-------------------------------------------------+");
	}
	/**
	 * Metodo que imprime o menu da Loja
	 */
	private void showMenuLoja(){
		System.out.println("\n+-------------------------------------------------+");
		  System.out.println("|                   TrazAqui!                     |");
		  System.out.println("+-------------------------------------------------+");
		  System.out.println("| 0 -> Log out                                    |");
		  System.out.println("| 1 -> Sinalizar uma encomenda como pronta        |");
		  System.out.println("| 2 -> Ver fila                                   |");
		  System.out.println("| 3 -> 10 Utilizadores que mais usaram o sistema  |");
		  System.out.println("| 4 -> 10 Empresas que mais utilizam o sistema    |");
		  System.out.println("| 5 -> Adiciona um produto ao catálogo            |");
		  System.out.println("| 6 -> Alterar opções gerais do perfil            |");
		  System.out.println("+-------------------------------------------------+");
	}
	/**
	 * Metodo que imprime o menu de alterar o perfil.
	 */
	private void showMenuPerfilGeral(){
		System.out.println("\n+--------------------------------------+");
		  System.out.println("|          TrazAqui! - Perfil          |");
		  System.out.println("+--------------------------------------+");
		  System.out.println("| 0 -> Exit                            |");
		  System.out.println("| 1 -> Alterar o nome                  |");
		  System.out.println("| 2 -> Alterar a password              |");
		  System.out.println("| 3 -> Alterar o email                 |");
		  System.out.println("+--------------------------------------+");
	}

	private void showMenuPerfilLoja(){
		System.out.println("\n+----------------------------------------+");
		  System.out.println("|          TrazAqui! - Perfil            |");
		  System.out.println("+----------------------------------------+");
		  System.out.println("| 0 -> Exit                              |");
		  System.out.println("| 1 -> Alterar o nome                    |");
		  System.out.println("| 2 -> Alterar a password                |");
		  System.out.println("| 3 -> Alterar o email                   |");
		  System.out.println("| 4 -> Alterar tamanho da fila           |");
		  System.out.println("| 5 -> Alterar tempo de atendimento médio|");
		  System.out.println("+----------------------------------------+");
	}

	private void showMenuPerfilTransporteMedico(){
		System.out.println("\n+-----------------------------------------------------+");
		  System.out.println("|                 TrazAqui! - Perfil                  |");
		  System.out.println("+-----------------------------------------------------+");
		  System.out.println("| 0 -> Exit                                           |");
		  System.out.println("| 1 -> Alterar o nome                                 |");
		  System.out.println("| 2 -> Alterar a password                             |");
		  System.out.println("| 3 -> Alterar o email                                |");
		  System.out.println("| 4 -> Alterar velocidade                             |");
		  System.out.println("| 5 -> Alterar disponibilidade                        |");
		  System.out.println("| 6 -> Alterar disponibilidade para encomendas médicas|");
		  System.out.println("+-----------------------------------------------------+");
	}

	private void showMenuPerfilTransporte(){
		System.out.println("\n+--------------------------------------+");
		  System.out.println("|          TrazAqui! - Perfil          |");
		  System.out.println("+--------------------------------------+");
		  System.out.println("| 0 -> Exit                            |");
		  System.out.println("| 1 -> Alterar o nome                  |");
		  System.out.println("| 2 -> Alterar a password              |");
		  System.out.println("| 3 -> Alterar o email                 |");
		  System.out.println("| 4 -> Alterar velocidade              |");
		  System.out.println("| 5 -> Alterar disponibilidade         |");
		  System.out.println("+--------------------------------------+");
	}

	/**
	 * Metodo que imprime o menu das encomendas 
	 */
	private void escolherEncomendaPendenteMenu(){
		System.out.println("\n+---------------------------------------------------------------------------+");
		  System.out.println("|                              TrazAqui!                                    |");
		  System.out.println("+---------------------------------------------------------------------------+");
		  System.out.println("| Insira 0 para sair                                                        |");
		  System.out.println("| Insira 1 se pretende aceitar ou 2 se pretende recusar alguma encomenda.   |");
		  System.out.println("+---------------------------------------------------------------------------+");
	}
	/**
	 * Metodo que imprime o menu da opçao de solicitar nova Encomenda do Utilizador
	 */
	private void novaEncomendaMenu(){
		System.out.println("\n+-------------------------------------------------+");
		  System.out.println("|          TrazAqui! - Nova Encomenda             |");
		  System.out.println("+-------------------------------------------------+");
		  System.out.println("| 0 -> Cancelar pedido                            |");
		  System.out.println("| 1 -> Finalizar pedido                           |");
		  System.out.println("| 2 -> Introduzir novo produto                    |");
		  System.out.println("+-------------------------------------------------+");
	}
	/**
	 * Metodo que imprime o menu quando se escolhe a opçao de visualizar as encomendas pendentes
	 */
	private void escolherEncomendaMenu(){
		System.out.println("\n+---------------------------------------------------+");
		  System.out.println("|           TrazAqui! - Aceitar encomenda           |");
		  System.out.println("+---------------------------------------------------+");
		  System.out.println("| Insira 0 para sair                                |");
		  System.out.println("| Insira o código da encomenda que pretende entregar|");
		  System.out.println("+---------------------------------------------------+");
	}
	/**
	 * Metodo que imprime o menu quando é escolhida a opçao para classificar uma entrega
	 */
	private void classificar(){
		System.out.println("\n+---------------------------------------+");
		  System.out.println("|   TrazAqui!- Classificar um serviço   |");
		  System.out.println("+---------------------------------------+");
		  System.out.println("| Insira 0 para sair.                   |");
		  System.out.println("| Insira a classificação.               |");
		  System.out.println("+---------------------------------------+");
	}

	/**
	 * Metodo que recolhe o input do Utilizador no menu Inicial
	 * @return String[]
	 */
	private String[] inputMenuInicial(){
		String op;
		boolean invalido = true;
		this.showMenuInicial();
		op = leOpcao();
		return op.split(" ");
	}	
	/**
         * Metodo que indica qual a opçao do para alterar o utilizador escolheu
         * @return String resultante
         */
	public String menuPerfil(){
		switch(this.tipo){
			case "T": case "V":
				showMenuPerfilTransporte();
				break;
			case "TM":case "VM":
				this.showMenuPerfilTransporteMedico();
				break;
			case "L":
				this.showMenuPerfilLoja();
				break;
			default:
				this.showMenuPerfilGeral();
				break;
		}
		return leOpcao();
	}
	/**
         * Metodo que dado uma Entidade altera a variavel tipo para a letra correspondente à Entidade
         * @param Entidade temp
         */
	private void getTipo(Entidade temp){
		if(temp instanceof VoluntarioMedico)
			this.tipo = "VM";
		else if(temp instanceof TransportadoraMedica)
			this.tipo = "TM";
		else if(temp instanceof Voluntario)
			this.tipo = "V";
		else if(temp instanceof Utilizador)
			this.tipo = "U";
		else if(temp instanceof Loja)
			this.tipo = "L";
		else if(temp instanceof Transportadora)
			this.tipo = "T";
	}
	/**
	 * Metodo que guarda o input do utilizador a quando do logIn para depois verificar as credenciais
	 * @boolean resultante da validaçao das credenciais
	 */
	private boolean inputMenulogin(){
		boolean exit = false, ret = false;
		//String entidade = this.inputMenuRegistoEntidade();
		Entidade temp = null;
		String [] info = {"Email", "Password"};
		String [] input = new String [2];
		String [] email = null;
		String [] pass = null;
		do{
			this.showMenulogin();
			for(int i = 0; i < 2 && !exit; i++){
				this.showInputLine(info[i]);
				input[i] = recebeInput();
				if(exited(input[i]))
					exit = true;
			}		
			if(!exit){
				email = input[0].split(" ");
				pass = input[1].split(" ");
				if(email.length == 1 && pass.length == 1){
					temp = this.controller.verificaCredenciais(email[0], pass[0]);
					if(temp != null){
						this.entity = temp.getCodigo();
						this.getTipo(temp);
					}
				}else
					System.out.println("Email e/ou password inválidos (não podem conter espaço).");
			}
		}while(this.entity.equals("") && !exit);
		return (!this.entity.equals(""));
	}
	/**
	 * Metodo que apos o input do utlizador decide que menu de que Entidade apresentar ao utilizador
	 */
	private void inputMenuEntidade(){
		switch(this.tipo){
			case "U":
				this.inputMenuUtilizador();
				break;
			case "V":case "VM":
				this.inputMenuVoluntario();
				break;
			case "T":case "TM":
				this.inputMenuTransportadora();
				break;
			case "L":
				this.inputMenuLoja();
				break;	
		}
	}
	/**
	 * Metodo que guarda o input do utlizador a quando do registo e verifica que nova Entidade se pretende registar
	 * @return String resultante
	 */
	private String inputMenuRegistoEntidade(){
		String op = null;
		boolean valido = false;
		do{
			this.showMenuEntidade();
			op = leOpcao();
			if(!op.equals("1") && !op.equals("2") && !op.equals("3") && !op.equals("4"))
				System.out.println("Opção inválida.");
			else valido = true;
		}while(!valido);
		return op;
	}
	/**
	 * Metodo que apos o input do utilizador dependendo de que Entidade se refere se sao dados validos ou nao
	 * @return boolean resultante da validaçao
	 */  
	private boolean registo(){
		boolean ret = false;
		String entidade = this.inputMenuRegistoEntidade();
		switch(entidade){
			case "1":
				String [] infoU = {"Nome","Email","Password", "Latitude", "Longitude"};
				ret = this.inputRegisto(infoU,"u");
				break;
			case "2":
				String [] infoL =  {"Nome","Email","Password", "Latitude", "Longitude"};
				ret = this.inputRegisto(infoL,"l");
				break;
			case "3":
				String [] infoV = {"Nome", "Email","Password", "Certificado Médico (1-Sim/2-Nao)", "Raio", "Latitude", "Longitude"};
				ret = this.inputRegisto(infoV,"v");
				break;
			case "4":
				String [] infoT = {"Nome","Email", "Password", "Capacidade", "Certificado Médico (1-Sim/2-Nao)", "Raio", "NIF", "Preco por kilometro","Latitude", "Longitude"};
				ret = this.inputRegisto(infoT,"t");
				break;
		}
		return ret;
	}

	/**
         * Metodo que recebe o input de um novo registo e verifica se este pode ou nao ser efectuado
         * @param String[]info, String entidade
         * @return boolean resultante da verificaçao
         */
	private boolean inputRegisto(String[]info, String entidade){
		boolean ret = false;
		boolean exit = false;
		do{
			this.showMenuRegisto();			
			String [] input = new String [info.length+1];
			input[0] = geraCodigo(entidade);
			for(int i = 0; i < info.length && !exit; i++){
				this.showInputLine(info[i]);
				input[i+1] = recebeInput();
				exit = exited(input[i+1]);
			}
			if(exit)
				System.out.println("Registo cancelado.");

			else if(!exit)
				ret = this.controller.regista(entidade,input);
		}while(!ret && !exit);
		if(!exit) 
			System.out.println("Registo Efetuado com sucesso!");
		return !ret;
	}
	/**
         * Metodo que recebe um input e verifica se este cumpre os requesitos para ser uma data se sim decolve a data criada
         * @return LoacalDateTime resultante
         */
	public LocalDateTime leData(){
		LocalDateTime data = null;
		String in = new String();
		String[] valores = null;
		boolean invalido = true;
		while(invalido){
			this.showMessage("Insira o ano, mês, dia, hora.");
			this.showInputLine("AA-MM-DD-HH");
			in = recebeInput();
			valores = in.split("-");
			
			if(valores.length == 4){
				try{
					data = LocalDateTime.of(Integer.parseInt(valores[0]),Integer.parseInt(valores[1]),Integer.parseInt(valores[2]),Integer.parseInt(valores[3]),0);
					invalido = false;
				}
				catch(DateTimeException e){
					showMessage("\nValores inválidos para a data.\n");
				}
			}
			else showMessage("\nValores inválidos para a data.\n");
		}
		return data;
	}
	/**
         * Metodo que recebe duas datas de um intervalo e verifica se o intervalo é valido
         * @return SimpleEntry<LocalDateTime,LocalDateTime>
         */
	public SimpleEntry<LocalDateTime,LocalDateTime> getIntervalo(){
		boolean invalido = true;
		LocalDateTime i1 = null;
		LocalDateTime i2 = null;
		SimpleEntry<LocalDateTime,LocalDateTime> intervalo = null;
		while(invalido){
			this.showMessage("Insira a data do inicio do intervalo de tempo.\n");
			i1 = leData();		
			this.showMessage("\nInsira a data do fim do intervalo de tempo.\n");
			i2 = leData();
			if(i2.isAfter(i1)) invalido = false;
			else showMessage("As datas inseridas não são válidas (A segunda data ocorre antes da primeira).\n");
		}
		return new SimpleEntry<LocalDateTime,LocalDateTime>(i1,i2);
	}
	/**
         * Metodo que com os inputs do utilizador tenta criar um produto se o conseguir fazer entao devolve esse Produto
         * @return Produto criado
         */
	public Produto fazProduto(){
		boolean invalida = true;
		double preco = 0.0;
		String desc = new String();
		String codigo = geraCodigo("p");
		showInputLine("Descrição");
		String input = "";
		String input2 = "";
		desc = recebeInput();
		do{
			showInputLine("Preço");
			input = recebeInput();
			showInputLine("É um produto médico? (1-sim/2-Nao):");
			input2 = recebeInput();
			if(!input.toUpperCase().equals("EXIT")){
				try{
					preco = Double.parseDouble(input);
					if(preco > 0) invalida = false;
				}
				catch(NumberFormatException e){
					showMessage("Valor inválido");
				}
				catch(NullPointerException e){
					showMessage("Valor inválido");	
				}
			}
			else invalida = false;
		}while(invalida);
		if(input.toUpperCase().equals("EXIT") || input2.toUpperCase().equals("EXIT")) return null;
		boolean medico = input2.equals("1");
		Produto ret = new Produto(codigo, desc, preco,medico);
		return ret;
	}

	/**
         * Metodo que le a opçao de um utilizador
         * @return String
         */
	public String leOpcao(){
        String op;
		boolean invalido = true;
		showInputLine("");
		op = recebeInput();
		while(invalido){
			if(op.split(" ").length == 1){
				try{
					Integer.parseInt(op);
					if(op.length() > 0) invalido = false;
				}
				catch(NumberFormatException e){
					showMessage("Input inválido. Insira nova opção.\n");	
					showInputLine("");
					op = recebeInput();
				}
			}
			else{
				showMessage("Input inválido. Insira nova opção.\n");
				showInputLine("");
				op = recebeInput();
			}
		}
		return op;
	}

	/**
	 * Método que verifica que opçao o utilizador que executar.
	 */
	private void inputMenuUtilizador(){
		String op;
		do{
			this.showMenuUtilizador();
			op = leOpcao();
			this.controller.opcoesUtilizador(op.split(" "),this.entity,this.tipo);
		}while(!op.equals("0"));
		this.entity = "";
	}
	/**
	 * Metodo que guarda os valores da opçao Classificar
	 * @return SimpleEntry<String,Integer> com a String do serviço e  nota que se pretende dar
	 */
	public SimpleEntry<String,String> inputClassificar(){
		String [] codigo = null;
		String [] classi = null;
		this.showInputLine("Código do serviço");
		codigo = this.recebeInput().split(" ");
		this.showInputLine("nota");
		classi = this.recebeInput().split(" ");
		SimpleEntry<String,String> ret = new SimpleEntry<>(codigo[0], classi[0]);
		return ret;
	}
	/**
	 * Metodo que guarda os inputs do utilizador a quando da solicitaçao de uma nova encomenda
	 * @return Encomenda
	 */ 
	public Encomenda fazerEncomenda(){
		Encomenda enc = new Encomenda();
		boolean resposta = this.getLojaAndCodigo(enc);
		boolean registoComSucesso = false;
		String in = null;
		boolean ret = false;
		if(resposta){
			do{
				this.novaEncomendaMenu();
				in = this.leOpcao();
				if(in.equals("2"))
					registoComSucesso = preencherEncomenda(enc);
				else if(in.equals("0"))
					ret = true;
			}while(!in.equals("0") && !in.equals("1"));
		}
		if(ret)
			enc = null;
		return enc;
	}
	/**
	 * Metodo que apos a solicitaçao de um encomenda preenche os campos da encomenda com os inputs do utilizador
	 * @param Encomenda enc
	 * @return boolean resultante da validaçao
	 */ 
	private boolean preencherEncomenda(Encomenda enc){
		boolean res = false, e = false, f = false, prod = false, cod = false;
		String [] produto = null;
		String []quant = null;
		Map<String,Produto> prods = this.controller.getProdutos(enc.getCodLoja());
		if(prods.size() > 0){
			this.showMessage("Loja: " + enc.getCodLoja() + "\nProdutos: ");
			/*
			for(Produto p : prods.values()){
				this.showMessage(p.toString());
			}
			*/
			this.showList(prods.values().stream().map(Produto::toString).collect(Collectors.toList()),6);
			this.showInputLine("Produto");
			produto = this.recebeInput().split(" ");
			if(produto[0].equals("0"))
				e = true;
			if(!e){
				this.showInputLine("Quantidade");
				quant = this.recebeInput().split(" ");
			}
			if(prods.containsKey(produto[0]) && Integer.parseInt(quant[0]) > 0 && produto.length == 1 && quant.length == 1){
				Produto p = prods.get(produto[0]);
				LinhaDeEncomenda n = new LinhaDeEncomenda(p.getCodigo(), p.getDesc(), Integer.parseInt(quant[0]), p.getPreco(),p.getMedico());
				enc.adicionaLinhaDeEncomenda(n);
				System.out.println("produto " + produto[0] + " adicionado à sua encomenda.");
				e = true;
			}else{
				e = false;
				System.out.println("Input inválido");
			}
		}
		return e;
	}
	/**
	 * Metodo que apos a escolha da opçao solicitaçao de uma nova encomenda imprime as Lojas do sistema e o utilizador escolhe em qual quer fazer a encomenda
	 * @param Encomenda f
	 * @return boolean que resulta da existencia da loja escolhida ou nao
	 */
	private boolean getLojaAndCodigo(Encomenda f){
		f.setCodEncomenda(this.geraCodigo("e"));
		boolean e = false,ret = false;
		String [] in = null;
		List<String> lojas = this.controller.getListaLojas();
		do{
			this.showList(lojas,10);
			this.showInputLine("Loja");
			in = this.recebeInput().split(" ");
			if(in[0].equals("0"))
				e = true;
			else if(controller.existeLoja(in[0])){
				f.setCodLoja(in[0]);
				f.setCodEncomenda(this.geraCodigo("e"));

			}
		}while(!e && !controller.existeLoja(in[0]));
		return !e;
	}

	/**
	 * Metodo que verifica que opçao o Voluntario que executar 
	 */
	private void inputMenuVoluntario(){
		String op = null;
		do{
			this.showMenuVoluntario();
			op = leOpcao(); 
			this.controller.opcoesVoluntario(op.split(" "),this.entity, this.tipo);
		}while(!op.equals("0"));
		this.entity = "";
	}

	/**
	 * Metodo que verifica que opçao o Transportadora quer executar 
	 */
	private void inputMenuTransportadora(){
		String op = null;
		do{
			this.showMenuTransportadora();
			op = leOpcao();
			this.controller.opcoesTransportadora(op.split(" "), this.entity,this.tipo);
		}while(!op.equals("0"));
		this.entity = "";
	}
	/**
	 * Metodo que verifica que opçao o Transportadora quer executar 
	 */
	private void inputMenuLoja(){
		String op = null;
		do{
			this.showMenuLoja();
			op = leOpcao();
			this.controller.opcoesLoja(op.split(" "), this.entity,this.tipo);
		}while(!op.equals("0"));
		this.entity = "";
	}
	/**
	 * Metodo que imprime uma mensagem
	 * @param String mensagem a imprimir
	 */
	public void showMessage(String s){
		System.out.println(s);
	}

	/**
	 * Metodo que imprime uma linha para o utilizador colocar o seu input
	 * @param String mensagem a imprimir
	 */
	public void showInputLine(String s){
		System.out.print(s + "> ");
	}
	/**
	 * Metodo que verifica se a String dada é igual a "EXIT"
	 * @param String op
	 * @return boolean resultante da comparaçao
	 */
	private boolean exited(String op){
		return op.split(" ")[0].toUpperCase().equals("EXIT");
	}
	/**
	 * Metodo que gera um codigo para uma encomenda de forma random
	 * @param String codigo default de uma encomenda
	 * @return codigo final da encomenda
	 */
	private String geraCodigo(String c){
		Random r = new Random();
		String ret = String.valueOf(r.nextInt(9999));
		int n;
		do{
			ret = String.valueOf(r.nextInt(9999));
			ret = (c+ret);

		} while(this.controller.validaCodigo(ret));
		return ret;
	}
	/**
	 * Metodo que atraves do input do utilizador eescolhe que encomenda escolher 
	 * @param List<Encomenda> e
	 * @return codigo da Encomenda
	 */
	public String escolherEncomenda(List<Encomenda> e){
		Iterator<Encomenda> enc = e.iterator();
		Encomenda temp = null;
		boolean ret = false,existe = false;
		String [] input = null;
		do{
			this.escolherEncomendaMenu();
			this.showInputLine("Op");
			input = this.recebeInput().split(" ");
			if(input[0].equals("0"))
				ret = true;
			else{
				String cod = input[0];
				existe = e.stream().anyMatch(v->v.getCodEncomenda().equals(cod));
				if(!existe)
					System.out.println("Código não existe");
			}
		}while(!existe && !ret);
		return input[0];
	}

	/**
	 * Metodo que escolhe uma encomenda pendente consoante o input do utilizador
	 * @param List<SimpleEntry<Encomenda,Double>> lista
	 * @return String
	 */
	public SimpleEntry<String,String> escolherEncomendaPendenteUtilizador(List<Pedido> lista){
		boolean existe = false;
		String op[] = null;
		String opcao = "";
		SimpleEntry<String,String> chosen = new SimpleEntry<>("0","");
		boolean ret = false;
		while(!existe && !ret){
			this.escolherEncomendaPendenteMenu();
			opcao = this.leOpcao();
			if(opcao.equals("0"))
				ret = true;
			else{
				this.showInputLine("Código");
				op = this.recebeInput().split(" ");
				String cod = op[0];
				existe = lista.stream().anyMatch(v->v.getEncomenda().getCodEncomenda().equals(cod));
				if(!existe)
					System.out.println("Código não existe");
				else
					chosen = new SimpleEntry<>(opcao,op[0]);
			}
		}
		return chosen;
	}
	/**
         * Metodo que dada uma Lista de encomenda verifica se a encomenda colocada com input existe se sim devolve o o seu codigo e se 
         * esta vai ser aceite ou rejeitada
         * @param List<Encomenda> lista
         * @return SimpleEntry<String,String>
         */
	public SimpleEntry<String,String> escolherEncomendaPendenteTransporte(List<Encomenda> lista){
		boolean existe = false;
		String op[] = null;
		String opcao = "";
		SimpleEntry<String,String> chosen = new SimpleEntry<>("0","");
		boolean ret = false;
		do{
			this.escolherEncomendaPendenteMenu();
			opcao = this.leOpcao();
			if(opcao.equals("0"))
				ret = true;
			else{
				this.showInputLine("Código");
				op = this.recebeInput().split(" ");
				String cod = op[0];
				existe = lista.stream().anyMatch(v->v.getCodEncomenda().equals(cod));
				if(!existe)
					System.out.println("Código não existe");
				else
					chosen = new SimpleEntry<>(opcao,op[0]);
			}
		}while(!ret && !existe);
		return chosen;
	}

	/**
	 * Metodo que imprime uma dada List<String> 
	 * @param List<String> e um int que representa quantos valores apresentar por pagina 
	 */
	public void showList(List<String> ret, int pageSize){
		int pag = 0;
		String [] op = null;
		INavegador p = new Navegador(ret, pageSize);
		if(ret.size() == 0){
			this.showMessage("Não existem resultados a apresentar.");
			return;
		}
		do{
			System.out.println(p.showPage());
			this.showInputLine("Opção");
			op = this.recebeInput().split(" ");
			while(op.length > 1 || op[0].length() != 1){
				showMessage("Input inválido");
				this.showInputLine("Opção");
				op = this.recebeInput().split(" ");
			}
			if(op[0].equals("j")){
				this.showInputLine("Página");
				try{
					pag = Integer.parseInt(this.recebeInput().split(" ")[0]);
				}
				catch(NumberFormatException e){
					showMessage("Input inválido");
				}
			}
			if(!op[0].equals("s")){
				this.controlNavegador(p,op[0],pag);
			}
		}while(!op[0].equals("s"));
	}

	/**
	 * Método que decide o que executar dentro do navegador consoante o input do utilizador
	 * @param INavegador n, String op, int pag
	 */
	private void controlNavegador(INavegador n, String op, int pag){
		if(op.length() != 1) return;
		switch(op.charAt(0)){
			case 'd':
				n.nextPage();
				break;
			case 'a':
				n.previousPage();
				break;
			case 'u':
				n.lastPage();
				break;
			case 'p':
				n.firstPage();
				break;
			case 'j':
				n.jumpToPage(pag);
				break;
		}
	}
	
}

