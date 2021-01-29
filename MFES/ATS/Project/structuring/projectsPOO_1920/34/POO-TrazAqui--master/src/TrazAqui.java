/**
* Classe TrazAqui corresponde ao modelo da aplicação TrazAqui.
* @author grupo60
* @version 1.0
*/
import java.time.LocalDateTime;
import java.util.*;
import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.File;
import java.util.stream.Collectors;

public class TrazAqui implements Serializable {

		private LogIn login;
		private Historico historico;
		private Map<String, Utilizador> utilizadores;
		private Map<String, Loja> lojas;
		private Map<String, Voluntario> voluntarios;
		private Map<String, Transportadora> transportadoras;
		private Map<String, Encomenda> encomendas;
		private List<String> aceites;

		/**
		* Construtor por omissão para a classe TrazAqui.
		*/
		public TrazAqui(){

			this.login = new LogIn();
			this.historico = new Historico();
			this.utilizadores = new HashMap<String,Utilizador>();
			this.lojas = new HashMap<String,Loja>();
			this.voluntarios = new HashMap<String,Voluntario>();
			this.transportadoras = new HashMap<String,Transportadora>();
			this.encomendas = new HashMap<String,Encomenda>();
			this.aceites = new ArrayList<>();
		}

		/**
		* Construtor parametrizado para a classe TrazAqui.
		*
		*/
		public TrazAqui(LogIn login, Historico historico, Map<String, Utilizador> utilizadores, Map<String, Loja> lojas,
			Map<String,Voluntario> voluntarios , Map<String,Transportadora> transportadoras, Map<String,Encomenda> encomendas,List<String> aceites){
			
			this.setLogIn(login);
			this.setHistorico(historico);
			this.setUtilizadores(utilizadores);
			this.setLojas(lojas);
			this.setVoluntarios(voluntarios);
 			this.setTransportadoras(transportadoras);
			this.setEncomendas(encomendas);
			this.setAceites(aceites);
			
		}

		/**
		* Construtor por cópia para a classe TrazAqui.
		*/
		public TrazAqui(TrazAqui traz){
			this.login = traz.getLogIn();
			this.utilizadores = traz.getUtilizadores();
			this.lojas = traz.getLojas();
			this.voluntarios = traz.getVoluntarios();
			this.transportadoras = traz.getTransportadoras();
			this.encomendas = traz.getEncomendas();
			this.aceites = traz.getAceites();
			this.historico = traz.getHistorico();
		}

		/**
		* Construtor que recebe uma instância da classe Parse, para a classe TrazAqui.
		* @param parse Uma instância da classe Parse.
		*/
		public TrazAqui (Parse parse){
			
			this.login = new LogIn();
			this.historico = new Historico();
			this.utilizadores = new HashMap<String,Utilizador>();
			this.lojas = new HashMap<String,Loja>();
			this.voluntarios = new HashMap<String,Voluntario>();
			this.transportadoras = new HashMap<String,Transportadora>();
			this.encomendas = new HashMap<String,Encomenda>();
			this.aceites = new ArrayList<>();

			for (Utilizador u : parse.getUtilizadores()){
				this.utilizadores.put(u.getCodUtilizador(),u.clone());
			}

			for (Loja l: parse.getLojas()){
				this.lojas.put(l.getCodLoja(),l.clone());
			}

			for (Voluntario v: parse.getVoluntarios()){
				this.voluntarios.put(v.getCodVoluntario(), v.clone());
			}

			for (Transportadora t: parse.getTransportadoras()){
				this.transportadoras.put(t.getCodEmpresa(), t.clone());
			}

			for (Encomenda e: parse.getEncomendas()){
				this.encomendas.put(e.getCodigoEncomenda(), e.clone());
			}

			for (Servico s: parse.getServicos()){
				this.historico.addServico(s.clone());
			}

			for(String e:parse.getAceites()){
				this.aceites.add(e);
			}
		}

		public LogIn getLogIn(){
			return this.login.clone();
		}

		public Historico getHistorico(){
			return this.historico.clone();
		}

		/**
		* Método Getter da variável utilizadores para a classe TrazAqui.
		* @return um Map com os utilizadores.
		*/
		public Map<String,Utilizador> getUtilizadores(){
			Map<String, Utilizador> clone = new HashMap<String,Utilizador>();

			for (Utilizador u: this.utilizadores.values()){
				clone.put(u.getCodUtilizador(), u.clone());
			}

			return clone;
		}

		/**
		* Método Getter para a variável lojas da classe TrazAqui.
		* @return um Map com as lojas.
		*/
		public Map<String,Loja> getLojas(){
			Map<String, Loja> clone = new HashMap<String,Loja>();

			for (Loja l: this.lojas.values()){
				clone.put(l.getCodLoja(), l.clone());
			}

			return clone;
		}

		/**
		* Método Getter para a varável voluntarios da classe TrazAqui.
		* @return um Map com os voluntarios.
		*/
		public Map<String, Voluntario> getVoluntarios(){
			Map<String,Voluntario> clone = new HashMap<String,Voluntario>();

			for (Voluntario v: this.voluntarios.values()){
				clone.put(v.getCodVoluntario(), v.clone());
			}

			return clone;
		}

		/**
		* Método Getter para a variável transportadoras para a classe TrazAqui.
		* @return um Map com as transportadoras.
		*/
		public Map<String, Transportadora> getTransportadoras(){
			Map<String, Transportadora> clone = new HashMap<String,Transportadora>();

			for (Transportadora t: this.transportadoras.values()){
				clone.put(t.getCodEmpresa(), t.clone());
			}

			return clone;
		}

		/**
		* Método Getter para a variável encomendas para a classe TrazAqui.
		* @return Um map com as encomendas.
		*/
		public Map<String, Encomenda> getEncomendas(){
			Map<String,Encomenda> clone = new HashMap<String, Encomenda>();

			for (Encomenda e: this.encomendas.values()){
				clone.put(e.getCodigoEncomenda(), e.clone());
			}

			return clone;
		}

	public List<String> getAceites() {
		return aceites;
	}

	public void setLogIn(LogIn login){
			this.login = login.clone();
		}

		public void setHistorico(Historico historico){
			this.historico = historico.clone();
		}

		/**
		* Método Setter para a variável utilizadores para a classe TrazAqui.
		* @param utilizadores Um map com os utilizadores.
		*/
		public void setUtilizadores(Map<String,Utilizador> utilizadores){
			for (Utilizador u: utilizadores.values()){
				this.utilizadores.put(u.getCodUtilizador(), u.clone());
			}
		}

		/**
		* Método Setter para a variável lojas para a classe TrazAqui.
		* @param lojas Um map com as lojas.
		*/
		public void setLojas(Map<String, Loja> lojas){
			for (Loja l: lojas.values()){
				this.lojas.put(l.getCodLoja(), l.clone());
			}
		}

		/**
		* Método Setter para a variável voluntarios para a classe TrazAqui.
		* @param voluntarios Um map com os voluntarios.
		*/
		public void setVoluntarios(Map<String,Voluntario> voluntarios){
			for(Voluntario v: voluntarios.values()){
				this.voluntarios.put(v.getCodVoluntario(), v.clone());
			}
		}

		/**
		* Método Setter para a variável transportadoras para a classe TrazAqui.
		* @param transportadoras Um map com as transportadoras.
		*/
		public void setTransportadoras(Map<String,Transportadora> transportadoras){
			for(Transportadora t: transportadoras.values()){
				this.transportadoras.put(t.getCodEmpresa(), t.clone());
			}
		}

		/**
		* Método Setter para a variável encomendas para a classe TrazAqui.
		* @param encomendas Um map com das encomendas.
		*/
		public void setEncomendas(Map<String, Encomenda> encomendas){
			for(Encomenda e: encomendas.values()){
				this.encomendas.put(e.getCodigoEncomenda(),e.clone());
			}
		}

		public void setAceites(List<String> aceites) {
			this.aceites = new ArrayList<>(aceites);
		}

	/**
		* Método toString para a classe TrazAqui.
		* @return String com informação do estado de um objecto TrazAqui.
		*/
		public String toString(){
			StringBuilder sb = new StringBuilder();

			for (Utilizador u: this.utilizadores.values()){
				sb.append(u.toString()).append("\n");
			}

			for(Loja l: this.lojas.values()){
				sb.append(l.toString()).append("\n");
			}

			for(Voluntario v: this.voluntarios.values()){
				sb.append(v.toString()).append("\n");
			}

			for (Transportadora t: this.transportadoras.values()){
				sb.append(t.toString()).append("\n");
			}
			
			for (Encomenda e: this.encomendas.values()){
				sb.append(e.toString()).append("\n");
				}

				sb.append("Aceites:\n");
			for (String s: this.aceites){
				sb.append(s+"\n");

			}

			for (Servico ser: this.historico.getServicos()){
				sb.append("Servicos:\n");
				sb.append(ser+"\n");
			}

			

			return sb.toString();
		}

		public TrazAqui clone(){
			return new TrazAqui(this);
		}

	/**
	* Método que grava o estado de uma instância da classe TrazAqui.
	* @param file O nome do ficheiro a guardar o objecto.
	*/
    public void gravaTrazAqui(String file) throws IOException {

    	
        FileOutputStream f = new FileOutputStream(new File(file));
		ObjectOutputStream o = new ObjectOutputStream(f);
		o.writeObject(this);	
		

		o.close();
		f.close();

	}

		/**
		 * Metodo que devolve uma Loja, sabendo o seu nome
		 */

		public Loja getLoja(String cod){
			Loja l = this.lojas.get(cod);
			if(l!= null) return l;
			return null;
		}

	public void addLogIn (String mail,Logger a){
		this.login.addLogIn(mail, a);
	}

		public void addUtilizador(String cod,Utilizador u){
			this.utilizadores.put(cod,u);
		}

		public void addTransportadora(String cod,Transportadora t){
			this.transportadoras.put(cod,t);
		}

		public void addVoluntario(String cod,Voluntario t){
			this.voluntarios.put(cod,t);
		}

		public void addLoja(String cod,Loja t){
			this.lojas.put(cod,t);
		}

		public void addEncomenda(String cod,Encomenda e){this.encomendas.put(cod,e);}

		public List<Encomenda> encomendasPLoja(String codLoja){
			return this.encomendas.values().stream().filter(e->e.getCodigoLoja().equals(codLoja) && !(aceites.contains(e.getCodigoEncomenda()))).collect(Collectors.toList());
		}


	public void menuAceitacaoLoja(List<Encomenda> lista){
			Scanner ler= new Scanner(System.in).useDelimiter("\n");
			if (lista.size()== 0) System.out.println("Nao ha encomendas pendentes na sua loja");
		    else{
		    	for(int i=0;i<lista.size();){
					System.out.println(lista.get(i));
					System.out.println("1)Aceitar Encomenda     2)Proxima Encomenda        3)Sair");
					int aux = ler.nextInt();
					if(aux==1) {
						this.aceites.add(lista.get(i).getCodigoEncomenda());
						i++;
					}else if(aux==2)i++;
					else break;
				}
			}
	}

	public void menuHistorico(){
		Scanner ler= new Scanner(System.in).useDelimiter("\n");
		System.out.println("Codigo de entidade: ");
		String codTransportador = ler.next();
		LocalDateTime data1=null, data2=null;
		int help =0;
		while(help==0) {
			System.out.println("Escolhe quais as datas que pretende ver a informacao:\nData inicial: ");
			data1 = this.scanDate();
			System.out.println("Data final: ");
			data2 = this.scanDate();
			if(data2.isBefore(data1)){
				System.out.println("Datas Invalidas");
			}else help++;
		}
		LocalDateTime finalData1 = data1;
		LocalDateTime finalData2 = data2;
		List<Servico> lista = this.getHistorico().getServicos().stream().filter(Servico::isConcluido).filter(e->e.isEntreDatas(finalData1, finalData2) && e.getCodTranportador().equals(codTransportador)).collect(Collectors.toList());
		if(lista.size()==0) System.out.println("Nao ha servicos desta entidade nestas datas.");
		else System.out.println(lista);
	}


	public void menuHistoricoSEntidade(){
		Scanner ler= new Scanner(System.in).useDelimiter("\n");
		LocalDateTime data1=null, data2=null;
		int help =0;
		while(help==0) {
			System.out.println("Escolhe quais as datas que pretende ver a informacao:\nData inicial: ");
			data1 = this.scanDate();
			System.out.println("Data final: ");
			data2 = this.scanDate();
			if(data2.isBefore(data1)){
				System.out.println("Datas Invalidas");
			}else help++;
		}
		LocalDateTime finalData1 = data1;
		LocalDateTime finalData2 = data2;
		List<Servico> lista = this.getHistorico().getServicos().stream().filter(Servico::isConcluido).filter(e->e.isEntreDatas(finalData1, finalData2)&& e.isConcluido()).collect(Collectors.toList());
		if(lista.size()==0) System.out.println("Nao ha servicos nestas datas.");
		else System.out.println(lista);
	}


	public LocalDateTime scanDate(){
		Scanner ler= new Scanner(System.in).useDelimiter("\n");
		int dia=0,mes=0,hora=0,minuto=0;
		int help=0;
		while(help==0) {
			System.out.println("Dia: ");
			dia = ler.nextInt();
			if(dia >=0 && dia <= 31) help++;
			else System.out.println("Dia invalido!");
		}
		while(help==1){
			System.out.println("Mes: ");
			mes = ler.nextInt();
			if(0 <= mes&& mes <=12) help++;
			else System.out.println("Mes invalido");
		}
		while(help==2){
			System.out.println("Hora: ");
			hora = ler.nextInt();
			if(0 <= hora && hora <=24) help++;
			else System.out.println("Hora invalida");
		}
		while(help==3){
			System.out.println("Minuto: ");
			minuto = ler.nextInt();
			if(0 <= minuto && minuto <=60) help++;
			else System.out.println("Minuto invalido");
		}
		while(help==4) break;
		return  LocalDateTime.of(2020,mes,dia,hora,minuto);
	}

	public double totalFaturado(String cod){
		System.out.println("Escolha as datas:");
		LocalDateTime data1=null,data2=null;
		double res = 0;
		int help =0;
		while(help==0) {
			System.out.println("Escolhe quais as datas que pretende ver a informacao:\nData inicial: ");
			data1 = this.scanDate();
			System.out.println("Data final: ");
			data2 = this.scanDate();
			if(data2.isBefore(data1)){
				System.out.println("Datas Invalidas");
			}else help++;
		}
		LocalDateTime finalData1 = data1;
		LocalDateTime finalData2 = data2;
		List<Servico> lista = this.getHistorico().getServicos().stream().filter(e->e.getCodTranportador().equals(cod)).collect(Collectors.toList());
		if(lista.size()>0) res = lista.stream().filter(e-> e.isEntreDatas(finalData1,finalData2) && e.isConcluido()).mapToDouble(e->e.getPreco()).sum();
		return res;
	}

	/**
	* Método que determina as 10 empresas com mais kms percorridos.
	* @return Uma Lista com os códigos das 10 empresas com mais kms percorridos.
	*/
	public List<AuxiliarEmpresaKms> empresasMaisKms() throws IndexOutOfBoundsException{

		// filtrar servicos feitos por empresas transportadoras.
		List<Servico> lista = 
			this.historico.getServicos().stream().filter(s->s.getCodTranportador().charAt(0) == 't').collect(Collectors.toList());

		List<String> temp = new ArrayList<String>();

		List<AuxiliarEmpresaKms> aux = new ArrayList<AuxiliarEmpresaKms>();

		for (Servico s: lista){

			if (!temp.contains(s.getCodTranportador())) {
			double kms = 
				lista.stream().filter(x->x.getCodTranportador().equals(s.getCodTranportador()) && x.isConcluido()).map(Servico :: getKmPercorridos).mapToDouble(num-> (double) num).sum();

				aux.add(new AuxiliarEmpresaKms(s.getCodTranportador(), kms));
				temp.add(s.getCodTranportador());
		}
	}


		Collections.sort(aux, new AuxiliarEmpresaKmsComparator());


		int s = aux.size();

		if (s< 10) {
			return aux.subList(0,s);
		}		

		return aux.subList(0,10);
	}

	/**
	* Determinar o código dos 10 utilizadores que mais utilizam o sistema no que toca ao número de encomendas transportadas.
	* @return Lista de AuxiliarUtilizadorEncomendas.
	*/
	public List<AuxiliarUtilizadorEncomendas> utilizadoresMaisEncomendas()  throws IndexOutOfBoundsException{
		List<Servico> lista = this.getHistorico().getServicos();

		List<String> temp = new ArrayList<String>();
		List<AuxiliarUtilizadorEncomendas> aux = new ArrayList<AuxiliarUtilizadorEncomendas>();

		for (Servico s: lista) {
			if (!temp.contains(s.getCodUtilizador())) {
				double numEncomendas = lista.stream().filter(x->x.getCodUtilizador().equals(s.getCodUtilizador()) && x.isConcluido()).count();//mapToDouble(num ->(double) num).sum();
			aux.add(new AuxiliarUtilizadorEncomendas(s.getCodUtilizador(), numEncomendas));
			temp.add(s.getCodUtilizador());
		}
	}

	Collections.sort(aux, new AuxiliarUtilizadorEncomendasComparator());

	int s = aux.size();

		if (s< 10) {
			return aux.subList(0,s);
		}		

		return aux.subList(0,10);
	}

	public List<Encomenda> encomendasDisponiveisVoluntario(Voluntario v) {
		List<Encomenda> lista = new ArrayList<Encomenda>();
		Utilizador u = new Utilizador();
		Loja l = new Loja();

		for (Encomenda e: this.encomendas.values()) {
			String codU = e.getCodigoUtilizador();
			String codL = e.getCodigoLoja();

			try {
				u = this.utilizadores.get(codU);
				l = this.lojas.get(codL);
			}
			catch(Exception ex) {
				System.out.println ("Utilizador ou loja não existentes.");
			}

			if ((v.getGps().distancia(u.getGps()) <= v.getRaio()) && (v.getGps().distancia(l.getGps()) <= v.getRaio())) {
				lista.add(e.clone());
			}
		}

		return lista;
	}

	public List<Encomenda> encomendasDisponiveisTransportadora(Transportadora t) {
		List<Encomenda> lista = new ArrayList<Encomenda>();
		Utilizador u = new Utilizador();
		Loja l = new Loja();

		for (Encomenda e: this.encomendas.values()) {
			String codU = e.getCodigoUtilizador();
			String codL = e.getCodigoLoja();

			try {
				u = this.utilizadores.get(codU);
				l = this.lojas.get(codL);
			}
			catch(Exception ex) {
				System.out.println ("Utilizador ou loja não existentes.");
			}

			if ((t.getGps().distancia(u.getGps()) <= t.getRaio()) && (t.getGps().distancia(l.getGps()) <= t.getRaio())) {
				lista.add(e.clone());
			}
		}

		return lista;
	}


	public void menuPropostas(List<Encomenda> list,Transportadora transportadora){
		Scanner ler= new Scanner(System.in).useDelimiter("\n");
		for(Encomenda e:list){
			int opcao= 0;
				System.out.println("Lista de encomendas disponiveis");
				System.out.println(e);
				double km = this.lojas.get(e.getCodigoLoja()).getGps().distancia(this.utilizadores.get(e.getCodigoUtilizador()).getGps());
				double kmT = km + transportadora.getGps().distancia(this.lojas.get(e.getCodigoLoja()).getGps());
				System.out.println("Distancia total percorrida em Km: " + kmT);
				System.out.println("Fazer Proposta?\n1)Sim       2)Nao        3)Sair");
				opcao = ler.nextInt();
				if(opcao==1){
					Servico servico = this.addServico(transportadora.getCodEmpresa(),e,km);
					this.historico.addServico(servico);
				}
				else if(opcao==3){
					break;
				}
			}
		}


		public void menuServicos(List<Encomenda> list, Voluntario voluntario){
			Scanner ler= new Scanner(System.in).useDelimiter("\n");
			for(Encomenda e:list){
				int opcao= 0;
				System.out.println("Lista de encomendas disponiveis");
				System.out.println(e);
				double km = this.lojas.get(e.getCodigoLoja()).getGps().distancia(this.utilizadores.get(e.getCodigoUtilizador()).getGps());
				double kmT = km + voluntario.getGps().distancia(this.lojas.get(e.getCodigoLoja()).getGps());
				System.out.println("Distancia total percorrida em Km: " + kmT);
				System.out.println("Fazer Encomenda?\n1)Sim       2)Nao        3)Sair");
				opcao = ler.nextInt();
				if(opcao==1){
					System.out.println("Codigo servico(numero a sua escolha):");
					String codS = ("s" + ler.next());
					System.out.println("Data de Inicio de Entrega:");
					LocalDateTime data = this.scanDate();
					System.out.println("Tempo demorado(dias):");
					int dias = ler.nextInt();
					Servico servico = new Servico(codS,true,e,data,e.getCodigoUtilizador(),voluntario.getCodVoluntario(),km,0,dias);
					this.historico.addServico(servico);
					this.encomendas.keySet().remove(e.getCodigoEncomenda());
					this.aceites.remove(e.getCodigoEncomenda());
					break;
				}
				else if(opcao==3){
					break;
				}
			}
		}

	public Servico addServico(String codTransportador,Encomenda encomenda,double km) {
		Scanner ler = new Scanner(System.in).useDelimiter("\n");

		int dias =0;
		String codigoS = null;
		double preco = 0;

			System.out.println("Codigo Servico(numero a sua escolha):");
			codigoS = ("s" + ler.next());

			System.out.println("Preco:");
			preco = ler.nextDouble();


			System.out.println("Dias estimados:");
			dias = ler.nextInt();



		return new Servico(codigoS, false, encomenda, null, encomenda.getCodigoUtilizador(),codTransportador,km,preco,dias);
	}

	public Transportadora getTransByCode(String cod){
		return this.transportadoras.get(cod);
	}


	public void menuAceitacaoProposta(List<Servico> l){
		Scanner ler = new Scanner(System.in).useDelimiter("\n");

		for (Servico s: l){
			Transportadora transportadora = this.getTransByCode(s.getCodTranportador());
				System.out.println(s.getEncomenda());
				System.out.println(transportadora.getCodEmpresa() + "," + transportadora.getNomeEmpresa());
				System.out.println("Tempo de espera: " + s.getDias() + "Dias");
				System.out.println("Preco: " + s.getPreco() + " euros");
				System.out.println("Aceitar Servico?\n1)Sim      2)Nao     3)Proximo      0)Sair");
				int op = ler.nextInt();
				if(op==1){
					System.out.println("Data Atual:");
					LocalDateTime date = scanDate();
					Servico servico = new Servico(s.getCodServico(),true,s.getEncomenda(),date,s.getCodUtilizador(),s.getCodTranportador(),s.getKmPercorridos(),s.getPreco(),s.getDias());
					this.historico.removeServico(s);
					this.historico.addServico(servico);
					this.encomendas.keySet().remove(s.getEncomenda().getCodigoEncomenda());
					this.aceites.remove(s.getEncomenda().getCodigoEncomenda());
					break;
				}
				else if(op==2){
					System.out.println("Servico Rejeitado");
					this.historico.removeServico(s);
					break;
				}
				else if(op==3) break;
			}

	}

	public void changeClassificacao(Servico s, int classificacao){
		int indice = this.getHistorico().getIndiceServico(s);
		this.historico.setClassServico(classificacao,indice);
	}

}

