package Model;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TrazAqui implements ITrazAqui, Serializable {
	
	private IUtilizadores utilizadores;
	private IVoluntarios voluntarios;
	private ITransportadoras transportadoras;
	private ILojas lojas;
	private IEncomendas encomendas;
	private IEncomendasAceites aceites;
	private ILogins logins;
	private IRegistosEncomenda registos;
	
	public TrazAqui() {
		this.utilizadores = new Utilizadores();
		this.voluntarios = new Voluntarios();
		this.transportadoras = new Transportadoras();
		this.lojas = new Lojas();
		this.encomendas = new Encomendas();
		this.aceites = new EncomendasAceites();
		this.logins = new Logins();
		this.registos = new RegistosEncomenda();
	}

	public TrazAqui(IUtilizadores u, IVoluntarios v, ITransportadoras t, ILojas l, IEncomendas e, IEncomendasAceites a,
					ILogins logs, IRegistosEncomenda registos) {
		setUtilizadores(u);
		setVoluntarios(v);
		setTransportadoras(t);
		setLojas(l);
		setEncomendas(e);
		setEncomendasAceites(a);
		setLogins(logs);
		setRegistos(registos);
	}


	public TrazAqui(TrazAqui ta) {
		setUtilizadores(ta.getUtilizadores());
		setVoluntarios(ta.getVoluntarios());
		setTransportadoras(ta.getTransportadoras());
		setLojas(ta.getLojas());
		setEncomendas(ta.getEncomendas());
		setEncomendasAceites(ta.getEncomendasAceites());
		setLogins(ta.getLogins());
		setRegistos(ta.getRegistos());
	}


	public IUtilizadores getUtilizadores() {
		return this.utilizadores;
	}


	public IVoluntarios getVoluntarios() {
		return this.voluntarios;
	}


	public ITransportadoras getTransportadoras() {
		return this.transportadoras;
	}


	public ILojas getLojas() {
		return this.lojas;
	}


	public IEncomendas getEncomendas() {
		return this.encomendas;
	}


	public IEncomendasAceites getEncomendasAceites() {
		return this.aceites;
	}


	public ILogins getLogins() {
		return this.logins;
	}

	public IRegistosEncomenda getRegistos() { return this.registos; }

	public void setUtilizadores(IUtilizadores u) {
		this.utilizadores = u;
	}

	
	public void setVoluntarios(IVoluntarios v) {
		this.voluntarios = v;
	}

	
	public void setTransportadoras(ITransportadoras t) {
		this.transportadoras = t;
	}


	public void setLojas(ILojas l) {
		this.lojas = l;
	}


	public void setEncomendas(IEncomendas e) {
		this.encomendas = e;
	}


	public void setEncomendasAceites(IEncomendasAceites a) {
		this.aceites = a;
	}


	public void setLogins(ILogins logs) {
		this.logins = logs;
	}

	public void setRegistos(IRegistosEncomenda registos) { this.registos = registos; }

	public TrazAqui clone() {
		return new TrazAqui(this);
	}

	@Override
	public void putUtilizador(String cod, Utilizador u) {
		this.utilizadores.put(cod, u);
	}

	@Override
	public void putVoluntario(String cod, Voluntario v) {
		this.voluntarios.put(cod, v);
	}

	@Override
	public void putTransportadora(String cod, Transportadora t) {
		this.transportadoras.put(cod, t);
	}

	@Override
	public void putLoja(String cod, Loja l) {
		this.lojas.put(cod, l);
	}

	@Override
	public void putEncomenda(String cod, Encomenda e) {
		this.encomendas.put(cod, e);
	}

	@Override
	public void putAceite(Aceite t) {
		this.aceites.add(t);
	}

	@Override
	public void putLogin(String username, String password) {
		this.logins.put(username, password);
	}
	
	@Override
	public boolean checkCredentials(String username, String password) {
		return this.logins.checkCredentials(username, password);
	}

	@Override
	public boolean checkUserName(String username) {
		return this.logins.checkUserName(username);
	}

	@Override
	public void changeAceitaTransportadora (String transportadora, boolean aceita) {
		this.transportadoras.changeAceita(transportadora,aceita);
	}

	@Override
	public void changeAceitaVoluntario (String voluntario, boolean aceita) {
		this.voluntarios.changeAceita(voluntario,aceita);
	}

	@Override
	public void changeAceitaLojas (String loja, boolean aceita) {
		this.lojas.changeAceitaFila(loja,aceita);
	}

	@Override
	public String extractNameByUserNameUtilizadores(String userName) {
		return this.utilizadores.extractNameByUserName(userName);
	}

	@Override
	public String extractNameByUserNameVoluntarios(String userName) {
		return this.voluntarios.extractNameByUserName(userName);
	}

	@Override
	public String extractNameByUserNameTransportadoras(String userName) {
		return this.transportadoras.extractNameByUserName(userName);
	}

	@Override
	public String extractNameByUserNameLojas(String userName) {
		return this.lojas.extractNameByUserName(userName);
	}

	@Override
	public String extractPassWordByUserName(String userName) {
		return this.logins.extractPassWord(userName);
	}

	@Override
	public Double extractXByUserName(String userName) {
		return this.utilizadores.extractXByUserName(userName);
	}

	@Override
	public Double extractYByUserName(String userName) {
		return this.utilizadores.extractYByUserName(userName);
	}

	@Override
	public String extractEmailByUserName(String userName) {
		return this.utilizadores.extractEmailByUserName(userName);
	}

	@Override
	public void changePassWord(String userName, String pw) {
		this.logins.changePassWord(userName,pw);
	}

	@Override
	public void changeName(String userName, String nome) {
		this.utilizadores.changeName(userName,nome);
	}

	@Override
	public void changeGPS(String userName, Double x, Double y) {
		this.utilizadores.changeGPS(userName,x,y);
	}

	@Override
	public void changeEmail(String userName, String email) {
		this.utilizadores.changeEmail(userName,email);
	}

	@Override
	public void saveData(String filename) throws IOException {
		ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(filename));
		o.writeObject(this);
		o.flush();
		o.close();
	}

	@Override
	public TrazAqui loadData(String filename) throws IOException, ClassNotFoundException {
		ObjectInputStream o = new ObjectInputStream(new FileInputStream(filename));
		TrazAqui trazAqui = (TrazAqui) o.readObject();
		o.close();
		return trazAqui;
	}

	@Override
	public void criaEncomenda(String codEncomenda, String userName, String codLoja, double peso) {
		this.encomendas.criaEncomenda(codEncomenda,userName,codLoja,peso);
	}

	@Override
	public void adicionaProdutos(String codEncomenda, String codProduto, String descricao, double quantidade, double valorUnitario) {
		this.encomendas.adicionaProdutos(codEncomenda,codProduto,descricao,quantidade,valorUnitario);
	}

	@Override
	public boolean checkExisteLoja(String codLoja) {
		return this.lojas.checkExisteLoja(codLoja);
	}

	@Override
	public boolean checkExisteVoluntario(String codVoluntario) {
		return this.voluntarios.checkExisteVoluntario(codVoluntario);
	}

	@Override
	public boolean checkExisteTransportadora(String codTransportadora) {
		return this.transportadoras.checkExisteTransportadora(codTransportadora);
	}

	@Override
	public void insereEncomendaFilaDeEspera(String codLoja, Encomenda e) {
		this.lojas.adicionaEncomendaFilaDeEspera(codLoja,e);
	}

	@Override
	public Encomenda extraiEncomenda(String codEncomenda) {
		return this.encomendas.extraiEncomenda(codEncomenda);
	}

	@Override
	public void adicionaEncomendaFilaDeEspera(String codLoja, Encomenda e) {
		this.lojas.adicionaEncomendaFilaDeEspera(codLoja, e);
	}

	@Override
	public List<String> mergeVoluntariosTransportadoras(List<String> voluntarios, List<String> transportadoras) {
		List<String> result = new ArrayList<>();
		result = Stream.of(voluntarios,transportadoras)
				       .flatMap(x -> x.stream())
				       .collect(Collectors.toList());
		return result;
	}

	@Override
	public void preencheRegistos() {
		int i = 0;
		int n = 0;
		int length = 0;
		int lengthUsers = 0;
		Random generator = new Random();
		String atribuiQuemFoi = new String();
		List<String> listQuemTransportou = new ArrayList<>();
		List<String> users = new ArrayList<>();
		List<RegistoEncomenda> temp = new ArrayList<>();
		List<String> keyLojas = new ArrayList<>();
		keyLojas = this.lojas.exportKeyLojas();
		this.registos = new RegistosEncomenda();
		listQuemTransportou = mergeVoluntariosTransportadoras(this.voluntarios.extractVoluntarios(),
				this.transportadoras.extractTransportadoras());
		users = this.utilizadores.exportUsers();
		length = listQuemTransportou.size();
		lengthUsers = users.size();
		for (String s : keyLojas) {
			i = generator.nextInt(length);
			temp = this.lojas.extraiLoja(s).queueParaRegisto(listQuemTransportou.get(i));
			for (RegistoEncomenda r : temp) {
				n = generator.nextInt(lengthUsers);
				if ((Auxiliar.voluntarioOuTransportadora(r.getQuemTransportou())) == 0) {
					r.setQuemEncomendou(users.get(n));
					this.registos.insertNosRegistos(r);
				} else {
					r.setCustoTotalTransporte((r.getDistanciaPercorrida()) *
							(this.transportadoras.extractTransportadora(r.getQuemTransportou()).getPreco()));
					r.setQuemEncomendou(users.get(n));
					this.registos.insertNosRegistos(r);
				}
			}
		}
	}

	@Override
	public List<RegistoEncomenda> registosDeAlguem(String cod) {
		return this.registos.extraiRegistosDeAlguem(cod);
	}

	@Override
	public RegistosEncomenda exportRegistos() {
		return this.registos.exportRegistos();
	}

	@Override
	public String atribuiTransportadoraVoluntario(String userName, int opcao) {
		String resultado = new String();
		int i = 0;
		int lengthVoluntarios = 0;
		int lengthTransportadoras = 0;
		double x = this.utilizadores.extractXByUserName(userName);
		double y = this.utilizadores.extractYByUserName(userName);
		List<String> baseDadosVoluntarios = new ArrayList<>();
		List<String> baseDadosTransportadoras = new ArrayList<>();
		Random generator = new Random();
		baseDadosVoluntarios = this.voluntarios.exportVoluntariosDentroRaio(x,y);
		baseDadosTransportadoras = this.transportadoras.exportTransportadorasDentroRaio(x,y);
		lengthVoluntarios = baseDadosVoluntarios.size();
		lengthTransportadoras = baseDadosTransportadoras.size();
		if (opcao == 1) {
			i = generator.nextInt(lengthVoluntarios);
			resultado = baseDadosVoluntarios.get(i);
		}
		else {
			i = generator.nextInt(lengthTransportadoras);
			resultado = baseDadosTransportadoras.get(i);
		}
		return resultado;
	}

	@Override
	public double custoTotalViagem(String userName, String transportadora) {
		double custoKM = this.transportadoras.extractCustoKM(transportadora);
		double distancia = Auxiliar.calculadoraDistancia(this.utilizadores.extractXByUserName(userName),
				this.utilizadores.extractYByUserName(userName),
				this.transportadoras.extractTransportadora(transportadora).getCoordenadas().getX(),
				this.transportadoras.extractTransportadora(transportadora).getCoordenadas().getY());
		double custoTotal = (custoKM * distancia);
		return custoTotal;
	}

	@Override
	public double exportDistancia(String userName, String transportadora) {
		double result = Auxiliar.calculadoraDistancia(this.utilizadores.extractXByUserName(userName),
				this.utilizadores.extractYByUserName(userName),
				this.transportadoras.extractTransportadora(transportadora).getCoordenadas().getX(),
				this.transportadoras.extractTransportadora(transportadora).getCoordenadas().getY());
		return result;
	}

	@Override
	public double tempoViagemVoluntario(String userName, String voluntario, int i) {
		double tempoViagem = 0;
		double distancia = 0.0;
		distancia = Auxiliar.calculadoraDistancia(this.utilizadores.extractXByUserName(userName),
				this.utilizadores.extractYByUserName(userName),
				this.voluntarios.extractVoluntario(voluntario).getCoordenadas().getX(),
				this.voluntarios.extractVoluntario(voluntario).getCoordenadas().getY());
		if (i == 0) {
			tempoViagem = (distancia / 300);
		}
		if (i == 1) {
			tempoViagem = (distancia / 200);
		}
		if (i == 2) {
			tempoViagem = (distancia / 100);
		}
		if (i == 3) {
			tempoViagem = (distancia / 60);
		}
		if (i == 4) {
			tempoViagem = (distancia / 40);
		}
		return tempoViagem;
	}

	@Override
	public double tempoViagemTransportadora(String userName, String transportadora, int i) {
		double tempoViagem = 0;
		double distancia = 0.0;
		distancia = Auxiliar.calculadoraDistancia(this.utilizadores.extractXByUserName(userName),
				this.utilizadores.extractYByUserName(userName),
				this.transportadoras.extractTransportadora(transportadora).getCoordenadas().getX(),
				this.transportadoras.extractTransportadora(transportadora).getCoordenadas().getY());
		if (i == 0) {
			tempoViagem = (distancia / 600);
		}
		if (i == 1) {
			tempoViagem = (distancia / 400);
		}
		if (i == 2) {
			tempoViagem = (distancia / 200);
		}
		if (i == 3) {
			tempoViagem = (distancia / 120);
		}
		if (i == 4) {
			tempoViagem = (distancia / 80);
		}
		return tempoViagem;
	}

	@Override
	public int tempoTransito() {
		int i = 0;
		Random generator = new Random();
		i = generator.nextInt(5);
		return i;
	}

	@Override
	public void addClassificacaoVoluntario (String voluntario, int classificacao) {
		this.voluntarios.addClassificacao(voluntario,classificacao);
	}

	@Override
	public void addClassificacaoTransportadora (String transportadora, int classificacao) {
		this.transportadoras.addClassificacao(transportadora,classificacao);
	}

	@Override
	public double exportClassMediaVoluntario (String voluntario) {
		return this.voluntarios.exportClassMediaVoluntario(voluntario);
	}

	@Override
	public double exportClassMediaTransportadora (String transportadora) {
		return this.transportadoras.exportClassMediaTransportadora(transportadora);
	}

	@Override
    public List<Integer> encomendaPronta (String codLoja) {
	    return this.lojas.encomendaPronta(codLoja);
    }
}
