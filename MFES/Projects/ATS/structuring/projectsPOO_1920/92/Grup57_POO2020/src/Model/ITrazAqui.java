package Model;

import java.io.IOException;
import java.util.List;

public interface ITrazAqui {

    void putUtilizador(String cod, Utilizador u);

	void putVoluntario(String cod, Voluntario v);

	void putTransportadora(String cod, Transportadora t);

	void putLoja(String cod, Loja t);

	void putEncomenda(String cod, Encomenda t);

	void putAceite(Aceite t);

	void putLogin(String username, String password);

	boolean checkCredentials(String username, String password);

	boolean checkUserName(String username);

    void changeAceitaTransportadora(String transportadora, boolean aceita);

	void changeAceitaVoluntario(String voluntario, boolean aceita);

    void changeAceitaLojas(String loja, boolean aceita);

    String extractNameByUserNameUtilizadores(String userName);

	String extractNameByUserNameVoluntarios(String userName);

	String extractNameByUserNameTransportadoras(String userName);

	String extractNameByUserNameLojas(String userName);

	String extractPassWordByUserName(String userName);

	Double extractXByUserName(String userName);

	Double extractYByUserName(String userName);

	String extractEmailByUserName(String userName);

	void changePassWord(String userName, String pw);

	void changeName(String userName, String nome);

	void changeGPS(String userName, Double x, Double y);

	void changeEmail(String userName, String email);

	void saveData(String filename) throws IOException;

	TrazAqui loadData(String filename) throws IOException, ClassNotFoundException;

	void criaEncomenda(String codEncomenda, String userName, String codLoja, double peso);

	void adicionaProdutos(String codEncomenda, String codProduto, String descricao, double quantidade, double valorUnitario);

	boolean checkExisteVoluntario(String codVoluntario);

	boolean checkExisteLoja(String codLoja);

	boolean checkExisteTransportadora(String codTransportadora);

	void insereEncomendaFilaDeEspera(String codLoja, Encomenda e);

    Encomenda extraiEncomenda(String codEncomenda);

    void adicionaEncomendaFilaDeEspera(String codLoja, Encomenda e);

	List<String> mergeVoluntariosTransportadoras(List<String> voluntarios, List<String> transportadoras);

	void preencheRegistos();

	List<RegistoEncomenda> registosDeAlguem(String cod);

	RegistosEncomenda exportRegistos();

	String atribuiTransportadoraVoluntario(String userName, int opcao);

	double custoTotalViagem(String userName, String transportadora);

	double exportDistancia(String userName, String transportadora);

	double tempoViagemVoluntario(String userName, String voluntario, int i);

	double tempoViagemTransportadora(String userName, String transportadora, int i);

	int tempoTransito();

    void addClassificacaoVoluntario(String voluntario, int classificacao);

	void addClassificacaoTransportadora(String transportadora, int classificacao);

	double exportClassMediaVoluntario(String voluntario);

    double exportClassMediaTransportadora(String transportadora);

    List<Integer> encomendaPronta(String codLoja);
}