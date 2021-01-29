import java.util.List;
import java.util.Map;
import java.io.IOException;
import java.io.FileNotFoundException;
public interface IController{
	/**
     * Metodo que faz o registo de uma  nova Entidade do sistema
     * @param String entidade, String [] op
     * @return boolean resultante da criaçao da Entidade
     */
	boolean regista(String entidade, String [] op);
	/**
     * Faz clone do objecto da classe
     * @return  objecto clonizada
     */
	Controller clone();
	/**
     * Devolve o objecto que implementa a interface ISistema
     * @return objecto que implementa a interface ISistema
     */
	ISistema getModel();
	/**
     * Devolve o objecto que implementa a interface IViewer
     * @return objecto que implementa a interface IViewer
     */
	IViewer getViewer();
	/**
     * Atualiza o objecto ,que implementa a interface IViewer, da classe
     * @param v novo viewer da classe
     */
	void setViewer(IViewer v);
	
     /**
     * Método que verifica se as credenciais colocadas no registo sao validas ou nao
     * @param String entidade,String email,String pass
     * @return String da entidade resultante
     */
	Entidade verificaCredenciais(String email,String pass);
	/**
     * Método que verifca se um dado codigo representa alguma entidade do sistema
     * @param String cod
     * @return bolean resultante da verificaçao
     */
	boolean validaCodigo(String cod);
	/**
     * Método que recebendo um dado codigo correspondente a uma entidade cria uma List com todos os objectos 
     * da classe RegistosTransporte existentes na entidade
     * @param String codigo
     * @return List<RegistosTransporte>
     */
	List<RegistosTransporte> registosTransporte(String codigo);
	  /**
     * Método que executa todas as opçoes que um Utilizador do sistema tem acesso
     * @param String [] opcao, String codEntity
     */
	void opcoesUtilizador(String [] opcao, String codEntity, String tipo);
	 /**
     * Método que executa todas as opçoes que um Transportadora do sistema tem acesso
     * @param String [] opcao, String codEntity
     */
	void opcoesTransportadora(String [] opcao, String codEntity, String tipo);
	/**
     * Método que executa todas as opçoes que uma Loja do sistema tem acesso
     * @param String [] opcao, String codEntity
     */
	void opcoesLoja(String [] opcao, String codEntity, String tipo);
	/**
     * Método que executa todas as opçoes que um Voluntario do sistema tem acesso
     * @param String [] opcao, String codEntity
     */
	void opcoesVoluntario(String [] opcao, String codEntity, String tipo);
	/**
	 * Método que cria um Map com todos os produtos de uma loja
	 * @param String cod
	 * @return Map<String,Produto>
	 */
	Map<String,Produto> getProdutos(String codLoja);
	/**
     * Método que cria uma List<String> com os codigos das lojas existentes no sistema
     * @return List<String>
     */
	List<String> getListaLojas();
	/**
     * Método que grava o estado do sistema num ficheiro
     * @param String filePath
     */
	void gravarEstado(String filePath) throws FileNotFoundException, IOException;
    /**
	 * Método que carrega o estado do sistema do ficheiro
	 * @param String filePath
	 * @return Sistema
	 */
    void carregarEstado(String filePath) throws FileNotFoundException,IOException,ClassNotFoundException;
    /**
     * Metodo que verifica se existe uma loja atraves do seu codigo
     * @param String l
     * @return boolean resultante da verificaçao
     */
    boolean existeLoja(String l);
    /**
     * Metodo que apresenta com ajuda do Navegador todos os pedidos do Sistema
     */
    void getPedidosSistema();
}