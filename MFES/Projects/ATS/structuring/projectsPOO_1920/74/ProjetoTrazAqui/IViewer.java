import java.util.List;
import java.util.AbstractMap.SimpleEntry;
import java.time.LocalDateTime;
public interface IViewer{
	/** 
	 * Metodo que inicia a execuçao do programa
	 */
	void runViewer();
	/**
	 * Metodo que guarda os valores da opçao Classificar
	 * @return SimpleEntry<String,Integer> com a String do serviço e  nota que se pretende dar
	 */
	SimpleEntry<String,String> inputClassificar();
	/**
	 * Metodo que guarda os inputs do utilizador a quando da solicitaçao de uma nova encomenda
	 * @return Encomenda
	 */ 
	Encomenda fazerEncomenda();
	/**
	 * Metodo que escolhe uma encomenda pendente consoante o input do utilizador
	 * @param List<SimpleEntry<Encomenda,Double>> lista
	 * @return String
	 */
	SimpleEntry<String,String> escolherEncomendaPendenteUtilizador(List<Pedido> lista);
	/**
         * Metodo que dada uma Lista de encomenda verifica se a encomenda colocada com input existe se sim devolve o o seu codigo e se 
         * esta vai ser aceite ou rejeitada
         * @param List<Encomenda> lista
         * @return SimpleEntry<String,String>
         */
	SimpleEntry<String,String> escolherEncomendaPendenteTransporte(List<Encomenda> lista);
	/**
	 * Metodo que atraves do input do utilizador eescolhe que encomenda escolher 
	 * @param List<Encomenda> e
	 * @return codigo da Encomenda
	 */
	String escolherEncomenda(List<Encomenda> e);
	/**
	 * Metodo que imprime uma dada List<String> 
	 * @param List<String> e um int que representa quantos valores apresentar por pagina 
	 */
	void showList(List<String> ret, int pageSize);
	/**
	 * Metodo que imprime uma mensagem
	 * @param String mensagem a imprimir
	 */
	void showMessage(String s);
	/**
     * Metodo que com os inputs do utilizador tenta criar um produto se o conseguir fazer entao devolve esse Produto
     * @return Produto criado
     */
	Produto fazProduto();
	/**
	 * Metodo que imprime uma linha para o utilizador colocar o seu input
	 * @param String mensagem a imprimir
	 */
	void showInputLine(String s);
	/**
	 * Metodo que guarda o input do utilizador numa String
	 * @return String resultante
	 */
	String recebeInput();
	/**
     * Metodo que recebe duas datas de um intervalo e verifica se o intervalo é valido
     * @return SimpleEntry<LocalDateTime,LocalDateTime>
     */
	SimpleEntry<LocalDateTime,LocalDateTime> getIntervalo();
	/**
     * Metodo que indica qual a opçao do para alterar o utilizador escolheu
     * @return String resultante
     */
	String menuPerfil();
	
}