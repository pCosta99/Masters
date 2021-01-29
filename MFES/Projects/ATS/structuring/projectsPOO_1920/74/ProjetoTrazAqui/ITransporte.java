import java.util.List;

public interface ITransporte{
	/**
	 * Metodo que devolve as encomendas 
	 * @return List<String>
	 	*/
	List<String> getEncomendas();
	/**
	 * Metodo que devolve a capacidade 
	 * @return int capacidade
	 */
	int getCapacidade();
	/**
	 * Metodo que devolve o boolean livre 
	 * @return boolean livre
	 */
	boolean getLivre();
	/**
	 * Metodo que atualiza o boolean livre
	 * @param state novo livre 
	 */
	void setLivre(boolean state);
	/**
	 * Metodo que atualiza o double velocidade
	 * @param vel nova velocidade 
	 */
	void setVelocidade(double vel);
	/**
	 * Metodo que devolve a Coordenada gps 
	 * @return Coordenada gps
	 */
	Coordenada getGps();
	/**
	 * Metodo que devolve o double raio 
	 * @return double raio
	 */
	double getRaio();
	/**
	 * Metodo que calcula o tempo total gasto para fazer uma entrega
	 * @param Encomenda p
	 * @return int calculado
	 */
	long tempoTotal(Encomenda p);
	/**
     * Devolve a String codigo da classe
     * @return String codigo
     */
	String getCodigo();
}