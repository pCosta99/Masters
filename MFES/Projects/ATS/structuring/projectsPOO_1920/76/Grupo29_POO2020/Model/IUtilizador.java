package Model;

import Exceptions.InvalidInputException;
import Utilities.Ponto;

/** Interface da classe Utilizador. */
public interface IUtilizador {
    /**
     * Devolve o id de um Utilizador. 
     * @return String com o id do Utilizador.
     */
    String getId();
  
    
    /**
     * Devolve o numero de encomedas realizadas pelo Utilizador.
     * @return int correspondente ao numero de encomedas realizadas pelo Utilizador.
     */
    int getnEnc();

    /**
     * Actualiza o numero de encomedas realizadas pelo Utilizador.
     * @param novonEnc novo numero de encomedas realizadas por este Utilizador
     */
    void setnEnc(int novonEnc) throws InvalidInputException;

    /**
     * Devolve a posiçao do Entidade.
     *
     * @return Ponto correspondente as coordenadas GPS posiçao do Entidade.
     */
    Ponto getPosicao();

    /**
     * Metodo que incrementa o contador do número de encomendas feitas por um utilizador
     */
    void incNEnc();

}