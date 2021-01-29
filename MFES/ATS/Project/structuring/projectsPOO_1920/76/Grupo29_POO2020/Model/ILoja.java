package Model;

import Utilities.Ponto;

public interface ILoja {
    /**
     * Devolve o id de uma loja. 
     * @return String com o id da loja.
     */
    String getId();
    /**
     * Devolve a posiçao do Entidade.
     *
     * @return Ponto correspondente as coordenadas GPS posiçao do Entidade.
     */
    public Ponto getPosicao();


    /*
     * Método que incrementa o número de elementos na fila da loja
     */
    public void incrementa();

    /*
     * Método que decrementa o número de elementos na fila da loja
     */
    public void decrementa();

    /*
    * @return o número de utilizadores em fila de uma @class Loja.
    */
    public int getEmFila();
}