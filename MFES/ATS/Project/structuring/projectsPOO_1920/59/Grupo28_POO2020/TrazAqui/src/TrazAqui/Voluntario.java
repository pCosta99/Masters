package TrazAqui;

import java.util.*;

public class Voluntario extends Estafeta {
    /**
     * Construtor vazio
     */
    public Voluntario() {
        super();
    }

    /**
     * Construtor parametrizado
     * @param cod String codigo
     * @param nome String nome
     * @param localizacao GPS localizacao
     * @param raio double raio
     * @param encomendasEntregues List<Encomenda> lista de encomendas entregues
     * @param pedidosEncomenda List<Encomenda> lista de encomendas pedidas
     * @param classificacao int[] array com classificacoes
     * @param disponivel boolean
     * @param certificada boolean
     */
    public Voluntario(String cod, String nome, GPS localizacao, double raio, List<Encomenda> encomendasEntregues, List<Encomenda> pedidosEncomenda, int[] classificacao, boolean disponivel, boolean certificada) {
        super(cod, nome, localizacao, raio, encomendasEntregues, pedidosEncomenda, classificacao, disponivel, certificada);
    }

    /**
     * Construtor por copia 
     * @param a Estafeta
     */
    public Voluntario(Estafeta a) {
        super(a);
    }

    /**
     * Retorna uma copia da class que a chama
     * @return Voluntario
     */
    public Voluntario clone() {
        return new Voluntario(this);
    }

    /**
     * Retorna o nome 
     * @return String
     */
    public String toStringNome() {
        return "Voluntario";
    }

}
