package Models;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * Classe que guarda as informações dos voluntarios
 *
 * @author (seu nome)
 * @version (número de versão ou data)
 */


public class Voluntario extends Agente implements Serializable {

    private Encomenda e; //e==null significa que está livre para entregar
    private StatsEntrega st;//st==null significa que a encomenta nao existe

    /**
     * Construtor da classe
     * @param nome nome do voluntario
     * @param cod codigo do voluntario
     * @param clas classificacao
     * @param t
     * @param x coordenada x
     * @param y coordenada y
     * @param r raio de acao
     * @param med pode transportar encomendas medicas
     * @param d
     */
    public Voluntario(String nome, String cod, double clas, int t, double x, double y, double r, boolean med, boolean d) {
        super(nome, cod, clas, t, x, y, r, med, d);
    }

    /**
     * Soliciar transporte a este voluntario
     * @param x        Models.Encomenda a ser pedida
     * @param u        Models.Utilizador que pediu a encomenda
     * @param l        Models.Loja a processar a encomenda
     * @return
     */
    public boolean solicitar_transporte(Encomenda x, Utilizador u, Loja l) {
        if (!encomenda_in_range(u, l)) return false; //encomenda fora do raio de acao
        else {
            if (this.e == null && getDisponibilidade()) { //voluntario livre para fazer a encomenda
                e = x.clone();
                st = new StatsEntrega(LocalDateTime.now());
                return true;
            } else return false; //voluntario ocupado
        }
    }

    /**
     * Verificar se existe encomenda com um dado codigo de cliente
     * @param user_code codigo de utilizador
     * @return retorna true caso existe ou false caso contrário
     */
    public boolean has_order_in_name(String user_code) {
        try {
            return this.e.getUser().equals(user_code);
        } catch (NullPointerException e){
            return false;
        }
    }

    /**
     * Remove encomenda do voluntario (entrega feita)
     * @param user_code código do cliente
     * @param hora_chegada hora de entrega
     */
    public void remove_encomenda(String user_code, LocalDateTime hora_chegada) {
        st.setHora_saida(hora_chegada);
        this.add_encomenda_to_historico(e,st);
        this.e=null;
        this.st=null;
    }
}
