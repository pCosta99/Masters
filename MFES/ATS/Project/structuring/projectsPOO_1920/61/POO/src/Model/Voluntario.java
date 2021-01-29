package Model;

import Exceptions.EncomendaInexistenteException;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Voluntario extends Transporte implements Serializable {

    private Encomenda paraEntregar;

    /**
     * Construtor da classe
     */
    public Voluntario(){
        super();
        paraEntregar = null;
    }

    /**
     * Construtor da classe
     * @param code O código do voluntário
     * @param nome O nome do voluntário
     * @param gps As coordenadas GPS do voluntário
     * @param encEsp A indicação do voluntário ou não de encomendas médicas
     * @param raio O raio de ação do voluntário
     * @param disp A disponibilidade do voluntário
     * @param encEntregues As encomendas entregues pelo voluntário
     * @param futurasEnc As encomendas que o voluntário tem de l
     * @param classificacoes As classificações atribuídas ao voluntário
     * @param paraEntregar As encomendas que o voluntário tem para entregar
     */
    public Voluntario(String code, String nome, GPS gps,boolean encEsp, Double raio, boolean disp, Map<Utilizador, Set<Encomenda>> encEntregues, Map<String, Encomenda> futurasEnc, List<Double> classificacoes, Encomenda paraEntregar) {
        super(code, nome, gps, raio, disp, encEntregues, futurasEnc, classificacoes, encEsp);
        setParaEntregar(paraEntregar);
    }

    /**
     * Construtor da classe
     * @param v O voluntário do qual se pretende extrair as informações
     */
    public Voluntario(Voluntario v){
        super(v);
        setParaEntregar(v.paraEntregar);
    }

    /**
     * Indica as encomendas que o voluntário tem para entregar
     * @return As encomendas que o voluntário tem para entregar
     * @throws EncomendaInexistenteException Caso não hajam encomendas
     */
    public Encomenda getParaEntregar() throws EncomendaInexistenteException {
        if (paraEntregar == null)
            throw new EncomendaInexistenteException("Nõa há encomenda para entregar.");
        return paraEntregar.clone();
    }

    /**
     * Define as encomendas que o voluntário tem para entregar
     * @param paraEntregar As encomendas que o voluntário tem para entregar
     */
    public void setParaEntregar(Encomenda paraEntregar) {
        if (paraEntregar!=null)
            this.paraEntregar = paraEntregar.clone();
        else
            this.paraEntregar = null;
    }

    /**
     * Cria um clone do voluntário
     * @return O clone do voluntário
     */
    public Voluntario clone(){
        return new Voluntario(this);
    }

    /**
     * Verifica se um objeto é igual a um voluntário
     * @param o O objeto com o qual se pretende comparar
     * @return True caso afirmativo e false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Voluntario)) return false;
        return super.equals(o);
    }


}
