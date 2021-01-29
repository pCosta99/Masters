package Model;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import Exceptions.*;


    /**
    * Classe que representa a Base de dados de todas as encomendas aceites 
    */
public class EncomendasAceitesDB implements Serializable{
    private static final long serialVersionUID = -1198946310732692558L;
    private Map<String,EncomendaAceite> encomendasA; 

    EncomendasAceitesDB() {
        this.encomendasA = new HashMap<>();
    }

    private EncomendasAceitesDB(EncomendasAceitesDB e) {
        this.encomendasA = e.encomendasA.values()
                                        .stream()
                                        .collect(Collectors
                                        .toMap(EncomendaAceite::getCodEncomenda, EncomendaAceite::clone));
    }

    /**
     * Método que adiciona encomenda aceites 
     */

    public void addEncomendaAceite(EncomendaAceite e) throws JaExisteEncomendaAceiteException{
        if(this.encomendasA.putIfAbsent(e.getCodEncomenda(), e) != null)
            throw new JaExisteEncomendaAceiteException();
    }

    /**
    * Getter da encomenda aceite dado um codigo de encomenda
    */

    public EncomendaAceite getEncomenda (String cdE) throws NaoExisteEncomendaAceiteException {
        EncomendaAceite e = this.encomendasA.get(cdE);
        if(e == null)
            throw new NaoExisteEncomendaAceiteException();
        return e;
    }

    public EncomendasAceitesDB clone() { return new EncomendasAceitesDB(this); }



    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EncomendasAceitesDB that = (EncomendasAceitesDB) o;
        return Objects.equals(encomendasA, that.encomendasA);
    }

    @Override
    public int hashCode() {
        return 1;
    }
}