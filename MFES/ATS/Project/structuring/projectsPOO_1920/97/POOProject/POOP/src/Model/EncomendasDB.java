package Model;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.*;
import java.util.stream.Collectors;
import Exceptions.*;


    /**
    * Classe que representa a Base de dados de todas as encomendas aceites 
    */

public class EncomendasDB implements Serializable{
    private static final long serialVersionUID = 7499751216948377880L;
    private Map<String,Encomendas> encomendas; 

    EncomendasDB() {
        this.encomendas = new HashMap<>();
    }

    private EncomendasDB(EncomendasDB e) {
        this.encomendas = e.encomendas
                .values()
                .stream()
                .collect(Collectors
                        .toMap(Encomendas::getCodEncomenda, Encomendas::clone));
    }

    public void addEncomenda(Encomendas e) throws JaExisteEncomendaException{
        if(this.encomendas.putIfAbsent(e.getCodEncomenda(), e) != null)
            throw new JaExisteEncomendaException();
    }

    /**
    * Getter da encomenda dado um codigo de encomenda
    */

    public Encomendas getEncomenda (String cdE) throws NaoExisteEncomendaException{
        Encomendas e = this.encomendas.get(cdE);
        if(e == null)
            throw new NaoExisteEncomendaException();
        return e;
    }

    public EncomendasDB clone() { return new EncomendasDB(this); }



    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EncomendasDB that = (EncomendasDB) o;
        return Objects.equals(encomendas, that.encomendas);
    }



    /**
    * Getter da lista de encomendas de um utilizador num determinado intervalo de tempo
    */
    List<Encomendas> getEncomendaListClientDH(User c, LocalDateTime init, LocalDateTime end) {
        String clientID = c.getCodUtilizador();
        List<Encomendas> aux = new ArrayList<>();

        for(Encomendas entry :this.encomendas.values()){
            if(entry.getCodUtilizador().equals(clientID)
                    && entry.getDataHora().isBefore(end)
                    && entry.getDataHora().isAfter(init))
                aux.add(entry.clone());
        }
         return aux;
    }



    /**
    * Getter da lista de encomendas de um certo cliente
    */
    List<Encomendas> getListEncClient(String clientID) {
        List<Encomendas> aux = new ArrayList<>();

        for(Encomendas e :this.encomendas.values()){
            if(e.getCodUtilizador().equals(clientID))
                aux.add(e.clone());
        }
        return aux;
    }

    @Override
    public int hashCode() {
        return 1;
    }


}
