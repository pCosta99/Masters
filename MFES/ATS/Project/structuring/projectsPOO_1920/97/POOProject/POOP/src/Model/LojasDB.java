package Model;
import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import Exceptions.*;


/**
 * Classe que representa a Base de Dados todas as lojas
 */

public class LojasDB implements Serializable{
    private static final long serialVersionUID = 5351270800229918438L;
    private Map<String, Lojas> lojas;

    LojasDB() {
        this.lojas = new HashMap<>();
    }

    private LojasDB(LojasDB l) {
        this.lojas= l.lojas
                .values()
                .stream()
                .collect(Collectors
                        .toMap(Lojas::getCodLoja, Lojas::clone));
    }

    /**
     * Método que nos retorna as lojas
     */

    public List<Lojas> getLojas(){
        return this.lojas.values().stream().collect(Collectors.toList());
    }

    /**
     * Método que adiciona uma loja á Base de Dados
     */

    public void addLoja(Lojas l) throws JaExisteLojaException{
        if(this.lojas.putIfAbsent(l.getCodLoja(), l) != null)
            throw new JaExisteLojaException();
    }

    /**
     * Método que nos dá uma loja dado o código da mesma
     */

    public Lojas getLoja(String cdL) throws NaoExisteLojaException {
        Lojas l = this.lojas.get(cdL);
        if(l == null)
            throw new NaoExisteLojaException();
        return l;
    }

    /**
    * Método que nos dá os produtos de uma determinada loja
    */

    public List<LinhaEncomenda> getProdutos(String codLoja ) throws NaoExisteLojaException{
        Lojas l = this.lojas.get(codLoja);
        return l.getProdutos();
    }
    /**
    * Método que adiciona um produto a uma loja
    */

    public void adicionaProduto(String codLoja, LinhaEncomenda l) throws JaExisteProdutoException{
        Lojas lo = this.lojas.get(codLoja);
        lo.adicionaProduto(l);
    }

    /**
    * Método que adiciona uma encomenda a uma loja
    */

    public void addEncomenda(Encomendas e){
        String loja = e.getNomeLoja();
        this.lojas.get(loja).adicionaEncomendas(e);
    }

    public LojasDB clone() { return new LojasDB(this); }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LojasDB lojasDB = (LojasDB) o;
        return Objects.equals(lojas, lojasDB.lojas);
    }

    @Override
    public int hashCode() {
        return 1;
    }

}


