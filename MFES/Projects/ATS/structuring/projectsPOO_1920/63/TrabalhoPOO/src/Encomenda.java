import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Encomenda implements Serializable {
    /** variaveis de instancia */
    private String codEncomenda;
    private Utilizador utilizador;
    private Loja loja;
    private Double peso;
    private List<LinhaEncomenda> linhasEncomenda;
    private boolean prontaASerLevantada;
    //private GPS localizacao;
    //private boolean encMedica; //default = false ou talvez seja melhor ter uma class so para enc Medicas

    /** variaveis de classe */
    private static int totalEncomendas = 0;

    /** constructores de classe */
    /** vazio */
    //apenas este construtor incrementa o contador de encomendas
    public Encomenda(){
        Encomenda.totalEncomendas++;
        this.codEncomenda = "e" + Encomenda.totalEncomendas;
        this.utilizador = new Utilizador();
        this.loja = new Loja();
        this.peso = 0.0;
        this.linhasEncomenda = new ArrayList<LinhaEncomenda>();
        this.prontaASerLevantada = false;
    }

    /** parametrico */
    public Encomenda(String newCodEncomenda, Utilizador newUtilizador, Loja newLoja, Double newPeso,
            LinhaEncomenda newLinhaEncomenda){
        this.codEncomenda = newCodEncomenda;
        this.utilizador = newUtilizador;
        this.loja = newLoja;
        this.peso = newPeso;
        this.linhasEncomenda = new ArrayList<LinhaEncomenda>();
        this.linhasEncomenda.add(newLinhaEncomenda);
        this.prontaASerLevantada = false;
    }

    public Encomenda(String newCodEncomenda, Utilizador newUtilizador, Loja newLoja, Double newPeso,
                     List<LinhaEncomenda> newLinhasEncomenda){
        this.codEncomenda = newCodEncomenda;
        this.utilizador = newUtilizador;
        this.loja = newLoja;
        this.peso = newPeso;
        this.linhasEncomenda = newLinhasEncomenda;
        this.prontaASerLevantada = false;
    }

    /** copia */
    public Encomenda(Encomenda newEncomenda){
        this.codEncomenda = newEncomenda.getCodEncomenda();
        this.utilizador = newEncomenda.getUtilizador();
        this.loja = newEncomenda.getLoja();
        this.peso = newEncomenda.getPeso();
        this.linhasEncomenda = newEncomenda.getLinhasEncomenda();
        this.prontaASerLevantada = newEncomenda.getProntaASerLevantada();
    }

    /** gets/sets das variaveis de instancia */
    public String getCodEncomenda(){ return this.codEncomenda; }
    public void setCodEncomenda(String newCodEncomenda){ this.codEncomenda = newCodEncomenda; }

    public Utilizador getUtilizador(){ return this.utilizador.clone(); }
    public void setUtilizador(Utilizador newUtilizador){ this.utilizador = newUtilizador.clone(); }

    public Loja getLoja(){ return this.loja.clone(); }
    public void setLoja(Loja newLoja){ this.loja = newLoja.clone(); }

    public Double getPeso(){ return this.peso; }
    public void setPeso(Double newPeso) { this.peso = newPeso; }

    public List<LinhaEncomenda> getLinhasEncomenda(){
        List<LinhaEncomenda> l = new ArrayList<>();
        for(LinhaEncomenda le : this.linhasEncomenda){
            l.add(le.clone());
        }
        return l;
    }

    public void setLinhasEncomenda(List<LinhaEncomenda> newLinhasEncomenda) {
        for(LinhaEncomenda le : newLinhasEncomenda) {
            this.linhasEncomenda.add(le.clone());
        }
    }

    public boolean getProntaASerLevantada() {
        return this.prontaASerLevantada;
    }

    public void setProntaASerLevantada(boolean prontaASerLevantada) {
        this.prontaASerLevantada = prontaASerLevantada;
    }

    /** metodos override */
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        Encomenda passed = (Encomenda) o;
        return (this.codEncomenda.equals(passed.getCodEncomenda()) &&
                this.utilizador.equals(passed.getUtilizador()) &&
                this.loja.equals(passed.getLoja()) &&
                this.peso == passed.getPeso() &&
                this.linhasEncomenda.equals(passed.getLinhasEncomenda()));
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
                sb.append("Encomenda:");
                sb.append(this.codEncomenda).append(",");
                sb.append(this.utilizador.getCodUtilizador().toString()).append(",");
                sb.append(this.loja.getCodLoja().toString()).append(",");
                sb.append(this.peso);
                for(LinhaEncomenda le : this.linhasEncomenda){
                    sb.append(",").append(le.toString());
                }
        return sb.toString();
    }

    public Encomenda clone(){
        return new Encomenda(this);
    }

    /** metodos especificos */


}
