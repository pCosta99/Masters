import java.util.ArrayList;
import java.util.List;

public class Encomenda {
    /** Codigo da Encomenda **/
    private String encomenda;
    /** Codigo do Utilizador **/
    private String utilizador;
    /** Codigo da Loja **/
    private String loja;
    /** Peso da encomenda **/
    private double peso;
    /** Lista de compras **/
    private List<LinhaEncomenda> listaCompras;
    /** Estado que determina se a encomenda ja foi aceite, recusado ou ainda está em espera */
    private String estado;

    /** Construtor nulo **/
    public Encomenda(){
        this.encomenda = "";
        this.utilizador = "";
        this.loja = "";
        this.peso = 0;
        this.listaCompras = new ArrayList<>();
        this.estado = "";
    }

    /** Construtor parametrizado para a classe Encomenda **/
    public Encomenda(String en,String u, String l, int p, List<LinhaEncomenda> lc, char em, String est){
        this.encomenda = en;
        this.utilizador = u;
        this.loja = l;
        this.peso = p;
        this.listaCompras = lc;
        this.estado = est;
    }

    /** Construtor de cópia **/
    public Encomenda (Encomenda e){
        this.encomenda = e.getEncomenda();
        this.utilizador = e.getUtilizador();
        this.loja = e.getLoja();
        this.peso = e.getPeso();
        this.listaCompras = e.getListaCompras();
        this.estado = e.getEstado();
    }

    /** Retorna o codigo da Encomenda **/
    public String getEncomenda(){
        return this.encomenda;
    }

    /** Retorna o codigo do Utilizador **/
    public String getUtilizador(){
        return this.utilizador;
    }

    /** Retorna o codigo da Loja **/
    public String getLoja(){
        return this.loja;
    }

    /** Retorna o peso da Encomenda**/
    public double getPeso(){
        return this.peso;
    }

    /** Retorna a Lista de compras **/
    public List<LinhaEncomenda> getListaCompras(){
        return this.listaCompras;
    }

    /** Define o codigo da Encomenda **/
    public void setEncomenda(String enc){
        this.encomenda = enc;
    }

    /** Define o codigo do Utilizador **/
    public void setUtilizador(String uz){
        this.utilizador = uz;
    }

    /** Define o codigo da Loja **/
    public void setLoja (String lj){
        this.loja = lj;
    }

    /** Define o peso da Encomenda **/
    public void setPeso(double ps){
        this.peso = ps;
    }

    /** Define a Lista de compras **/
    public void setListaCompras (List<LinhaEncomenda> ltC){
        this.listaCompras = ltC;
    }
    
    /** Retorna o estado da Encomenda */
    public String getEstado(){
        return this.estado;
    }
    /** Define o estado da Encomenda */
    public void setEstado(String e){
        this.estado = e;
    }

    /** Método que clona uma Encomenda **/
    public Encomenda clone(){
        return new Encomenda (this);
    }
    
    /** Método que devolve um boolean true caso as Encomendas sejam iguais e false caso não sejam */
    public boolean equals(Object o){
        if (o==this) return true;
        if (o==null || o.getClass() != this.getClass()) return false;
        Encomenda e = (Encomenda) o;
        return this.getEncomenda().equals(e.getEncomenda()) &&
               this.getUtilizador().equals(e.getUtilizador()) &&
               this.getLoja().equals(e.getUtilizador()) && 
               this.getPeso() == e.getPeso() &&
               this.getListaCompras().equals(e.getListaCompras()) &&
               this.getEstado().equals(e.getEstado());
    }

    /** Método que cria uma string com a informação da Encomenda **/
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo da Encomenda: ").append(this.encomenda+"\n");
        sb.append("Codigo do Utilizador: ").append(this.utilizador+"\n");
        sb.append("Codido da Loja: ").append(this.loja+"\n");
        sb.append("Lista de compras: ").append(this.listaCompras+"\n");
        sb.append("Peso: ").append(this.peso+"\n");
        sb.append("Estado: ").append(this.estado+"\n");
        return sb.toString();
    }
}