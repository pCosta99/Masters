import java.io.Serializable;
import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;
public class Pedido implements Serializable{
    private boolean pendenteUtilizador;
    private boolean pendenteTransporte;
    private boolean aceite;
    private double custo;
    private String transportadora;
    private String loja;
    private Encomenda encomenda;
    private List<String> rejeitaram;
    /**
     * Construtor por omissao da classe
     */
    public Pedido(){
        this.pendenteUtilizador = false;
        this.pendenteTransporte = false;
        this.aceite = false;
        this.custo = 0.0;
        this.encomenda = new Encomenda();
        this.transportadora = "";
        this.loja = "";
        this.rejeitaram = new ArrayList<>();
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros Encomenda enc
     */ 
    public Pedido(Encomenda enc){
        this.pendenteUtilizador = false;
        this.pendenteTransporte = false;
        this.aceite = false;
        this.encomenda = enc.clone();
        this.transportadora = "";
        this.loja = "";
        this.custo = 0.0;
        this.rejeitaram = new ArrayList<>();
    }

    /**
     * Construtor parametrizado da classe
     * Aceita como parametros Encomenda enc
     */ 
    public Pedido(Encomenda enc, List<String> rej){
        this.pendenteUtilizador = false;
        this.pendenteTransporte = false;
        this.aceite = false;
        this.encomenda = enc.clone();
        this.transportadora = "";
        this.loja = "";
        this.custo = 0.0;
        this.rejeitaram = new ArrayList<>(rej);
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros boolean el, boolean et, boolean ee, Encomenda enc, String t, String l,double c
     */ 
    public Pedido(boolean el, boolean et, boolean ee, Encomenda enc, String t, String l,double c){
        this.pendenteUtilizador = el;
        this.pendenteTransporte = et;
        this.aceite = ee;
        this.encomenda = enc.clone();
        this.transportadora = t;
        this.loja = l;
        this.custo = c;
        this.rejeitaram = new ArrayList<>();
    }
    /**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */  
    public Pedido(Pedido umPedido){
        this.pendenteUtilizador = umPedido.getPendenteUtilizador();
        this.pendenteTransporte = umPedido.getPendenteTransporte();
        this.aceite = umPedido.getAceite();
        this.encomenda = umPedido.getEncomenda();
        this.transportadora = umPedido.getTransporte();
        this.loja = umPedido.getLoja();   
        this.custo = umPedido.getCusto();
        this.rejeitaram = umPedido.getRejeitaram();
    }
     /**
     * Faz clone da classe
     * @return o clone da classe
     */
    public Pedido clone(){
        return new Pedido(this);
    }
    /**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
    public boolean equals(Object o){
        if(this == o)
            return true;
        if(o == null || (o.getClass() != this.getClass()))
            return false;
        Pedido p = (Pedido) o;
        return this.pendenteUtilizador == p.getPendenteUtilizador() 
            && this.pendenteTransporte == p.getPendenteTransporte()
            && this.aceite == p.getAceite()
            && this.encomenda.equals(p.getEncomenda());
    }
    /**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Aceite: ").append(this.aceite).append("\n");
        sb.append("Pendente utilizador: ").append(this.pendenteUtilizador).append("\n");
        sb.append("pendenteTransporte: ").append(this.pendenteTransporte).append("\n");
        sb.append("custo: ").append(this.custo).append("\n");
        sb.append("transportadora: ").append(this.transportadora).append("\n");
        sb.append("loja: ").append(this.loja).append("\n");
        return sb.toString();
    }

    public List<String> getRejeitaram(){
        return this.rejeitaram.stream().collect(Collectors.toList());
    }

    public String simpleToString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda: ").append(this.encomenda.getCodEncomenda()).append("\n")
          .append("Utilizador: ").append(this.encomenda.getCodUtilizador()).append("\n")
          .append("Transporte: ").append(this.transportadora).append("\n")
          .append("Loja: ").append(this.loja).append("\n");
        return sb.toString();
    }

    public String info(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda: ").append(this.encomenda.getCodEncomenda()).append("\n")
          .append("Loja: ").append(this.encomenda.getCodLoja()).append("\n")
          .append("Transporte: ").append(this.transportadora).append("\n")
          .append("Custo: ").append(custo).append("\n");
        return sb.toString();
    }
    /**
     * Devolve o boolean pendenteUtilizador da classe
     * @return boolean pendenteUtilizador 
     */
    public boolean getPendenteUtilizador(){
        return this.pendenteUtilizador;
    }
    /**
     * Devolve o boolean pendenteTransporte da classe
     * @return boolean pendenteTransporte
     */
    public boolean getPendenteTransporte(){
        return this.pendenteTransporte;
    }
    /**
     * Devolve o boolean aceite da classe
     * @return boolean aceite
     */
    public boolean getAceite(){
        return this.aceite;
    }
    /**
     * Devolve o double custo da classe
     * @param double custo
     */
    public double getCusto(){
        return this.custo;
    }
    /**
     * Devolve a String loja da classe
     * @return String loja
     */
    public String getLoja(){
        return this.loja;
    }
    /**
     * Devolve a String transportadora da classe
     * @return String Transportadora
     */
    public String getTransporte(){
        return this.transportadora;
    }
    /**
     * Devolve a Encomenda encomenda da classe
     * @return Encomenda encomenda
     */
    public Encomenda getEncomenda(){
        return this.encomenda.clone();
    }
    /**
     * Atualiza o boolean pendenteUtilizador da classe
     * @param el novo pendenteUtilizador da classe
     */
    public void setPendenteUtilizador(boolean el){
        this.pendenteUtilizador = el;
    }
    /**
     * Atualiza o boolean pendenteTransporte da classe
     * @param et novo pendenteTransporte da classe
     */
    public void setPendenteTransporte(boolean et){
        this.pendenteTransporte = et;
    }
    /**
     * Atualiza o boolean aceite da classe
     * @param ee novo aceite da classe
     */
    public void setAceite(boolean ee){
        this.aceite = ee;
    }
    /**
     * Atualiza o double custo da classe
     * @param c novo custo da classe
     */
    public void setCusto(double c){
        this.custo = c;
    }
    /**
     * Atualiza a String loja da classe
     * @param l nova loja da classe
     */
    public void setLoja(String l){
        this.loja = l;
    }
    /**
     * Atualiza a String transportadora da classe
     * @param t nova transportadora da classe
     */
    public void setTransporte(String t){
        this.transportadora = t;
    }
    /**
     * Atualiza a Encomenda encomenda da classe
     * @param enc nova encomenda da classe
     */
    public void setEncomenda(Encomenda enc){
        this.encomenda = enc.clone();
    }
    /** 
     * Verifica se o pedido esta pronto para ser entregue
     * @return o boolean resultante dessa verificaçao
     */
    public boolean ready(){
        return !this.pendenteUtilizador && this.aceite && !this.pendenteTransporte && this.loja.equals("");
    }
    /**
     * Metodo que devolve a String codEncomenda da Encomenda
     * @return String codEncomenda
     */
    public String getCodEncomenda(){
        return this.encomenda.getCodEncomenda();
    }

    public void adicionaRejeitaram(String cod){
        this.rejeitaram.add(cod);
    }

    public boolean rejeita(String cod){
        return this.rejeitaram.contains(cod);
    }
}
