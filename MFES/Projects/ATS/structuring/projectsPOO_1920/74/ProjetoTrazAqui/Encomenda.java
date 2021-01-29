import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
import java.util.Random;

//Encomenda:<CodEncomenda>, <CodUtilizador>, <CodLoja>, <Peso>, <LinhaEncomenda>+
public class Encomenda implements Serializable{
    private String codEncomenda;
    private Coordenada localizacaoLoja;
    private Coordenada localizacaoUtilizador;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private List<LinhaDeEncomenda> produtos;
    private boolean medicas;
    /**
     * Construtor por omissao da classe
     */
    public Encomenda(){
        this.codEncomenda  = "";
        this.codUtilizador = "";
        this.codLoja = "";
        this.peso = 0;
        this.produtos = new ArrayList<>();
        this.medicas = false;
        this.localizacaoUtilizador = new Coordenada();
        this.localizacaoLoja = new Coordenada();
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros String codEncomenda, String codUtilizador, String codLoja e double peso
     */ 
    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso){
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.medicas = false;
        this.produtos = new ArrayList<>();  
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros String codEncomenda,Coordenada ll, Coordenada lu, String codUtilizador, 
     * String codLoja, int peso, List<LinhaDeEncomenda> p, boolean medicas
     */
    public Encomenda(String codEncomenda,Coordenada ll, Coordenada lu, String codUtilizador, String codLoja, int peso, List<LinhaDeEncomenda> p, boolean medicas){
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.medicas = medicas;
        this.localizacaoLoja = ll;
        this.localizacaoUtilizador = lu;
        this.setProdutos(p);   
    }
    /**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */  
    public Encomenda(Encomenda umaEncomenda){
        this.codEncomenda = umaEncomenda.getCodEncomenda();
        this.codUtilizador = umaEncomenda.getCodUtilizador();
        this.codLoja = umaEncomenda.getCodLoja();
        this.peso = umaEncomenda.getPeso();
        this.produtos = umaEncomenda.getProdutos();  
        this.medicas = umaEncomenda.getMedicas();
        this.localizacaoUtilizador = umaEncomenda.getLocalizacaoUtilizador();
        this.localizacaoLoja = umaEncomenda.getLocalizacaoLoja();
        
    }
    /**
     * Devolve a String codEncomenda da classe
     * @return String codEncomenda
     */
    public String getCodEncomenda(){
        return this.codEncomenda;
    }
     /**
     * Devolve a String codUtilizador da classe
     * @return String codUtilizador
     */
    public String getCodUtilizador(){
        return this.codUtilizador;
    }
    /**
     * Devolve a String codLoja da classe
     * @return String codLoja
     */
    public String getCodLoja(){
        return this.codLoja;
    }
    /**
     * Devolve a double peso da classe
     * @return double peso
     */
    public double getPeso(){
        return this.peso;
    }
    /**
     * Devolve a Coordenada localizacaoLoja da classe
     * @return Coordenada localizacaoLoja
     */
    public Coordenada getLocalizacaoLoja(){
        return this.localizacaoLoja.clone();
    }
    /**
     * Devolve a Coordenada localizacaoUtilizador da classe
     * @return Coordenada localizacaoUtilizador
     */
    public Coordenada getLocalizacaoUtilizador(){
        return this.localizacaoUtilizador.clone();
    }
    /**
     * Devolve a List<LinhaDeEncomenda> produtos da classe
     * @return List<LinhaDeEncomenda> produtos
     */ 
    public List<LinhaDeEncomenda> getProdutos(){
        List<LinhaDeEncomenda> ret = new ArrayList<>();
        for(LinhaDeEncomenda p:this.produtos) 
            ret.add(p.clone());
        return ret;
    }   
    /**
     * Devolve o boolean medicas da classe
     * @return boolean medicas
     */
    public boolean getMedicas(){
        return this.medicas;
    }
    /**
     * Atualiza a String codEncomenda da classe
     * @param codEncomenda novo codEncomenda da classe
     */
    public void setCodEncomenda(String codEncomenda){
        this.codEncomenda = codEncomenda;
    }
    /**
     * Atualiza a String codUtilizador da classe
     * @param codUtilizador novo codUtilizador da classe
     */
    public void setCodUtilizador(String codUtilizador){
        this.codUtilizador = codUtilizador;
    }
    /**
     * Atualiza a String codLoja da classe
     * @param codLoja novo codLoja da classe
     */
    public void setCodLoja(String codLoja){
        this.codLoja = codLoja;
    }
     /**
     * Atualiza o double peso da classe
     * @param peso novo peso da classe
     */
    public void setPeso(double peso){
        this.peso = peso;
    }
    /**
     * Atualiza a Coordenada localizacaoLoja da classe
     * @param ll novo localizacaoLoja da classe
     */
    public void setLocalizacaoLoja(Coordenada ll){
        this.localizacaoLoja = ll.clone();
    }
    /**
     * Atualiza a Coordenada localizacaoUtilizador da classe
     * @param l novo localizacaoUtilizador da classe
     */
    public void setLocalizacaoUtilizador(Coordenada l){
        this.localizacaoUtilizador = l.clone();
    }
    /**
     * Atualiza a List<LinhaDeEncomenda> produtos da classe
     * @param prod nova List<LinhaDeEncomenda> da classe
     */
    public void setProdutos(List<LinhaDeEncomenda> prod){
        this.produtos = new ArrayList<>();
        for(LinhaDeEncomenda p :prod)
            this.produtos.add(p.clone());
    }
    /**
     * Atualiza o boolean medicas da classe
     * @param medicas novo boolean da classe
     */
    public void setMedicas(boolean medicas){
        this.medicas = medicas;
    }
    /**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("CodEncomenda: ").append(this.codEncomenda).append("\n")
            .append("CodUtilizador: ").append(this.codUtilizador).append("\n")
            .append("CodLoja: ").append(this.codLoja).append("\n")
            .append("Peso: ").append(this.peso).append("\n");
        if(this.medicas) sb.append("Encomenda com produtos médicos.\n");
        else sb.append("A encomenda não inclui produtos medicos.\n");
        sb.append("Lista de produtos:");

        for(LinhaDeEncomenda p : this.produtos){
            sb.append(p.toString()).append(" ");
        }

        return sb.toString();
    }
    /** 
     *Metodo que coloca numa String todos os codigos presentes na Encomenda
     */ 
    public String codigos(){
        StringBuilder sb = new StringBuilder();
        sb.append("CodEncomenda: ").append(this.codEncomenda).append("\n")
            .append("CodUtilizador: ").append(this.codUtilizador).append("\n")
            .append("CodLoja: ").append(this.codLoja).append("\n");

        return sb.toString();        
    }
    /**
     * Metodo que coloca numa String algumas variaveis de instancia da Encomenda
     */ 
    public String infoEncomenda(){
        StringBuilder sb = new StringBuilder();
        sb.append("CodEncomenda: ").append(this.codEncomenda).append("\n")
            .append("CodUtilizador: ").append(this.codUtilizador).append("\n")
            .append("CodLoja: ").append(this.codLoja).append("\n")
            .append("Peso: ").append(this.peso).append("\n");

        return sb.toString();          
    }
    /**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj== null || obj.getClass() !=this.getClass()) return true;
        
        Encomenda enc = (Encomenda) obj;
        
        return(enc.getCodEncomenda().equals(this.codEncomenda) && enc.getCodUtilizador().equals(this.codUtilizador) &&
               enc.getCodLoja().equals(this.codLoja) && enc.getPeso()==this.peso && enc.getMedicas() == this.medicas && this.produtos.equals(enc.getProdutos()));
    }
     /**
     * Faz clone da classe
     * @return o clone da classe
     */
    public Encomenda clone(){
        return new Encomenda(this);
    }
    /**
     * Metodo que adiciona um objecto da classe LinhaDeEncomenda à classe Encomenda
     * @param LinhaDeEncomenda l
     */
    public void adicionaLinhaDeEncomenda(LinhaDeEncomenda l){
        this.produtos.add(l.clone());
        this.medicas = l.getMedic();
        Random r = new Random();
        this.peso += 10*r.nextDouble() + 1;
    }
     /**
     * Metodo que devolve o tamanho da variavel de instancia produtos
     * @return int correspondente
     */ 
    public int getSize(){
        return this.produtos.size();
    }
}
