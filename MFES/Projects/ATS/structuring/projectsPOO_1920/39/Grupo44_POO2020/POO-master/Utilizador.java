import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.Map;
import java.util.HashMap;
/**
 * Write a description of class Cliente here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Utilizador extends Registo
{
    /** Código do utilizador**/
    private String codUtilizador;
    /** Nome do utilizador**/
    private String nome;
    /** Localização do utilizador**/
    private Localizacao posicao;
    /** Map com chave dos códdigos das encomendas do Utilizador **/
    private Map<String,Encomenda> encomendas;
    
    /** Construtor nulo */
    public Utilizador(){
        super();
        this.codUtilizador= "";
        this.nome= "";
        Localizacao novaposicao=new Localizacao();
        this.encomendas = new HashMap<>();
    }
    
    /** Construtor parametrizado para a classe Utilizador */
    public Utilizador(String ema, String pas,String codU, String nom, Localizacao l, Map<String,Encomenda> enc){
        super(ema,pas);
        this.codUtilizador=codU;
        this.nome=nom;
        this.posicao=l.clone();
        this.encomendas = new HashMap<>();
        for(Encomenda e: enc.values()){
            this.encomendas.put(e.getEncomenda(),e.clone());
        }
    }
    /** Construtor de cópia */
    public Utilizador(Utilizador u){
        super(u);
        this.codUtilizador= u.getCodUtilizador();
        this.nome= u.getNome();
        this.posicao=u.getPosicao();
        this.encomendas=u.getEncomendas();
    }
    /** Retorna o código do Utilizador**/
    public String getCodUtilizador(){
        return this.codUtilizador;
    }
    /** Define o código do Utilizador **/
    public void setCodUtilizador(String novocod){
        this.codUtilizador= novocod;
    }
    /** Retorna o nome do Utilizador **/
    public String getNome(){
        return this.nome;
    }
    /** Define o nome do Utilizador **/
    public void setNome(String nom){
        this.nome= nom;
    }
    /** Retorna a localização do Utilizador **/
    public Localizacao getPosicao(){
        return this.posicao.clone();
    }
    /** Define a localização do Utilizador **/
    public void setPosicao(Localizacao pos){
        this.posicao=pos;
    }
    /** Retorna a lista dos códigos das encomendas do Utilizador */
    public Map<String,Encomenda> getEncomendas(){
        Map<String,Encomenda> lista = new HashMap<> ();
        for(Encomenda e: this.encomendas.values()){
            lista.put(e.getEncomenda(),e);
        }
        return lista;
    }
    /** Define a lista dos códigos das encomendas do Utilizador */
    public void setEncomendas(Map<String,Encomenda> lista){
        for(Encomenda e: lista.values()){
            this.encomendas.put(e.getEncomenda(),e.clone());
        }
    }
    /** Método que clona um Utilizador **/
    public Utilizador clone(){
        return new Utilizador(this);
    }
    /** Método que devolve um boolean true caso os 
     * Utilizadores sejam iguais e false caso não sejam **/
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        
        Utilizador u = (Utilizador) o;
        return this.codUtilizador.equals(u.getCodUtilizador()) &&
               this.nome.equals(u.getNome()) &&
               this.posicao.equals(u.getPosicao()) &&
               this.encomendas.equals(u.getEncomendas());
    }
    /** Método que cria uma string com a informação do Utilizador **/
    public String toString(){
        StringBuilder sb=new StringBuilder();
        sb.append(super.toString());
        sb.append("Código do Utilizador: ").append(this.codUtilizador+"\n");        
        sb.append("Nome: ").append(this.nome+"\n");
        sb.append("GPS: ").append(this.posicao);
        sb.append("Encomendas: ").append(this.encomendas);
        return sb.toString();
    }
    
    /** Método que verifica se o utilizador contém uma encomenda */
    public boolean contemEncomenda(String cod){
        return this.getEncomendas().containsKey(cod);
    }
}
