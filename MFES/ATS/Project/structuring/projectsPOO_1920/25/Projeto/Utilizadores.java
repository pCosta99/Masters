    
/**
 * Escreva a descrição da classe Utilizadores aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */

import java.util.Set;
import java.util.HashSet;
import java.time.LocalDateTime;
import java.io.Serializable;
public class Utilizadores implements Login, Serializable
{
    // variáveis de instância 
    private String nome;
    private GPS localizaçao;
    private String codigo;
    private Set<Encomenda> registos;
    private String email;
    private String password;

    /**
     * Construtor por omissão da classe Utilizadores.
     */
    public Utilizadores()
    {
       this.nome = new String();
       this.codigo = new String();
       this.localizaçao = new GPS();
       this.registos = new HashSet<Encomenda>();
       this.email = new String();
       this.password = new String();
    }
    
    /**
     * Construtor parametrizado da classe Utilizadores.
     */
    public Utilizadores(String nome, String codigo, GPS localizaçao,String em, String pass, Set<Encomenda> registos)
    {
       this.setNome(nome);
       this.setCodigo(codigo);
       this.setLocalizaçao(localizaçao);
       this.setRegistos(registos);
       this.setEmail(em);
       this.setPassword(pass);
    }
    
    /**
     * Construtor de cópia da classe Utilizadores.
     */
    public Utilizadores(Utilizadores u)
    {
        this.setNome(u.getNome());
        this.setCodigo(u.getCodigo());
        this.setLocalizaçao(u.getLocalizaçao());
        this.setRegistos(u.getRegistos());
        this.setEmail(u.getEmail());
        this.setPassword(u.getPassword());
    }
    
    //getters
    public String getNome(){
        return this.nome;
    }
    
    public String getCodigo(){
        return this.codigo;
    }
    
    public GPS getLocalizaçao(){
        return this.localizaçao.clone();
    }
    
    public Set<Encomenda> getRegistos(){
        Set<Encomenda> ret = new HashSet<>();
        for(Encomenda e : this.registos){
            ret.add(e.clone());
        }
        return ret;
    }
    
    public String getEmail(){
        return this.email;
    }
    
    public String getPassword(){
        return this.password;
    }
    
    //setters
    public void setNome(String nome){
        this.nome = nome;
    }
    
    public void setCodigo(String codigo){
        this.codigo = codigo;
    }
    
    public void setLocalizaçao(GPS localizaçao){
        this.localizaçao = localizaçao.clone();
    }
    
    public void setRegistos(Set<Encomenda> registos){
        Set<Encomenda> novo = new HashSet<Encomenda>();
        for (Encomenda registo : registos){
            novo.add(registo.clone());
        }
        this.registos = novo;
    }
    
    public void setEmail(String email){
        this.email = email;
    }
    
    public void setPassword(String pass){
        this.password = pass;
    }
    
    /**
     * Método que faz uma copia do objecto receptor da mensagem.
     * Para tal invoca o construtor de copia.
     */
    public Utilizadores clone() {
        return new Utilizadores(this);
    }
    
    /**
     *  Método que devolve a representaçao em String da Utilizadores.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Nome: ").append(this.nome).append("\n");
        sb.append("Codigo: ").append(this.codigo).append("\n");
        sb.append("Localizaçao: ").append(this.localizaçao).append("\n");
        sb.append("Registos: ").append(this.registos).append("\n");
        sb.append("Email: ").append(this.email).append("\n");
        sb.append("\n");
        return sb.toString();
    }
    
    /**
     * Método que determina se dois utilizadores sao iguais.
     * 
     */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;      
        Utilizadores u = (Utilizadores) obj;
        return this.nome.equals(u.getNome()) &&
               this.codigo.equals(u.getCodigo()) &&
               this.localizaçao.equals(u.getLocalizaçao()) &&
               this.email.equals(u.getEmail()) &&
               this.password.equals(u.getPassword()) &&
               this.registos.equals(this.getRegistos());
    }
    
    /**
     * Método que adiciona um registo (quando uma encomenda é entregue ao destinatario).
     */
    public void addRegisto(Encomenda e){
        Set<Encomenda> novo = new HashSet<Encomenda>();
        novo = this.getRegistos();
        novo.add(e.clone());
        this.setRegistos(novo);
    }
    /**
     * Método que solicita a entrega de uma encomenda que foi pedida a uma loja.
     */
    public void pedeEncomenda(Lojas l, Encomenda e){
        l.adicionaFilaEspera(e.clone());   
    }
    
    /**
     * Método que retorna as encomendas recebidas num determinado periodo de tempo pelo utilizador.
     */
    public Set<String> getEncomendasTempo(LocalDateTime inicio, LocalDateTime fim){
        Set<String> set = new HashSet<String>();
        for(Encomenda e: this.getRegistos()){
            if(e.getData().isAfter(inicio) && e.getData().isBefore(fim)){
                set.add(e.getCodEncomenda());
            }
        }
        return set;    
    }

    /**
     * Método que verifica credenciais de um login de um utilizador.
     */
    public boolean verificaCredenciais(String cod, String pass){
        return (this.email.equals(cod) && this.password.equals(pass));
    }
    
    /**
     * Método que representa em ficheiro CSV um utilizador.
     */
    public String toStringCSV(){
      StringBuilder sb = new StringBuilder();
      sb.append("Utilizador:");
      sb.append(this.codigo).append(",").append(this.nome).append(",").append(this.localizaçao);
      return sb.toString();
    }
}
