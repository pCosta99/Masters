package Projeto.Entidades;

import Projeto.Interfaces.IAviso;
import Projeto.Interfaces.IEncomenda;
import Projeto.Interfaces.IEntidade;
import Projeto.Util.GPS;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Classe que implementa um Utilizador.
 * Um Utilizador é qualquer entidade que usa a APP.
 * Ou seja, neste caso, uma loja, uma empresa, um voluntario e um cliente sao todos utilizadores.
 */
public abstract class Entidade implements IEntidade, Comparable<IEntidade>, Serializable {
    private String id;
    private String password;
    private String nome;
    private GPS loc;
    private Collection<IEncomenda> encs;
    private Collection<IAviso> notificacoes;

    /*
     * Construtores da Classe Utilizador.
     * Declaracao dos construtores por omissao, parametrizado e de copia.
     */
    /**
     * Construtor por omissao de Utilizador.
     */
    public Entidade() {
        this.id = "";
        this.password = "";
        this.nome = "";
        this.loc = new GPS();
        this.encs = new ArrayList<>();
        this.notificacoes = new ArrayList<>();
    }

    /**
     * Construtor parametrizado de Utilizador.
     */
    public Entidade(String id, String password, String nome, GPS loc, Collection<IEncomenda> encs){
        this.nome = nome;
        this.password = password;
        this.id = id;
        setLocalizacao(loc);
        setEncomendas(encs);
        this.notificacoes = new ArrayList<>();
    }
     
    /**
     * Construtor por cópia de Utilizador.
     * Aceita como parametro outra Utilizador e utiliza os metodos de acesso aos valores das variaveis de instancia.
     */
    public Entidade(Entidade u) {
       this.nome = u.getNome();
       this.password = u.getPassword();
       this.id = u.getId();
       this.loc = u.getLocalizacao();
       this.encs = u.getEncomendas();
       this.notificacoes = u.getNotificacoes();
    }

    /*
     * Getters e Setters
     */
    /**
     * Metodo que retorna o nome do utilizador.
     */
    public String getNome(){
        return this.nome;
    }

    /**
     * Metodo que retorna o ID do utilizador.
     */
    public String getId(){
        return this.id;
    }

    /**
     * Método que retorna a password
     */
    public String getPassword() {
        return this.password;
    }

    /**
     * Metodo que retorna a localizacao do utilizador.
     */
    public GPS getLocalizacao(){
        return loc.clone();
    }
    
    /**
     * Metodo que retorna as encomendas do utilizador.??????????? é lista de encomendas?
     */
    public Collection<IEncomenda> getEncomendas() {
         return this.encs.stream()
                 .map(IEncomenda::clone)
                 .collect(Collectors.toList());
    }

    public Collection<IAviso> getNotificacoes() {
        return this.notificacoes.stream().map(IAviso::clone).collect(Collectors.toList());
    }
    
    /**
     * Metodo que atualiza o ID do utilizador.
     * @param novoId String do novo utilizador
     */
    public void setId(String novoId){
        this.id = novoId;
    }
    
    /**
     * Metodo que atualiza o nome do utilizador.
     * @param novoNome String do novo utilizador
     */
    public void setNome(String novoNome){
        this.nome = novoNome;
    }
       
    /**
     * Metodo que atualiza a localizaçao do utilizador.
     * @param novaLoc GPS do novo utilizador
     */
    public void setLocalizacao(GPS novaLoc){
        this.loc = novaLoc.clone();
    }
    
    /**
     * Metodo que atualiza a lista de encomendas do utilizador.
     * @param enc List<Encomenda> do novo utilizador
     */
   public void setEncomendas(Collection<IEncomenda> enc) {
       this.encs = enc.stream()
               .map(IEncomenda::clone)
               .collect(Collectors.toList());
   }

    /**
     * Altera a lista de notificações para uma nova
     * @param nots nova lista
     */
   public void setNotificacoes(Collection<IAviso> nots) {
       this.notificacoes = nots.stream().map(IAviso::clone).collect(Collectors.toList());
   }

    /**
     * Altera a password
     */
   public void setPassword(String password) {
        this.password = password;
   }

    /*
     * Restantes Metodos de Instancia
     */
    public void addNotificacao(IAviso a) {
        this.notificacoes.add(a.clone());
    }

    public void removeNotificacao(IAviso a) {
        this.notificacoes.remove(a);
    }
   
   /**
    * Metodo que adiciona uma encomenda a lista de encomendas do utilizador.
    */
   public void adicionaEnc(IEncomenda e){
        this.encs.add(e.clone());
   }
   
   /**
    * Metodo que remove uma encomenda da lista de encomendas do utilizador.
    */
   public void removeEnc(IEncomenda e){
       this.encs.remove(e); 
   }

    /**
     * Método que devolve uma encomenda da lista de encomendas do utilizador.
     * @param encomendaID - encomenda que se quer
     * @return encomenda com o id igual ao encomendaID.
     */
   public IEncomenda getEncomenda(String encomendaID) {
       Iterator<IEncomenda> it = this.encs.iterator();
       IEncomenda ret = null;
       boolean encontrado = false;
       while (it.hasNext() && !encontrado) {
           IEncomenda e = it.next();
           if(e.getID().equals(encomendaID)) {
               ret = e;
               encontrado = true;
           }
       }
       return ret;
   }

    /**
     * Metodo que ve se dois utilizadores sao iguais.
     * @return boolean verdadeiro caso o id de dois utilizadores seja igual.
    */
   public boolean equals(Object o){
      if (o == this) return true; 
      if (o == null || o.getClass() != this.getClass()) return false;
      Entidade u = (Entidade) o;
      return this.id.equals(u.getId()); 
   }
   
   /**
    * Metodo que devolve a representaçao em String do Utilizador.
    * @return String com as variaveis desta instancia.
    */
   public String toString() {
       StringBuilder sb = new StringBuilder();
       sb.append("Id: ").append(this.id)
         .append("\nNome: ").append(this.nome)
         .append("\nlocalizaçao: ").append(this.loc)
         .append("\nLinhas de Encomenda: ");
       for(IEncomenda e : this.encs) {
           sb.append(e.toString());
       }
       return sb.toString();
   }

    /**
     * Define um critério de ordenação para o Utilizador.
     * Neste caso, será por ordem alfabética (tendo em conta o id).
     */
    public int compareTo(IEntidade u) {
        return this.id.compareTo(u.getId());
    }

    /**
     * Método hashCode
     */
    @Override
    public int hashCode() {
        return Objects.hash(id, password, nome, loc, encs, notificacoes);
    }
}