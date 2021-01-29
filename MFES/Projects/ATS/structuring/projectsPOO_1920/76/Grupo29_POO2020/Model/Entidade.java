package Model;

import java.io.Serializable;

import Utilities.Ponto;

/** Classe abstrata que representa uma entidade. */
public abstract class Entidade implements Serializable{
  private static final long serialVersionUID = 127L;
    private String id;
    private String nome;
    private Ponto posicao;

   /**
   * Construtores da classe Entidade.
   */
  
  /**
   * Construtor por omissao de Entidade.
   */
  public Entidade() {
    this.id = "";
    this.nome = "";
    this.posicao = new Ponto();
  }
  
  /**
   * Construtor parametrizado de Entidade.
   */
  public Entidade(String id, String nome, Ponto posicao) {
    this.id = id;
    this.nome = nome;
    this.posicao = posicao.clone();
  }
  
  /**
   * Construtor de copia de Entidade.
   */
  public Entidade(Entidade umaEntidade) {
    this.id = umaEntidade.getId();
    this.nome = umaEntidade.getNome();
    this.posicao = umaEntidade.getPosicao();
  }
  
  /**
   * metodos de instancia
   */
  
  //gets
  
  /**
   * Devolve o id do Entidade.
   * 
   * @return String com o id do Entidade.
   */
  public String getId() {
    return this.id;
  }
  
  /**
   * Devolve o nome do Entidade.
   * 
   * @return String com o nome do Entidade.
   */
  public String getNome() {
    return this.nome;
  }
  
  /**
   * Devolve a posiçao do Entidade.
   * 
   * @return Ponto correspondente as coordenadas GPS posiçao do Entidade.
   */
  public Ponto getPosicao() {
    return this.posicao.clone();
  }
  
  //sets
  
  /**
   * Actualiza o id do Entidade.
   * 
   * @param novoId novo id deste Entidade
   */
    public void setId(String novoId){
        this.id = novoId;
    }
  
  /**
   * Actualiza o nome do Entidade.
   * 
   * @param novoNome novo nome deste Entidade
   */
  public void setNome(String novoNome) {
    this.nome = novoNome;
  }
  
  /**
   * Actualiza a posicao do Entidade.
   * 
   * @param novaPosicao novo posicao deste Entidade
   */
  public void setPosicao(Ponto novaPosicao) {
    this.posicao = novaPosicao.clone();
  }
  

  //outros metodos obrigatorios
  
  /**
   * Metodo que devolve a representacao em String do Entidade.
   * @return String com o id, nome e posicao.
   */
  public abstract String toString();
  
  /**
   * Metodo que verifica se o Objeto o e igual ao Entidade para o qual a funçao e chamada
   */
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if ((o == null) || (this.getClass() != o.getClass()))
      return false;
    Entidade p = (Entidade) o;
    return (   this.id.equals(p.getId()) 
            && this.nome.equals(p.getNome())
            && this.posicao.equals(p.getPosicao()));  
  }
  
  /**
   * Metodo que faz uma copia do objecto receptor da mensagem.
   */
  
  public abstract Entidade clone(); 

   /**
   * Metodo que desloca a posicao de uma Entidade somando um delta as coordenadas em x e y.
   * 
   * @param deltaX valor de deslocamento do x
   * @param deltaY valor de deslocamento do y
   */
  public void deslocamento(double deltaX, double deltaY) {
    this.posicao.deslocamento(deltaX,deltaY);
  }
  

}