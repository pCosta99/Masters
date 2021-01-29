package Model;

import Exceptions.InvalidInputException;
import Utilities.Ponto;

/**
 *   @class Utilizador define um Utilizador registado na classe @class TrazAqui.
 */
public class Utilizador extends Entidade implements IUtilizador {

  /*
   * variaveis de instancia
   */
  private int nEnc;
  private static final long serialVersionUID = 133L;
  
  /**
   * Construtores da classe Utilizador.
   */
  
  /**
   * Construtor por omissao de Utilizador.
   */
  public Utilizador() {
    super();
    this.nEnc = 0;
  }
  
  /**
   * Construtor parametrizado de Utilizador.
   */
  public Utilizador(String id, String nome, Ponto posicao, int nEnc) {
    super(id, nome, posicao);
    this.nEnc = nEnc;
  }
  
  /**
   * Construtor de copia de Utilizador.
   */
  public Utilizador(Utilizador umUtilizador) {
    super.setId(umUtilizador.getId());
    super.setNome(umUtilizador.getNome());
    super.setPosicao(umUtilizador.getPosicao());
    this.nEnc = umUtilizador.getnEnc();
  }
  
  /**
   * metodos de instancia
   */
  
  //gets
  
  /**
   * Devolve o numero de encomedas realizadas pelo Utilizador.
   * 
   * @return int correspondente ao numero de encomedas realizadas pelo Utilizador.
   */
  public int getnEnc() {
    return this.nEnc;
  }
  
  //sets
 
  /**
   * Actualiza o numero de encomedas realizadas pelo Utilizador.
   * 
   * @param novonEnc novo numero de encomedas realizadas por este Utilizador
   */
  public void setnEnc(int novonEnc) throws InvalidInputException{
    if(novonEnc < 0) throw new InvalidInputException("\'" + novonEnc + "\' não é um número de encomendas válido.");
    this.nEnc = novonEnc;
  }
  
  //outros metodos obrigatorios
  
  /**
   * Metodo que devolve a representacao em String do Utilizador.
   * @return String com o codigo, nome e posicao.
   */
  public String toString() {
    int i;
    StringBuilder sCod = new StringBuilder(), 
                  sNome = new StringBuilder(),
                  sPosicao = new StringBuilder(),
                  sNEnc = new StringBuilder(),
                  res = new StringBuilder();
    sCod.append("┫ Utilizador [").append(getId()).append("] ┣");
    sNome.append("┃ Nome: ").append(getNome());
    sPosicao.append("┃ Posição: ").append(getPosicao());
    sNEnc.append("┃ Número de Encomendas realizadas: ").append(nEnc);
    
    /*Box drawing with ID */
    int l = Math.max(sCod.length(), Math.max(sNome.length(), Math.max(sPosicao.length(), sNEnc.length()))) + 4;
        l += (l - sCod.length()) % 2 == 0 ? 2 : 3;
    for(i = (l - sCod.length())/2 + 1; i > 0; i--)
      res.append(" ");
    res.append("┏");
    for(i = sCod.length() - 2; i > 0; i--)
      res.append("━");
    res.append("┓\n┏");
    for(i = (l - sCod.length())/2; i > 0; i--)
      res.append("━");
    res.append(sCod);
    for(i = (l - sCod.length())/2; i > 0; i--)
      res.append("━");
    res.append("┓\n┃");
    for(i = (l - sCod.length())/2; i > 0; i--)
      res.append(" ");
    res.append("┗");
    for(i = sCod.length() - 2; i > 0; i--)
      res.append("━");
    res.append("┛");
    for(i = (l - sCod.length())/2; i > 0; i--)
      res.append(" ");
    res.append("┃\n");

    /* True content*/
    res.append(sNome);
    for(i = 0; i < l - sNome.length() + 1; i++)
      res.append(" ");
    res.append("┃\n").append(sPosicao);
    for(i = 0; i < l - sPosicao.length() + 1; i++)
      res.append(" ");
    res.append("┃\n").append(sNEnc);
    for(i = 0; i < l - sNEnc.length() + 1; i++)
      res.append(" ");
    res.append("┃\n").append("┗");
    for(i = 0; i < l; i++)
      res.append("━");
    res.append("┛");

    return res.toString();
  }
  
  /**
   * Metodo que verifica se o Objeto o e igual ao Utilizador para o qual a funçao e chamada
   */
  public boolean equals(Object o) {
    if (! super.equals(o))
      return false;
    if ((this.getClass() != o.getClass()))
      return false;
    Utilizador p = (Utilizador) o;
    return (  super.getId().equals(p.getId()) 
            && super.getNome().equals(p.getNome())
            && super.getPosicao().equals(p.getPosicao()) 
            && this.nEnc == p.getnEnc() );      
  }
  
  /**
   * Metodo que faz uma copia do objecto receptor da mensagem.
   */
  
  public Utilizador clone() {
    return new Utilizador(this);    
  }

  /**
   * Metodo que determina se dois pontos sao iguais.
   * @return booleano que e verdadeiro se os valores das duas coordenadas forem iguais
   */
  /*public boolean iguais(Utilizador umUtilizador) {
    return (this.codigo.equals(umUtilizador.getCodigo())
            && this.nome.equals(umUtilizador.getNome())
            && this.posicao.equals(umUtilizador.getPosicao())
            && this.nEnc == umUtilizador.getnEnc() );
  }*/

  /**
   * Metodo que incrementa o contador do número de encomendas feitas por um utilizador
   */
  public void incNEnc(){
    this.nEnc++;
  }
}
