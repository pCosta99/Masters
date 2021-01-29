import java.io.Serializable;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;


public class Utilizador extends AppUser implements Serializable, Comparable<Utilizador> {
  private String codigo;
  private String nome;
  private Coordenadas coordenadas;
  private Set<EncomendaEntregue> historico;

  /**
   * Construtor parametrizado da classe Utilizador.
   */
  public Utilizador(Sistema sistema, String codigo, String nome, String username, String password, Coordenadas coord) {
    super(sistema, username, password);
    this.codigo = codigo;
    this.nome = nome;
    this.coordenadas = coord;
    this.historico = new TreeSet<>();
  }



  /**
   * Construtor parametrizado da classe Utilizador.
   */
  public Utilizador(Sistema sistema, String codigo, String nome, String username, String password, Coordenadas coord, Set<EncomendaEntregue> historico) {
    super(sistema, username, password);
    this.codigo = codigo;
    this.nome = nome;
    this.coordenadas = coord;
    this.setHistorico(historico);
  }

  /**
   * Construtor de cópia da classe Utilizador.
   * @param o um utilizador
   */
  public Utilizador(Utilizador o) {
    super(o);
    this.codigo = o.getCodigo();
    this.nome = o.getNome();
    this.coordenadas = o.getCoordenadas();
    this.historico = o.getHistorico();
  }


  // Getters

  /**
   * Retorna o código de do Utilizador.
   * @return código do utilizador
   */
  public String getCodigo() {
    return codigo;
  }

  /**
   * Retorna o nome do Utilizador.
   * @return nome do utilizador
   */
  public String getNome(){ return this.nome; }

  /**
   * Retorna as coordenadas do utilizador.
   * @return coordenadas do utilizador
   */
  public Coordenadas getCoordenadas() {
    return coordenadas;
  }

  /**
   * Retorna o histórico d eencomendas solicitadas pelo utilizador.
   * @return
   */
  public Set<EncomendaEntregue> getHistorico() {
    Set<EncomendaEntregue> res = new TreeSet<>();
    Iterator<EncomendaEntregue> it = this.historico.iterator();
    EncomendaEntregue e;

    while (it.hasNext()) {
      e = it.next();
      res.add(e.clone());
    }
    return res;
  }


  // Setters

  /**
   * Define o código de um Utilizador.
   * @param codigo codigo de um utilizador
   */
  public void setCodigo(String codigo) {
    this.codigo = codigo;
  }

  /**
   * Define o nome de um Utilizador.
   * @param nome
   */
  public void setNome(String nome) { this.nome=nome; }

  /**
   * Define as coordenadas de um Utilizador.
   * @param coordenadas coordenadas de um utilizador
   */
  public void setCoordenadas(Coordenadas coordenadas) {
    this.coordenadas = coordenadas;
  }


  /**
   * Define o histórico de encomendas solicitadas por um Utilizador.
   * @param historico historico de encomendas
   */
  public void setHistorico(Set<EncomendaEntregue> historico) {
    this.historico = historico.stream().map(EncomendaEntregue::clone).collect(Collectors.toSet());
  }

  public int numeroEncomendas () {
    return this.historico.size();
  }


  public void solicitarEntrega(Loja loja, Set<LinhaEncomenda> prods, Sistema sistema) {
    sistema.novaEncomenda(this.codigo, loja.getCodLoja(), prods);
  }

  public boolean aceitaEntrega(Encomenda encomenda, Transportadora t) {
    if (encomenda.estaAceite()) return true;
    String input;
    ViewGeral.PrintMensagem("Aceita que a sua encomenda seja transportada pela transportadora " + t.getCodigo() +
                             "?\n O preço será " + t.precoTransporte(encomenda));
    ViewGeral.PrintMensagem("          's' para aceitar e 'n' para rejeitar");

    input = Input.lerString();
    while(input.compareTo("s")!=0 && input.compareTo("n")!=0) {
      ViewGeral.PrintMensagem("\n\n INPUT INVÁLIDO. Tente novamente: ");
      input = Input.lerString();
    }

    if (input.compareTo("s") == 0) return true;
    return false;
  }

  public void atualizaHistorico(EncomendaEntregue enc) {
    this.historico.add(enc);
  }

  public double getTempoUltimaEncomenda() {
    EncomendaEntregue ultimaEncomenda = this.historico.stream().sorted().findFirst().get();
    if (ultimaEncomenda == null) return -1;
    return ultimaEncomenda.tempoEntrega();
  }


  public boolean equals(Object o) {
    boolean res = super.equals(o);
    if (res) {
      Utilizador cl = (Utilizador) o;

      res = this.codigo.equals(cl.getCodigo()) &&
            this.nome.equals(cl.getNome()) &&
            this.coordenadas.equals(cl.getCoordenadas()) &&
            this.historico.equals(cl.getHistorico());
    }

    return res;
  }

  public Utilizador clone() {
    return new Utilizador(this);
  }

  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("Utilizador: ").append('\n').append("Código: ").append(this.codigo).append("Nome: ").append(this.nome).
            append("GPS: ").append(this.coordenadas.toString()).append("Histórico: ").append(this.historico.toArray().toString());

    return sb.toString();
  }

  /**
   * O primeiro critério de comparação entre dois utilizadores é o número de encomendas pedidas. Caso este número seja igual,
   * compara com base no seu código.
   * @param u um utilizador
   * @return 0 se forem iguais, -1 se o utilizador que invoca o método fôr menor e 1 se o utilizador que invoca o método fôr maior.
   */
  public int compareTo (Utilizador u) {
    if (this.historico.size() == u.numeroEncomendas()) {
      if (this.codigo.compareTo(u.getCodigo()) < 0) return -1;
      if (this.codigo.compareTo(u.getCodigo()) > 0) return 1;
      else return 0;
    }
    if (this.historico.size() < u.numeroEncomendas()) return 1;
    return -1;
  }


}
