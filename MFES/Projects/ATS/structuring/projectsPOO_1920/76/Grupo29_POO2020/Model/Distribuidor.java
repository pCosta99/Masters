package Model;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import Exceptions.InvalidInputException;
import Utilities.Ponto;
import Utilities.Rating;

import java.time.LocalDateTime;

/**
 *   @AbstractClass Distribuidor representa as entidades distribuidoras na TrazAqui.
 */
public abstract class Distribuidor extends Entidade{

    /*
     * variaveis de classe
     */
    private static final long serialVersionUID = 125L;

    /*
     * variaveis de instancia
     */
    private double raio;
    private double kmTotais;
    private Rating classificacao;
    private Set<String> encomendas; 
    private Map<LocalDateTime, Double> registoKm;
    private boolean disponivel;

    /*
     *   Contrutor vazio cria uma Distribuidor sem código, nome, nif, com local, raio, ppkm e classificacao a 0.
     */
    public Distribuidor(){
        super();
        this.raio = 0;
        this.encomendas = new HashSet<>();
        this.classificacao = new Rating();
        this.kmTotais = 0;
        this.registoKm = new HashMap<>();
        disponivel = true;
    }

    /*
    *  Simplesmente insere os parâmetros nas instância respetivas.
    */
    public Distribuidor(String id, String nome, Ponto pos, double raio, Set<String> encs, Rating r, double kmTotais, Map<LocalDateTime, Double> registoKm){
        super(id, nome, pos);
        this.raio = raio;
        this.setEncomendas(encs);
        this.classificacao = r.clone();
        this.kmTotais = kmTotais;
        this.setRegistoKm(registoKm);
        disponivel = true;
    }

    /*
    *  Simplesmente insere os parâmetros nas instância respetivas.
    */
    public Distribuidor(Distribuidor d){
        super(d);
        this.raio = d.getRaio();
        this.encomendas = d.getEncomendas();
        this.classificacao = d.getClassificacao();
        this.kmTotais = d.getKmTotais();
        this.registoKm = d.getRegistoKm();
        this.disponivel = d.getDisponivel();
    }
   
    /*
    *  Define @param raio como o raio de uma @class Distribuidor.
    */
    public void setRaio(double raio){
        this.raio = raio;
    }

    /*
    *  Define @param encs como as encomendas de uma @class Distribuidor.
    */
    public void setEncomendas(Set<String> encs){
        this.encomendas = new HashSet<>();
        for(String e : encs)
            this.encomendas.add(e);
    }

    /*
    *  Define @param classificacao como a classificação de uma @class Distribuidor.
    */
    public void setClassificacao(Rating r){
        this.classificacao = r.clone();
    }

    /*
    *  Define @param kmTotais como os km totais efetuados de uma @class Distribuidor.
    */
    public void setKmTotais(double kmTotais){
        this.kmTotais = kmTotais;
    }

    /*
    *  Define @param registoKm como o registo de kms feitos por data de uma @class Distribuidor.
    */
    public void setRegistoKm(Map<LocalDateTime, Double> registoKm){
        this.registoKm = registoKm
                         .entrySet()
                         .stream()
                         .collect(Collectors.toMap(Map.Entry :: getKey, 
                                                   Map.Entry :: getValue,
                                                   (prev, next) -> next,
                                                   HashMap :: new));
    }

    public void setDisponivel(boolean estado){
        this.disponivel = estado;
    }


    /*
    * @return o raio de uma @class Distribuidor.
    */
    public double getRaio(){
        return this.raio;
    }

    /**
     * Devolve o conjunto de encomendas entregues pelo Voluntario.
     * 
     * @return o conjunto de encomendas entregues pelo Voluntario.
     */
    public Set<String> getEncomendas() {
        return new HashSet<>(this.encomendas);
    }

    /*
    * @return a classificação de uma @class Distribuidor.
    */
    public Rating getClassificacao(){
        return this.classificacao.clone();
    }

    /*
    * @return os km totais precorridos por uma @class Distribuidor.
    */
    public double getKmTotais(){
        return this.kmTotais;
    }

    /*
    * @return o registo de km precorridos por data de uma @class Distribuidor.
    */
    public Map<LocalDateTime, Double> getRegistoKm(){
        return this.registoKm
        .entrySet()
        .stream()
        .collect(Collectors.toMap(Map.Entry :: getKey, 
                                  Map.Entry :: getValue,
                                  (prev, next) -> next,
                                  HashMap :: new));
    }

    /*
     * Metodo que devolve a dispovibilidade de um distribuidor para entregar encomendas
     */
    public boolean getDisponivel(){
        return this.disponivel;
    }

    /*
    * Torna uma @class Distribuidor numa @class String. 
    */
    public abstract String toString();

    /*
    *  Clona uma @class Distribuidor.
    */
    public abstract Distribuidor clone();

    /*
    *  Determina se a @class Distribuidor é igual ao @param obj da classe @class Object genérica.
    */
    public boolean equals(Object obj){
        if(!super.equals(obj)) return false;
        if(this.getClass() != obj.getClass()) return false;
        Distribuidor d = (Distribuidor) obj;
        return d.getRaio() == this.raio 
               && this.encomendas.equals(d.getEncomendas()) 
               && d.getClassificacao().equals(this.classificacao)
               && d.getRegistoKm().equals(this.registoKm);
    }

    /**
   * Metodo que atualiza a classificaçao que e atribuida ao Distribuidor pelos Utilizadores.
   * 
   * @param novaClassificacao classificaçao atribuida ao Distribuidor que ainda nao participa da media das classificaçoes deste
   */
  public void atualizaClassificacao(String utilizador, int novaClassificacao) throws InvalidInputException{
    this.classificacao.addRate(utilizador, novaClassificacao);
 }

  /**
   * Metodo que adiciona uma encomenda ao conjunto de Encomendas entregues pelo Distribuidor.
   * 
   * @param novaEncomenda encomenda que o Distribuidor aceitou entregar (e que por isso +assa a pertencer ao seu conjunto de encomendas)
   */
  public void atualizaEncomendas(String novaEncomenda){
    this.encomendas.add(novaEncomenda);
  }

  /**
   * Metodo que adiciona um percurso a um distribuidor
   **/
  public void fezKm(LocalDateTime ldt, double km) throws InvalidInputException{
      if(km <= 0) throw new InvalidInputException("\'" + km + "\' km não é um valor válido.");
      this.kmTotais += km;
      this.registoKm.put(ldt, km);
      this.disponivel = true;
  }

    /**
     * Metodo que atualiza a disponibilidade de um distribuidor
     **/
    public void inverteDisponivel(){
      this.disponivel = !this.disponivel;
  } 

    public boolean temEncomenda(String enc){
        return this.encomendas.contains(enc);
    }

}