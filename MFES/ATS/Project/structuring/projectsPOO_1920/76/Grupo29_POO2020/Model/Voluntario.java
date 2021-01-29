package Model;

import java.util.Map;
import java.util.Set;

import Utilities.Ponto;
import Utilities.Rating;

import java.time.LocalDateTime;

/**
 *   @class Voluntario representa as entidades voluntario na TrazAqui.
 */
public class Voluntario extends Distribuidor implements IVoluntario{

    /*
     * variaveis de classe
     */
  private static final long serialVersionUID = 134L;

  /*
   * variaveis de instancia
   */
  private boolean transportaMed;
  
  /**
   * Construtores da classe Voluntario.
   */
  
  /**
   * Construtor por omissao de Voluntario.
   */
  public Voluntario() {
    super();
    this.transportaMed = true;
  }
  
  /**
   * Construtor parametrizado de Voluntario.
   */
  public Voluntario(String id, String nome, Ponto posicao,
                    double raio, Boolean transportaMed,
                    Boolean disponivel,Rating classificacao,
                    Set<String> encomendas, double kmTotais,
                    Map<LocalDateTime, Double> registoKm) {
    super(id, nome, posicao, raio, encomendas, classificacao, kmTotais, registoKm);
    this.transportaMed = transportaMed;
  }
  
  /**
   * Construtor de copia de Voluntario.
   */
  public Voluntario(Voluntario umVoluntario) {
    super(umVoluntario);
    this.transportaMed = umVoluntario.getTransportaMed();
  }
  
  /**
   * metodos de instancia
   */
  
  //gets
  
  /**
   * Indica se o Voluntario tem condicoes para transportar medicamentos.
   */
  public boolean getTransportaMed() {
    return this.transportaMed;
  }
  
 
  //sets
  
  /**
   * Atualiza se o Voluntario tem condicoes para transportar medicamentos.
   */
  public void setTransportaMed(boolean novoTransportaMed) {
    this.transportaMed = novoTransportaMed;
  }
  
  //outros metodos obrigatorios
  
  /**
   * Metodo que devolve a representacao em String do Voluntario.
   *
   * @return String com o codigo, nome, posicao, raio, capacidade de transportar medicamentos, disponibilidade,
   * classificacao e conjunto de Encomendas entregues pelo Voluntario.
   */
  public String toString() {
    StringBuilder s=new StringBuilder();
        s.append("┏━━━━━┫ Voluntário [");
        s.append(super.getId());
        s.append("] ┣━━━━━┓\n Nome: ");
        s.append(super.getNome());
        s.append("\n Posição: ");
        s.append(super.getPosicao());
        s.append("\n Raio: ");
        s.append(super.getRaio());  
        s.append(" Km");
        s.append("\n Distância total precorrida: ");
        s.append(String.format("%.2f", super.getKmTotais()));
        s.append(" Km");
        s.append("\n Transporta Medicamentos: ");
        s.append(transportaMed);
        s.append("\n Disponível: ");
        s.append(super.getDisponivel());
        s.append("\n Classificação: ");
        s.append(super.getClassificacao());
        s.append("\n Conjuntos de Encomendas: ");
        s.append(super.getEncomendas());
        s.append("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");
    return s.toString();
  }
  
  /**
   * Metodo que verifica se o Objeto o e igual ao Voluntario para o qual a funçao e chamada
   */
  public boolean equals(Object o) {
    if (!super.equals(o))
      return false;
    if ((this.getClass() != o.getClass()))
      return false;
    Voluntario p = (Voluntario) o;
    return this.transportaMed == p.getTransportaMed();      
  }
  
  /**
   * Metodo que faz uma copia do objecto receptor da mensagem.
   */
  public Voluntario clone() {
    return new Voluntario(this);    
  } 
  
}

