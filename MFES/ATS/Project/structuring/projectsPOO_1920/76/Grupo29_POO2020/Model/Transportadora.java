package Model;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import Utilities.Ponto;
import Utilities.Rating;
/**
*   @class Transportadora define uma transportadora registada na classe @class TrazAqui. 
*/
public class Transportadora extends Distribuidor implements ITransportadora {

    /*
     * variaveis de classe
     */
    private static final long serialVersionUID = 132L;

    /*
     * variaveis de instancia
     */
    private String nif;
    private BigDecimal ppkm;
    private Set<String> rejeitadas;

    /*
    *   Contrutor por omissão
    */
    public Transportadora(){
        super();
        this.nif = "n/a";
        this.ppkm = BigDecimal.ZERO;
        this.rejeitadas = new HashSet<>();
    }

    /*
    *  Construtor paramentrizado
    */
    public Transportadora(String id, String nome, String nif, Ponto local, double raio, BigDecimal ppkm, Set<String> encs, Rating r, double kmTotais, Map<LocalDateTime, Double> registoKm){
        super(id, nome, local, raio, encs, r, kmTotais, registoKm); 
        this.nif = nif;
        this.ppkm = ppkm;
        this.rejeitadas = new HashSet<>();
    }

    /*
     *  Construtor de copia
     */
    public Transportadora(Transportadora t){
        super(t);
        this.nif = t.getNif();
        this.ppkm = t.getPpkm();
        this.rejeitadas = t.getRejeitadas();
    }

    /*
    *  Define @param nif como o nif de uma @class Transportadora.
    */
    public void setNif(String nif){
        this.nif = nif;
    }

    /*
    *  Define @param ppkm como o ppkm de uma @class Transportadora.
    */
    public void setPpkm(BigDecimal ppkm){
        this.ppkm = ppkm;
    }

    /*
    * @return o nif de uma @class Transportadora.
    */
    public String getNif(){
        return this.nif;
    }

    /*
    * @return o ppkm de uma @class Transportadora.
    */
    public BigDecimal getPpkm(){
        return this.ppkm;
    }

    public Set<String> getRejeitadas(){
        return new HashSet<>(this.rejeitadas);
    }

    /*
    * Torna uma @class Transportadora numa @class String. 
    */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\n┏━━━━━┫ Transportadora [");
        sb.append(super.getId());
        sb.append("] ┣━━━━━┓\n");
        sb.append(" Nome: ");
        sb.append(super.getNome());
        sb.append("\n NIF: ");
        sb.append(this.nif);
        sb.append("\n Posição: ");
        sb.append(super.getPosicao());
        sb.append("\n Raio: ");
        sb.append(String.format("%.2f", super.getRaio()));
        sb.append(" Km");
        sb.append("\n Distância total precorrida: ");
        sb.append(String.format("%.2f", super.getKmTotais()));
        sb.append(" Km");
        sb.append("\n Preço: ");
        sb.append(String.format("%.2f", this.ppkm));
        sb.append(" €/Km");
        sb.append("\n Classificação: ");
        sb.append(super.getClassificacao());
        sb.append("\n Encomendas: ");
        sb.append(super.getEncomendas());
        sb.append("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        return sb.toString();
    }

    /*
    *  Clona uma @class Transportadora.
    */
    public Transportadora clone(){
        return new Transportadora(this);
    }

    /*
    *  Determina se a @class Transportadora é igual ao @param obj da classe @class Object genérica.
    */
    public boolean equals(Object obj){
        if(!super.equals(obj)) return false;
        if(this.getClass() != obj.getClass()) return false;
        Transportadora t = (Transportadora) obj;
        return  t.getNif().equals(this.nif) &&
                t.getPpkm() == this.ppkm &&
                t.getRejeitadas().equals(this.rejeitadas);
    }

    public void removeRejeitada(String enc){
        this.rejeitadas.remove(enc);
    }

    public void adicionaRejeitada(String enc){
        this.rejeitadas.add(enc);
    }

    public boolean existeRejeitada(String enc){
       return this.rejeitadas.contains(enc);
    }
}