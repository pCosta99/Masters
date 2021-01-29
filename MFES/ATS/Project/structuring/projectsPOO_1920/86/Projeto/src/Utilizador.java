import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que representa um Utilizador e que contem a lista de encomendas realizadas pelo utilizador.
 */
public class Utilizador extends Entidade implements Serializable {

    public List<Encomenda> encomendas;

    public Utilizador() {
        super();
        this.encomendas = new ArrayList<>();
    }

    public Utilizador(String codUtilizador, String nome, GPS gps,List<Encomenda> enc) {
        super(codUtilizador,nome,gps);
        this.setEncomendas(enc);
    }

    public Utilizador(Utilizador o){
        super(o.getCodigo(),o.getNome(),o.getCoordenadas());
        this.setEncomendas(o.getEncomendas());
    }

    public List<Encomenda> getEncomendas() {
        return this.encomendas.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    public void setEncomendas(List<Encomenda> encomendas) {
        this.encomendas = encomendas.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Utilizador that = (Utilizador) o;
        return encomendas.equals(that.encomendas);
    }


    public String toString() {
        final StringBuilder sb = new StringBuilder("Utilizador:");
        sb.append(getCodigo()).append(",");
        sb.append(getNome()).append(",");
        sb.append(getCoordenadas().toString()).append(",");
        sb.append(getEncomendas().toString());
        return sb.toString();
    }

    /**
     * Método que determina um clone do Utilizador.
     * @return Clone do Utilizador.
     */
    public Utilizador clone(){
        return new Utilizador(this);
    }

    /**
     * Método que adiciona uma encomenda ao historico.
     * @param e Encomenda a adicionar ao histórico.
     */
    public void addToHistorico(Encomenda e){
        this.encomendas.add(e);
    }

    /**
     * Método que determina a lista de encomendas do histórico num certo intervalo de tempo.
     * @param date Data que vamos usar para filtrar as encomendas.
     * @param date2 Data que vamos usar para filtrar as encomendas.
     * @return Lista de encomendas do histórico num certo intervalo de tempo.
     */
    public List<Encomenda> historicoData(LocalDateTime date, LocalDateTime date2){
        return this.encomendas.stream().filter(a->a.getEntrega().isAfter(date) && a.getEntrega().isBefore(date2)).collect(Collectors.toList());
    }

    /**
     * Método que determina a lista de códigos das Encomendas pertencentes no histórico.
     * @return Lista de códigos das Encomendas pertencentes no histórico.
     */
    public List<String> codHistorico(){
        if(this.encomendas.size() == 0) return null;
        return this.encomendas.stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList());
    }

}
