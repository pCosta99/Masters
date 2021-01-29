package Model;
import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

public class Utilizador extends LocalCodeName implements Serializable {

    private Map<String,Encomenda> encomendaMap;

    /**
     * Construtor da classe
     * @param codigo O código do utilizador
     * @param nome O nome do utilizador
     * @param gps O GPS do utilizador
     * @param encomendaMap As encomendas que o utilizador já recebeu
     */
    public Utilizador(String codigo, String nome, GPS gps, Map<String,Encomenda> encomendaMap) {
        super(codigo,nome,gps);
        setEncomendaMap(encomendaMap);
    }

    /**
     * Construtor da classe
     */
    public Utilizador() {
        super();
        this.encomendaMap = new TreeMap<>();
    }

    /**
     * Construtor da classe
     * @param uti O utilizador do qual se pretende extrair a informação
     */
    public Utilizador(Utilizador uti) {
        super(uti);
        setEncomendaMap(uti.getEncomendaMap());
    }

    /**
     * Indica as encomendas que o utilizador já recebeu
     * @return As encomendas que o utilizador já recebeu
     */
    public Map<String, Encomenda> getEncomendaMap() {
        if (encomendaMap == null)
            return null;
        return this.encomendaMap.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, v ->v.getValue().clone()));
    }

    /**
     * Indica o número de encomendas compradas pelo utilizador
     * @return número de encomendas
     */
    public int nEncsBought(){return this.encomendaMap.size();}

    /**
     * Define as encomendas que o utilizador já comprou
     * @param enc As encomendas que o utilizador já comprou
     */
    public void setEncomendaMap(Map<String,Encomenda> enc) {
        if(enc != null)
            this.encomendaMap = enc.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, v ->v.getValue().clone()));
        else
            this.encomendaMap = null;
    }

    /**
     * Verifica se um objeto é igual a um utilizador
     * @param o O objeto com o qual se pretende comparar
     * @return True caso afirmativo e false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Utilizador that = (Utilizador) o;
        return  this.getCode().equals(that.getCode());
    }

    /**
     * Cria um clone do utilizador
     * @return O clone do utilizador
     */
    public Utilizador clone() {
        return new Utilizador(this);
    }

    /**
     * Altera a morada do utilizador para coordenadas aleatórias
     */
    public void alterarMorada() {
        getGps().randomGPS();
    }

    /**
     * Adiciona uma encomenda às encomendas já compradas pelo utilizador
     * @param e A encomenda que se pretende adicionar
     */
    public void addEnc(Encomenda e){this.encomendaMap.put(e.getNumEnc(),e.clone());}

    /**
     * Indica a encomenda associada a um dado código que está presente na lista das encomendas compradas pelo utilizador
     * @param e O código da encomenda que se pretende obter
     * @return A encomenda correspondente ao código
     */
    public Encomenda getEnc(String e){
        return this.encomendaMap.get(e).clone();
    }

    /**
     * Transforma um utilizador numa String
     * @return A String correspondente ao utilizador
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Nome: ").append(this.getNome()).append("\n")
                .append("Código: ").append(this.getCode()).append("\n")
                .append(this.getGps()).append("\n")
                .append("Encomendas feitas: ").append(encomendaMap.size());

        return sb.toString();
    }
}
