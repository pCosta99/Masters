import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Classificacoes implements Serializable {

    private List<Integer> classificacoes;

    public Classificacoes(){
        this.classificacoes = new ArrayList<>();
    }

    public Classificacoes(List<Integer> classificacoes) {
        setClassificacoes(classificacoes);
    }

    public Classificacoes(Classificacoes c){
        setClassificacoes(c.getClassificacoes());
    }

    public List<Integer> getClassificacoes() {
        return new ArrayList<>(classificacoes);
    }

    public void setClassificacoes(List<Integer> classificacoes) {
        this.classificacoes = new ArrayList<>(classificacoes);
    }

    /**
     * Adicionar classificação à lista
     * @param classificacao Classificação
     */
    public void adicionaClassificacao(int classificacao){
        this.classificacoes.add(classificacao);
    }

    /**
     * Determina a classificação média da lista de classificações
     * @return Classificação média
     */
    public double classificacaoMedia(){
        return this.classificacoes.stream().mapToDouble(Integer::doubleValue).average().orElse(0.0);

    }
    public Classificacoes clone(){
        return new Classificacoes(this);
    }
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Classificacoes that = (Classificacoes) o;
        return classificacoes.equals(that.classificacoes);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Classificaçoes: ").append(this.classificacoes.toString());
        return sb.toString();
    }
}