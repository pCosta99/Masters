import java.io.Serializable;
import java.util.Objects;

/**
 * Classe abstrata que representa uma entidade do sistema. Todos as entidades têm parametros
 * em comum.
 */
public abstract class Entidade implements Serializable {
    private String codigo;
    private String nome;
    private GPS coordenadas;

    public Entidade(){
        this.codigo = new String();
        this.nome = new String();
        this.coordenadas = new GPS();
    }

    public Entidade(String codigo, String nome, GPS coordenadas) {
        this.codigo = codigo;
        this.nome = nome;
        this.coordenadas = coordenadas.clone();
    }

    public Entidade(Entidade e){

    }

    public String getCodigo() {
        return codigo;
    }

    public String getNome() {
        return nome;
    }

    public GPS getCoordenadas() {
        return coordenadas.clone();
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setCoordenadas(GPS coordenadas) {
        this.coordenadas = coordenadas.clone();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Entidade entidade = (Entidade) o;
        return this.codigo.equals(entidade.getCodigo()) &&
               this.nome.equals(entidade.getNome()) &&
               this.coordenadas.equals(entidade.getCoordenadas());
    }

    public abstract String toString();

    public abstract Entidade clone();

}
