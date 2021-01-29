import java.io.Serializable;
import java.util.List;

public abstract class Entidade  implements Serializable{
    private String codigo;
    private String nome;
    private GPS localizacao;

    public Entidade(){
        this.codigo = "n/a";
        this.nome = "n/a";
        this.localizacao = new GPS();
    }

    public Entidade(String umCodigo, String umNome, GPS umaLocalizacao){
        this.codigo = umCodigo;
        this.nome = umNome;
        this.localizacao = umaLocalizacao;
    }

    public Entidade(Entidade umaEntidade) {
        this.codigo = umaEntidade.getCodigo();
        this.nome = umaEntidade.getNome();
        this.localizacao = umaEntidade.getLocalizacao();
    }

    public abstract Entidade clone();

    public String getCodigo(){
        return this.codigo;
    }

    public String getNome(){
        return this.nome;
    }

    public GPS getLocalizacao(){
        return this.localizacao;
    }

    public void setCodigo(String novoCodigo){
        this.codigo = novoCodigo;
    }

    public void setNome(String novoNome){
        this.nome = novoNome;
    }

    public void setLocalizacao(GPS novaLocalizaca){
        this.localizacao = novaLocalizaca;
    }

    public boolean equals(Object o){
        if(this == o)
            return true;
        if(o == null || this.getClass() != o.getClass())
            return false;
        Entidade e = (Entidade) o;
        return this.codigo.equals(e.getCodigo()) &&
                this.nome.equals(e.getNome()) &&
                this.localizacao.equals(e.getLocalizacao());
    }

    public String toString(){
        return this.getClass().getSimpleName() + ":{" +
                "\n \t" + "Código = " + this.codigo +
                "\n \t" + "Nome = " + this.nome +
                "\n \t" + "Localização = " + this.localizacao.toString();
    }

    public String toCSV(){
        return this.getClass().getSimpleName() + ',' +
                    this.getCodigo() + ',' +
                    this.getNome() + ',' +
                    this.getLocalizacao();
    }

    public String toLog(){
        return this.getClass().getSimpleName() + ',' +
                this.codigo + ',' + this.nome + this.localizacao.toLog();
    }
}
