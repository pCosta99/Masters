import java.io.Serializable;
import java.util.Objects;

public abstract class Pessoa implements Serializable {
    private String codigo;
    private String nome;
    private double gpsx;
    private double gpsy;
    private String email;
    private String password;

    public Pessoa(){
        this.codigo = "";
        this.nome = "";
        this.gpsx = 0;
        this.gpsy = 0;
        this.email = "";
        this.password = "";
    }

    public Pessoa(String codigo,
                  String nome,
                  double gpsx,
                  double gpsy,
                  String email,
                  String password){
        this.codigo = codigo;
        this.nome = nome;
        this.gpsx = gpsx;
        this.gpsy = gpsy;
        this.email = email;
        this.password = password;
    }

    public Pessoa(Pessoa p){
        this.codigo = p.getCodigo();
        this.nome = p.getNome();
        this.gpsx = p.getGpsx();
        this.gpsy = p.getGpsy();
        this.email = p.getEmail();
        this.password = p.getPassword();
    }

    public String getCodigo() {
        return this.codigo;
    }

    public void setCodigo(String codVoluntario) {
        this.codigo = codVoluntario;
    }

    public String getNome() {
        return this.nome;
    }

    public void setNome(String nomeVoluntario) {
        this.nome = nomeVoluntario;
    }

    public double getGpsx() {
        return this.gpsx;
    }

    public double getGpsy() {
        return this.gpsy;
    }

    public void setGpsx(double gpsx) {
        this.gpsx = gpsx;
    }

    public void setGpsy(double gpsy) {
        this.gpsy = gpsy;
    }

    public String getEmail(){
        return this.email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPassword() {
        return this.password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public abstract Pessoa clone();

    @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(" Nome: '").append(nome).append('\'').
                    append(", GpsX: ").append(gpsx).
                    append(", GpsY: ").append(gpsy).
                    append(", Email: '").append(email).append('\'').
                    append(", Password: '").append(password).append('\'');
            return  sb.toString();
        }



    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Pessoa pessoa = (Pessoa) o;
        return Double.compare(pessoa.gpsx, gpsx) == 0 &&
                Double.compare(pessoa.gpsy, gpsy) == 0 &&
                Objects.equals(codigo, pessoa.codigo) &&
                Objects.equals(nome, pessoa.nome) &&
                Objects.equals(email, pessoa.email) &&
                Objects.equals(password, pessoa.password);
    }

    @Override
    public int hashCode() {
        return Objects.hash(codigo, nome, gpsx, gpsy, email, password);
    }
}
