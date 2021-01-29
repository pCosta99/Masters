import java.io.Serializable;
import java.util.Objects;

public abstract class Empresa implements Serializable {
    private String codigo;
    private String nome;
    private double gpsx;
    private double gpsy;
    private String email;
    private String password;

    public Empresa(){
        this.codigo = "";
        this.nome = "";
        this.gpsx = 0;
        this.gpsy = 0;
        this.email = "";
        this.password = "";
    }

    public Empresa(String codigo, String nome, double gpsx, double gpsy, String email, String password){
        this.codigo = codigo;
        this.nome = nome;
        this.gpsx = gpsx;
        this.gpsy = gpsy;
        this.email = email;
        this.password = password;
    }

    public Empresa(Empresa e){
        this.codigo = e.getCodigo();
        this.nome = e.getNome();
        this.gpsx = e.getGpsx();
        this.gpsy = e.getGpsy();
        this.email = e.getEmail();
        this.password = e.getPassword();
    }

    public String getCodigo() {
        return this.codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public String getNome() {
        return this.nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getGpsx() {
        return this.gpsx;
    }

    public void setGpsx(double gpsx) {
        this.gpsx = gpsx;
    }

    public double getGpsy() {
        return this.gpsy;
    }

    public void setGpsy(double gpsy) {
        this.gpsy = gpsy;
    }

    public String getEmail() {
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

    public abstract Empresa clone();

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
        Empresa empresa = (Empresa) o;
        return Double.compare(empresa.gpsx, gpsx) == 0 &&
                Double.compare(empresa.gpsy, gpsy) == 0 &&
                Objects.equals(codigo, empresa.codigo) &&
                Objects.equals(nome, empresa.nome) &&
                Objects.equals(email, empresa.email) &&
                Objects.equals(password, empresa.password);
    }

    @Override
    public int hashCode() {
        return Objects.hash(codigo, nome, gpsx, gpsy, email, password);
    }
}
