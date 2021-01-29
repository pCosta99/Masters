package Users;

import java.io.Serializable;
import java.util.Objects;
import java.util.TreeSet;

import Base.Basic.Coordenadas;
import Base.Encomenda.Aceite;
import Base.Encomenda.Encomenda;

public class Utilizador implements Serializable{
    private String codUtilizador;
    private String nome;
    private String email;
    private String password;
    private Coordenadas gps;
    private TreeSet<Aceite> encomendasFeitas;

    public Utilizador() {
    }

    public Utilizador(String codUtilizador, String nome, String email, String password, Coordenadas gps) {
        this.codUtilizador = codUtilizador;
        this.nome = nome;
        this.email = email;
        this.password = password;
        this.gps = gps.clone();
        this.encomendasFeitas = new TreeSet<Aceite>();
    }

    public Utilizador(Utilizador x) {
        this(x.codUtilizador,x.nome,x.email,x.password,x.gps);
        this.encomendasFeitas = getEncomendasFeitas();
    }

    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getNome() {
        return this.nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
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

    public Coordenadas getGps() {
        return this.gps;
    }

    public void setGps(Coordenadas gps) {
        this.gps = gps;
    }


    public TreeSet<Aceite> getEncomendasFeitas() {
        TreeSet<Aceite> ret = new TreeSet<>();
        for (Aceite aceite : this.encomendasFeitas) {
            ret.add(aceite);
        }
        return ret;
    }

    public void setEncomendasFeitas(TreeSet<Aceite> encomendasFeitas) {
        this.encomendasFeitas.clear();
        for (Aceite aceite : encomendasFeitas) {
            this.encomendasFeitas.add(aceite);
        }
    }

    public void addEncomendasFeitas(Aceite e) {
        this.encomendasFeitas.add(e.clone());
    }

    public void addEncomendasFeitas(String codEncomenda) {
        addEncomendasFeitas(new Aceite(codEncomenda));
    }

    public void addEncomendasFeitas(Encomenda e) {
        addEncomendasFeitas(new Aceite(e));
    }


    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Utilizador)) {
            return false;
        }
        Utilizador utilizador = (Utilizador) o;
        return Objects.equals(codUtilizador, utilizador.codUtilizador) && Objects.equals(nome, utilizador.nome) && Objects.equals(email, utilizador.email) && Objects.equals(password, utilizador.password) && Objects.equals(gps, utilizador.gps);
    }

    @Override
    public int hashCode() {
        return Objects.hash(codUtilizador, nome, email, password, gps);
    }

    @Override
    public String toString() {
        return "{" +
            " codUtilizador='" + getCodUtilizador() + "'" +
            ", nome='" + getNome() + "'" +
            ", email='" + getEmail() + "'" +
            ", password='" + getPassword() + "'" +
            ", gps='" + getGps() + "'" +
            "}";
    }



    public Utilizador clone() {
        return new Utilizador(this);
    }


}