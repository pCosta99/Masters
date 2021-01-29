package Model.Atores;

import java.awt.geom.Point2D;
import java.io.Serializable;
import java.util.Objects;

public abstract class Ator implements Serializable
{
    /**
     * Variaveis de instancia
     */
    private String email;
    private String referencia;
    private String nome;
    private String password;
    private Point2D.Double morada;
    private long nif;

    /**
     * Construtores
     */
    public Ator()
    {
        this.email="";
        this.referencia = "";
        this.nome="";
        this.password="";
        this.morada= null;
        this.nif=0;
    }

    public Ator(String email, String referencia,String nome,String password,Point2D.Double morada,long nif)
    {
        this.email=email;
        this.referencia = referencia;
        this.nome=nome;
        this.password=password;
        this.morada=morada;
        this.nif=nif;
    }

    public Ator(Ator a)
    {
        this.email=a.getEmail();
        this.referencia = a.getReferencia();
        this.nome=a.getNome();
        this.password=a.getPassword();
        this.morada=a.getMorada();
        this.nif=a.getNif();
    }
    /**
     * Getters
     */
    public String getEmail()
    {
        return this.email;
    }

    public String getReferencia() {
        return referencia;
    }

    public String getNome()
    {
        return this.nome;
    }
    public String getPassword()
    {
        return this.password;
    }
    public Point2D.Double getMorada(){return this.morada;}
    public long getNif ()
    {
        return this.nif;
    }

    /**
     * Setters
     */
    public void setEmail(String email)
    {
        this.email=email;
    }

    public void setReferencia(String referencia) {
        this.referencia = referencia;
    }

    public void setNome(String nome)
    {
        this.nome=nome;
    }
    public void setPassword(String password)
    {
        this.password=password;
    }

    public void setMorada(Point2D.Double morada)
    {
        this.morada=morada;
    }
    public void setNif(long nif)
    {
        this.nif=nif;
    }



    public abstract Ator clone();

    @Override
    public String toString() {
        return
                "email='" + email + '\'' +
                ", referencia='" + referencia + '\'' +
                ", nome='" + nome + '\'' +
                ", password='" + password + '\'' +
                ", morada=" + morada
                ;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Ator)) return false;
        Ator ator = (Ator) o;
        return getNif() == ator.getNif() &&
                Objects.equals(getEmail(), ator.getEmail()) &&
                Objects.equals(getReferencia(), ator.getReferencia()) &&
                Objects.equals(getNome(), ator.getNome()) &&
                Objects.equals(getPassword(), ator.getPassword()) &&
                Objects.equals(getMorada(), ator.getMorada());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getEmail(), getReferencia(), getNome(), getPassword(), getMorada(), getNif());
    }
}