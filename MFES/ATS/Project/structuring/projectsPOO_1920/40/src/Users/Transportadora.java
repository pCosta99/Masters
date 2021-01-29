package Users;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.TreeSet;

import Base.Basic.Coordenadas;
import Base.Encomenda.Aceite;
import Base.Encomenda.Encomenda;

public class Transportadora implements Serializable{
    private String codEmpresa;
    private String password;
    private String nomeEmpresa;
    private Coordenadas gps;
    private int nif;
    private double raio;
    private double priceKm;

    private boolean on;
    private double priceKg;
    private TreeSet<Aceite> encomendasFeitas;

    private double kmFeitos;

    public Transportadora() {
    }


    public Transportadora(String codEmpresa, String password, String nomeEmpresa, Coordenadas gps, int nif, double raio, double priceKm, boolean on, double priceKg, TreeSet<Aceite> encomendasFeitas, double kmFeitos) {
        this.codEmpresa = codEmpresa;
        this.password = password;
        this.nomeEmpresa = nomeEmpresa;
        this.gps = gps.clone();
        this.nif = nif;
        this.raio = raio;
        this.priceKm = priceKm;
        this.on = on;
        this.priceKg = priceKg;
        TreeSet<Aceite> encFeitas = new TreeSet<Aceite>();
        if(encomendasFeitas != null) {
            for (Aceite encomenda : encomendasFeitas) {
                encFeitas.add(encomenda.clone());
            }
        }
        this.encomendasFeitas = encFeitas;
        this.kmFeitos = 0.0;
    }

    public Transportadora(Transportadora x) {
        this(x.codEmpresa, x.password, x.nomeEmpresa, x.gps, x.nif, x.raio, x.priceKm, x.on, x.priceKg, x.encomendasFeitas, x.kmFeitos);
    }

    public String getCodEmpresa() {
        return this.codEmpresa;
    }

    public void setCodEmpresa(String codEmpresa) {
        this.codEmpresa = codEmpresa;
    }

    public String getPassword() {
        return this.password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getNomeEmpresa() {
        return this.nomeEmpresa;
    }

    public void setNomeEmpresa(String nomeEmpresa) {
        this.nomeEmpresa = nomeEmpresa;
    }

    public Coordenadas getGps() {
        return this.gps;
    }

    public void setGps(Coordenadas gps) {
        this.gps = gps;
    }

    public int getNif() {
        return this.nif;
    }

    public void setNif(int nif) {
        this.nif = nif;
    }

    public double getRaio() {
        return this.raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public double getPriceKm() {
        return this.priceKm;
    }

    public void setPriceKm(double priceKm) {
        this.priceKm = priceKm;
    }

    public boolean isOn() {
        return this.on;
    }

    public boolean getOn() {
        return this.on;
    }

    public void setOn(boolean on) {
        this.on = on;
    }

    public boolean toogleOn() {
        return (this.on = (this.on == false));
    }

    public double getPriceKg() {
        return this.priceKg;
    }

    public void setPriceKg(double priceKg) {
        this.priceKg = priceKg;
    }

    public void addEncomendaFeitas(Aceite e) {
        this.encomendasFeitas.add(e.clone());
    }

    public void addEncomendaFeitas(String codEncomenda) {
        this.encomendasFeitas.add(new Aceite(codEncomenda));
    }

    public void addEncomendaFeitas(Encomenda encomenda) {
        this.encomendasFeitas.add(new Aceite(encomenda));
    }

    public TreeSet<Aceite> getEncomendasFeitas() {
        TreeSet<Aceite> ret = new TreeSet<>();
        for (Aceite aceite : this.encomendasFeitas) {
            ret.add(aceite.clone());
        }
        return ret;
    }

    public void setEncomendasFeitas(TreeSet<Aceite> encomendasFeitas) {
        this.encomendasFeitas.clear();
        for (Aceite aceite : encomendasFeitas) {
            this.encomendasFeitas.add(aceite.clone());
        }
    }

    public double getKmFeitos() {
        return this.kmFeitos;
    }

    public void setKmFeitos(double kmFeitos) {
        this.kmFeitos = kmFeitos;
    }

    public void addKmFeitos(double kmFeitos) {
        this.kmFeitos += kmFeitos;
    }

    //não mexe com Loka l ou Utilizador u
    public void addKmFeitos(final Loja l, final Utilizador u) {
        this.kmFeitos += this.distancia(l, u);
    }

    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Transportadora)) {
            return false;
        }
        Transportadora transportadora = (Transportadora) o;
        return Objects.equals(codEmpresa, transportadora.codEmpresa) && Objects.equals(nomeEmpresa, transportadora.nomeEmpresa) && Objects.equals(gps, transportadora.gps) && nif == transportadora.nif && raio == transportadora.raio && priceKm == transportadora.priceKm && on == transportadora.on && priceKg == transportadora.priceKg && Objects.equals(encomendasFeitas, transportadora.encomendasFeitas);
    }

    @Override
    public int hashCode() {
        return Objects.hash(codEmpresa, nomeEmpresa, gps, nif, raio, priceKm, on, priceKg, encomendasFeitas);
    }

    @Override
    public String toString() {
        return "{" +
            " codEmpresa='" + getCodEmpresa() + "'" +
            ", nomeEmpresa='" + getNomeEmpresa() + "'" +
            ", gps='" + getGps() + "'" +
            ", nif='" + getNif() + "'" +
            ", raio='" + getRaio() + "'" +
            ", priceKm='" + getPriceKm() + "'" +
            ", on='" + isOn() + "'" +
            ", priceKg='" + getPriceKg() + "'" +
            ", encomendasFeitas='" + getEncomendasFeitas() + "'" +
            "}";
    }


    public Transportadora clone(){
        return new Transportadora(this);
    }

    private double custo(double km){
        return km * this.priceKm;
    }

    private double custo(Encomenda e) {
        return this.priceKg * e.getPeso();
    }

    private double custo(Loja l, Utilizador u) {
        List<Coordenadas> list = new ArrayList<>();
        list.add(l.getGps());
        list.add(u.getGps());
        return this.custo(this.gps.distanceSequencial(list));
    }

    public double distancia(Loja l, Utilizador u) {
        List<Coordenadas> list = new ArrayList<>();
        list.add(l.getGps());
        list.add(u.getGps());
        return this.gps.distanceSequencial(list);
    }

    public double custo(Loja l, Utilizador u, Encomenda e) {
        return custo(l,u) + custo(e);
    }

    private boolean isNextTo(Loja l) {
        return this.getGps().isNextTo(l.getGps(), this.raio);
    }

    private boolean isNextTo(Utilizador u) {
        return this.getGps().isNextTo(u.getGps(), this.raio);
    }

    public boolean isNextTo(Loja l, Utilizador u) {
        return this.isNextTo(l) && this.isNextTo(u);
    }

}