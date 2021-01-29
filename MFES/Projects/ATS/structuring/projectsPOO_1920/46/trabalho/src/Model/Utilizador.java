package Model;

import com.sun.webkit.network.Util;

import java.io.Serializable;
import java.util.*;

public class Utilizador implements Serializable, IUtilizador{
    private String id;
    private String nome;
    private String email;
    private String pwd;
    private int acessos;
    private double localizacaoX;
    private double localizacaoY;
    private int estado;

    private Set<IEncomenda> historico;

    public Utilizador(String id,String email, String pwd, String nome, int acessos, double localizacaoX, double localizacaoY, Set<IEncomenda> historico, int estado) {
        this.id = id;
        this.nome = nome;
        this.email = email;
        this.pwd = pwd;
        this.acessos = acessos;
        this.localizacaoX = localizacaoX;
        this.localizacaoY = localizacaoY;
        this.historico = historico;
        this.estado = estado;
    }

    public Utilizador(Utilizador utilizador) {
        this.id = utilizador.id;
        this.nome = utilizador.nome;
        this.email = utilizador.email;
        this.pwd = utilizador.pwd;
        this.acessos = utilizador.acessos;
        this.localizacaoX = utilizador.localizacaoX;
        this.localizacaoY = utilizador.localizacaoY;
        this.historico = utilizador.historico;
        this.estado = utilizador.estado;

    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPwd() {
        return pwd;
    }

    public void setPwd(String pwd) {
        this.pwd = pwd;
    }

    public Utilizador() {
        this.id = null;
        this.nome = null;
        this.email = null;
        this.pwd = null;
        this.acessos = 0;
        this.localizacaoX = 0;
        this.localizacaoY = 0;
        this.historico = new HashSet<>() ;
        this.estado = 0;
    }

    public String getId() {
        return id;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public String getNome() {
        return nome;
    }

    public void setId(String id) {
        this.id = id;
    }

    public int getAcessos() {
        return acessos;
    }

    public int getEstado() {
        return estado;
    }

    public void setEstado(int estado) {
        this.estado = estado;
    }

    public void setAcessos(int acessos) {
        this.acessos = acessos;
    }

    public double getLocalizacaoX() {
        return localizacaoX;
    }

    public double getLocalizacaoY() {
        return localizacaoY;
    }

    public void setLocalizacaoX(double localizacaoX) {
        this.localizacaoX = localizacaoX;
    }

    public void setLocalizacaoY(double localizacaoY) {
        this.localizacaoY = localizacaoY;
    }

    public void addHistorico(IEncomenda e){
        historico.add(e);
    }

    public List<String> historico() {
        List<String> s = new ArrayList<>();
        for (IEncomenda e: historico) {
            s.add(e.getId());
        }
        return s;
    }

    public void setHistorico(TreeSet<IEncomenda> historico) {
        this.historico = historico;
    }

    @Override
    public Utilizador clone() throws CloneNotSupportedException {
        return new Utilizador(this);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Utilizador that = (Utilizador) o;
        return acessos == that.acessos &&
                estado == that.estado &&
                Double.compare(that.localizacaoX, localizacaoX) == 0 &&
                Double.compare(that.localizacaoY, localizacaoY) == 0 &&
                Objects.equals(id, that.id) &&
                Objects.equals(nome, that.nome) &&
                Objects.equals(historico, that.historico);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, nome, acessos, localizacaoX, localizacaoY, historico, estado);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizador{").append("id =").append(id)
                .append(", nome = ").append(nome)
                .append(", acessos = ").append(acessos)
                .append(", localizaçãoX = ").append(localizacaoX)
                .append(", localizaçãoY = ").append(localizacaoY)
                .append(", historico = ").append(historico)
                .append("}; \n");
        return sb.toString();
    }
}
