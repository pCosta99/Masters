package Model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class Encomenda implements Comparable<Encomenda>, Serializable {

    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private GPS lojaGPS;
    private GPS userGPS;
    private double peso;
    private List<LinhaEncomenda> linhasEncomenda;

    public Encomenda() {
        this.codEncomenda = "";
        this.codUtilizador = "";
        this.codLoja = "";
        this.lojaGPS = new GPS();
        this.userGPS = new GPS();
        this.peso = 0;
        this.linhasEncomenda = new ArrayList<>();
    }

    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso, List<LinhaEncomenda> linhasEncomenda) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.linhasEncomenda = linhasEncomenda;
    }

    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, GPS lojaGPS, GPS userGPS, double peso, List<LinhaEncomenda> linhasEncomenda) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.lojaGPS = lojaGPS;
        this.userGPS = userGPS;
        this.peso = peso;
        this.linhasEncomenda = linhasEncomenda;
    }

    public Encomenda(Encomenda e) {
        this.codEncomenda = e.codEncomenda;
        this.codUtilizador = e.codUtilizador;
        this.codLoja = e.getCodLoja();
        this.lojaGPS = e.getLojaGPS();
        this.userGPS = e.getUserGPS();
        this.peso = e.getPeso();
        this.linhasEncomenda = e.linhasEncomenda;
    }

    public GPS getUserGPS() {
        return userGPS;
    }

    public void setUserGPS(GPS userGPS) {
        this.userGPS = userGPS;
    }

    public String getCodEncomenda() {
        return codEncomenda;
    }

    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    public String getCodUtilizador() {
        return codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getCodLoja() {
        return codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public GPS getLojaGPS() {
        return lojaGPS;
    }

    public void setLojaGPS(GPS lojaGPS) {
        this.lojaGPS = lojaGPS;
    }



    public double getPeso() {
        return peso;
    }

    public void setPeso(float peso) {
        this.peso = peso;
    }

    public List<LinhaEncomenda> getLinhasEncomenda() {
        return linhasEncomenda;
    }

    public void setLinhasEncomenda(List<LinhaEncomenda> linhasEncomenda) {
        this.linhasEncomenda = linhasEncomenda;
    }

    @Override
    public String toString() {
        return "Encomenda: " +
                "codEnc=" + codEncomenda + " | " +
                "codUser='" + codUtilizador + " | " +
                "codStore='" + codLoja + " | " +
                "GPS=" + lojaGPS + " | " +
                "peso=" + peso;
    }

    @Override
    public int compareTo(Encomenda enc) {
        return Integer.compare(Integer.parseInt(this.codEncomenda.substring(1)),Integer.parseInt(enc.getCodEncomenda().substring(1)));
    }
}
