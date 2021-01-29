package Modelo.Utilizadores;

import Modelo.Encomendas.Encomenda;
import Modelo.Encomendas.RegistoEncomendas;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public abstract class Utilizador implements Serializable {

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */

    private String nome;
    private String email;
    private String password;
    private String codUtilizador;
    private String nif;
    private double latitude;
    private double longitude;
    private List<Encomenda> pedidos;
    private RegistoEncomendas registo;

    /**
     * CONSTRUTOR VAZIO
     */

    public Utilizador() {
        this.nome = "n/a";
        this.email = "n/a";
        this.password = "n/a";
        this.codUtilizador = "n/a";
        this.nif = "n/a";
        this.latitude = 0.0;
        this.longitude = 0.0;
        this.pedidos = new ArrayList<>();
        this.registo = new RegistoEncomendas();
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 1
     */

    public Utilizador(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, double nLatitude, double nLongitude) {
        this.nome = nNome;
        this.email = nEmail;
        this.password = nPassword;
        this.codUtilizador = nCodUtilizador;
        this.nif = nNif;
        this.latitude = nLatitude;
        this.longitude = nLongitude;
        this.pedidos = new ArrayList<>();
        this.registo = new RegistoEncomendas();
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 2
     */

    public Utilizador(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, double nLatitude, double nLongitude, List<Encomenda> nPedidos, RegistoEncomendas nRegisto) {
        this.nome = nNome;
        this.email = nEmail;
        this.password = nPassword;
        this.codUtilizador = nCodUtilizador;
        this.nif = nNif;
        this.latitude = nLatitude;
        this.longitude = nLongitude;
        this.registo = nRegisto.clone();
        this.pedidos = new ArrayList<>();
        for(Encomenda e: nPedidos) {
            this.pedidos.add(e.clone());
        }
    }

    /**
     * CONSTRUTOR POR CÓPIA
     */

    public Utilizador(Utilizador nUtilizador) {
        this.nome = nUtilizador.getNome();
        this.email = nUtilizador.getEmail();
        this.password = nUtilizador.getPassword();
        this.codUtilizador = nUtilizador.getCodUtilizador();
        this.nif = nUtilizador.getNif();
        this.latitude = nUtilizador.getLatitude();
        this.longitude = nUtilizador.getLongitude();
        this.pedidos = nUtilizador.getPedidos();
        this.registo = nUtilizador.getRegisto();
    }

    /**
     * GETTERS
     */

    public String getNome() {
        return this.nome;
    }

    public String getEmail() {
        return this.email;
    }

    public String getPassword() {
        return this.password;
    }

    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    public String getNif() {
        return this.nif;
    }

    public double getLatitude() {
        return this.latitude;
    }

    public double getLongitude() {
        return this.longitude;
    }

    public List<Encomenda> getPedidos() {
        List<Encomenda> res = new ArrayList<>();

        for(Encomenda e: this.pedidos) {
            res.add(e.clone());
        }

        return res;
    }

    public RegistoEncomendas getRegisto() {
        return this.registo.clone();
    }

    /**
     * SETTERS
     */

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    public void setPedidos(List<Encomenda> nPedidos) {
        this.pedidos = new ArrayList<>();

        for(Encomenda e: nPedidos) {
            this.pedidos.add(e.clone());
        }
    }

    public void setRegisto(RegistoEncomendas nRegisto) {
        this.registo = new RegistoEncomendas(nRegisto);
    }

    /**
     * MÉTODO CLONE
     */

    public abstract Utilizador clone();

    /**
     * MÉTODO EQUALS
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Utilizador that = (Utilizador) o;
        return nif == that.nif &&
                Double.compare(that.latitude, latitude) == 0 &&
                Double.compare(that.longitude, longitude) == 0 &&
                Objects.equals(nome, that.nome) &&
                Objects.equals(email, that.email) &&
                Objects.equals(password, that.password) &&
                Objects.equals(codUtilizador, that.codUtilizador) &&
                Objects.equals(pedidos, that.pedidos) &&
                Objects.equals(registo, that.registo);
    }

    /**
     * MÉTODO TOSTRING
     */

    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("Utilizador{");
        sb.append("nome=").append(this.nome);
        sb.append(", email=").append(this.email);
        sb.append(", password=").append(this.password);
        sb.append(", codUtilizador=").append(this.codUtilizador);
        sb.append(", nif=").append(this.nif);
        sb.append(", latitude=").append(this.latitude);
        sb.append(", longitude=").append(this.longitude);
        sb.append(", pedidos=").append(this.pedidos);
        sb.append(", registo=").append(this.registo.toString());
        sb.append('}');
        return sb.toString();
    }

    /**
     * MÉTODO DISTANCIA
     */

    public void alterarLocal(double nLatitude, double nLongitude) {
        this.latitude = nLatitude;
        this.longitude = nLongitude;
    }

    public double getDistancia(double nLatitude, double nLongitude) {
        return Math.sqrt(Math.pow(this.latitude - nLatitude, 2) + Math.pow(this.longitude - nLongitude, 2));
    }

    /**
     * MÉTODO ADICIONAR PEDIDO
     */

    public void adicionarPedido(Encomenda nEncomenda) {
        this.pedidos.add(nEncomenda.clone());
    }

    public void removerPedido(Encomenda nEncomenda) {
        this.pedidos.remove(nEncomenda);
    }

    public Encomenda getPedido(String codEncomenda) {
        for(Encomenda e: this.pedidos) {
            if(e.getCodEncomenda().matches(codEncomenda)) {
                return e.clone();
            }
        }
        return null;
    }

    public boolean temPedido(String codEncomenda) {
        for(Encomenda e: this.pedidos) {
            if(e.getCodEncomenda().matches(codEncomenda)) {
                return true;
            }
        }
        return false;
    }

    public void atualizarPedidoAceite(Encomenda enc) {
        for(Encomenda e: this.pedidos) {
            if(e.getCodEncomenda().matches(enc.getCodEncomenda())) {
                this.pedidos.remove(e);
                this.pedidos.add(enc.clone());
            }
        }
    }
}
