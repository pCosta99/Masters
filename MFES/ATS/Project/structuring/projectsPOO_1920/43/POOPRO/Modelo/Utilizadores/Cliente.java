package Modelo.Utilizadores;

import Modelo.Encomendas.Encomenda;
import Modelo.Encomendas.RegistoEncomendas;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.List;

public class Cliente extends Utilizador  {

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */



    /**
     * CONSTRUTOR VAZIO
     */

    public Cliente() {
        super();
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 1
     */

    public Cliente(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, double nLatitude, double nLongitude) {
        super(nNome, nEmail, nPassword, nCodUtilizador, nNif, nLatitude, nLongitude);
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 2
     */

    public Cliente(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, List<Encomenda> nPedidos, RegistoEncomendas nRegisto, LocalDate nDataNascimento, double nLatitude, double nLongitude) {
        super(nNome, nEmail, nPassword, nCodUtilizador, nNif, nLatitude, nLongitude, nPedidos, nRegisto);
    }

    /**
     * CONSTRUTOR POR CÓPIA
     */

    public Cliente(Cliente nCliente) {
        super(nCliente);
    }

    /**
     * GETTERS
     */



    /**
     * SETTERS
     */



    /**
     * MÉTODO CLONE
     */

    public Cliente clone() {
        return new Cliente(this);
    }

    /**
     * MÉTODO EQUALS
     */

    public boolean equals(Object o) {
        return super.equals(o);
    }

    /**
     * MÉTODO TOSTRING
     */

    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("Cliente{");
        sb.append(super.toString());
        sb.append('}');
        return sb.toString();
    }

}