package Modelo.Encomendas;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class RegistoEncomendas implements Serializable {

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */

    Map<String, Encomenda> registo;

    /**
     * CONSTRUTOR VAZIO
     */

    public RegistoEncomendas() {
        this.registo = new HashMap<>();
    }

    /**
     * CONSTRUTOR PARAMETRIZADO
     */

    public RegistoEncomendas(Map<String, Encomenda> nRegisto) {
        this.registo = new HashMap<>();
        for (Map.Entry<String, Encomenda> l : nRegisto.entrySet()) {
            this.registo.put(l.getKey(), l.getValue().clone());
        }
    }

    /**
     * CONSTRUTOR POR CÓPIA
     */

    public RegistoEncomendas(RegistoEncomendas nRegistoEncomendas) {
        this.registo = nRegistoEncomendas.getRegisto();
    }

    /**
     * GETTERS
     */

    public Map<String, Encomenda> getRegisto() {
        Map<String, Encomenda> res = new HashMap<>();

        for (Map.Entry<String, Encomenda> l : this.registo.entrySet()) {
            res.put(l.getKey(), l.getValue().clone());
        }

        return res;
    }

    /**
     * SETTERS
     */

    public void setRegisto(Map<String, Encomenda> nRegisto) {
        this.registo = new HashMap<>();

        for (Map.Entry<String, Encomenda> l : nRegisto.entrySet()) {
            this.registo.put(l.getKey(), l.getValue().clone());
        }
    }

    /**
     * MÉTODO CLONE
     */

    public RegistoEncomendas clone() {
        return new RegistoEncomendas(this);
    }

    /**
     * MÉTODO EQUALS
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RegistoEncomendas that = (RegistoEncomendas) o;
        return Objects.equals(registo, that.registo);
    }

    /**
     * MÉTODO TOSTRING
     */

    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("Registo de Encomendas{");
        sb.append("registo=").append(this.registo);
        sb.append('}');
        return sb.toString();
    }
}
