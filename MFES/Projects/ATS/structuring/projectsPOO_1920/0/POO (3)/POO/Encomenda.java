import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Encomenda implements Serializable {
    private String               codEncomenda;
    private String               codCliente;
    private String               codLoja;
    private double               peso;
    private List<LinhaEncomenda> linhasEncomendas;

    public Encomenda() {
        this.codEncomenda = new String();
        this.codCliente = new String();
        this.codLoja = new String();
        this.peso = 0;
        this.linhasEncomendas = new ArrayList<>();
    }

    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso, ArrayList<LinhaEncomenda> linhasEncomenda) {
        this.codEncomenda = codEncomenda;
        this.codCliente = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.setLinhasEncomendas(linhasEncomenda);
    }

    public Encomenda(Encomenda e) {
        this.codEncomenda = e.getCodEncomenda();
        this.codCliente = e.getCodCliente();
        this.codLoja = e.getCodLoja();
        this.peso = e.getPeso();
        this.linhasEncomendas = e.getLinhasEncomendas();
    }

    public String getCodEncomenda() {
        return this.codEncomenda;
    }

    public String getCodCliente() {
        return this.codCliente;
    }

    public String getCodLoja() {
        return this.codLoja;
    }

    public double getPeso() {
        return this.peso;
    }

    public List<LinhaEncomenda> getLinhasEncomendas() {
        List<LinhaEncomenda> aux = new ArrayList<>();
        for (LinhaEncomenda l : this.linhasEncomendas) {
            aux.add(l.clone());
        }
        return aux;
    }

    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    public void setCodCliente(String codUtilizador) {
        this.codCliente = codUtilizador;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public void setLinhasEncomendas(List<LinhaEncomenda> linha) {
        this.linhasEncomendas = new ArrayList<>(linha.size());
        linha.forEach(le -> this.linhasEncomendas.add(le.clone()));
    }
    
    public Encomenda clone() {
        return new Encomenda(this);
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("\tCodigo Encomenda: " + this.codEncomenda + "\n");
        s.append("\tCodUtilizador: " + this.codCliente + "\n");
        s.append("\tCodLoja: " + this.codLoja + "\n");
        s.append("\tPeso: " + this.peso + "\n");
        s.append("\tLinhas de encomenda: " + this.linhasEncomendas.toString());
        return s.toString();
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
            
        if ((obj == null) || (this.getClass() != obj.getClass()))
            return false;
        
        Encomenda e = (Encomenda) obj;
        return this.codEncomenda.equals(e.getCodEncomenda()) &&
                this.codCliente.equals(e.getCodCliente()) &&
                this.codLoja.equals(e.getCodLoja()) &&
                this.peso == e.getPeso() &&
                this.linhasEncomendas.equals(e.getLinhasEncomendas());
    }
}

