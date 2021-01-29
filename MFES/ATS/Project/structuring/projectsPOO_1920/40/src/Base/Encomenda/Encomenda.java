package Base.Encomenda;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Objects;

import MVC.Model.Model;

/**
 * Encomenda
 */

public class Encomenda implements Comparable, Comparator<Encomenda>, Serializable {
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private String codTransportador; //Voluntario ou Loja
    private double peso;
    private ArrayList<LinhaEncomenda> lista;

    private LocalDateTime criation;

    private int timePassed = 0;
    private LocalDateTime received;
    private double custo = -1;


    private double rating;
    //private Coordenadas gps;
    

    public Encomenda() {
        this.lista = new ArrayList<LinhaEncomenda>();
        this.criation = LocalDateTime.now();

    }

    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso, ArrayList<LinhaEncomenda> lista) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.lista = new ArrayList<LinhaEncomenda>();
        if(lista != null) {
            for (LinhaEncomenda linhaEncomenda : lista) {
                this.lista.add(linhaEncomenda);
            }
        }
        this.criation = LocalDateTime.now();
        this.rating = -1.0;
        this.timePassed = 0;
    }

    public Encomenda(Encomenda x) {
        this(x.codEncomenda,x.codUtilizador,x.codLoja,x.peso,x.lista);
        this.criation = x.getCriation();
    }

    public String getCodEncomenda() {
        return this.codEncomenda;
    }

    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getCodLoja() {
        return this.codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public double getPeso() {
        return this.peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public void addPeso(double peso) {
        this.peso += peso;
    }

    public boolean listaEmpty() {
        return this.lista.size() == 0;
    }

    public ArrayList<LinhaEncomenda> getLista() {
        ArrayList<LinhaEncomenda> ret = new ArrayList<>();

        for(LinhaEncomenda i : this.lista) {
            ret.add(i.clone());
        }

        return ret;
    }

    public void setLista(ArrayList<LinhaEncomenda> lista) {
        this.lista.clear();
        for (LinhaEncomenda linhaEncomenda : lista) {
            this.lista.add(linhaEncomenda);
        }
    }

    public void addLinhaEncomenda(LinhaEncomenda x) {
        this.lista.add(x.clone());
    }

    public LocalDateTime getCriation() {
        return this.criation;
    }

    public void setCriation(LocalDateTime criation) {
        this.criation = criation;
    }


    public String getCodTransportador() {
        return this.codTransportador;
    }

    public void setCodTransportador(String codTransportador) {
        this.codTransportador = codTransportador;
    }


    public double getRating() {
        return this.rating;
    }

    public void setRating(double rating) {
        this.rating = rating;
    }

    public LocalDateTime getReceived() {
        return this.received;
    }

    public void setReceived(LocalDateTime received) {
        this.received = received;
    }

    public double getCusto() {
        return this.custo;
    }

    public void setCusto(double custo) {
        this.custo = custo;
    }

    public int getTimePassed() {
        return this.timePassed;
    }

    public void setTimePassed(int timePassed) {
        this.timePassed = timePassed;
    }

    @Override
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof Encomenda)) {
            return false;
        }
        Encomenda encomenda = (Encomenda) o;
        return Objects.equals(codEncomenda, encomenda.codEncomenda) && Objects.equals(codUtilizador, encomenda.codUtilizador) && Objects.equals(codLoja, encomenda.codLoja) && peso == encomenda.peso && Objects.equals(lista, encomenda.lista);
    }

    @Override
    public int hashCode() {
        return Objects.hash(codEncomenda, codUtilizador, codLoja, peso, lista);
    }

    @Override
    public String toString() {
        return "{" +
            " codEncomenda='" + getCodEncomenda() + "'" +
            ", codUtilizador='" + getCodUtilizador() + "'" +
            ", codLoja='" + getCodLoja() + "'" +
            ", peso='" + getPeso() + "'" +
            ", lista='" + getLista() + "'" +
            "}";
    }

    public String toString(Model x) {
        return "{" +
            " codEncomenda='" + getCodEncomenda() + "'" +
            ", codUtilizador='" + getCodUtilizador() + "'" +
            ", Loja='" + x.getLoja(getCodLoja()) + "'" +
            ", peso='" + getPeso() + " kg'" +
            ", lista='" + getLista() + "'" +
            ", creation='" + getCriation().toString() + "'" +
            "}";
    }

    public Encomenda clone() 
    {
        return new Encomenda(this);
    }

    public double calculaValorTotal() {
        double total = 0;
        for(LinhaEncomenda i : this.lista) {
            total += i.calculaValorLinhaEnc();
        }
        return total;
    }

    // public double calcularValorDesconto() {
    //     double total = 0;
    //     for(LinhaEncomenda i : this.lista) {
    //         total += i.calculaValorDesconto();
    //     }
    //     return total;
    // }

    public int numeroTotalProdutos() {
        int total = 0;
        for(LinhaEncomenda i : this.lista) {
            total += i.getQuantidade();
        }
        return total;
    }

    public boolean existeProdutoEncomenda(String refProduto) {
        for(LinhaEncomenda i : this.lista) {
            if(i.getCodProduto() == refProduto){
                return true;
            }
        }
        return false;
    }

    public void adicionaLinha(LinhaEncomenda linha) {
        this.lista.add(linha);
    }

    public void removeProduto(String codProd) {
        for(LinhaEncomenda i : this.lista) {
            if (i.getCodProduto() == codProd) this.lista.remove(i);
        }
    }

    @Override
    public int compare(Encomenda o1, Encomenda o2) {
        return o1.codEncomenda.compareTo(o2.codEncomenda);
    }

    @Override
    public int compareTo(Object o) {
        return this.codEncomenda.compareTo(((Encomenda) o).codEncomenda);
    }
}