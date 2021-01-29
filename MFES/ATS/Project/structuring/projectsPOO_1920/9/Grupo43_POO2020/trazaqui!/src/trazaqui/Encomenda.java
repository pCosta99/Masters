package trazaqui;

import java.io.Serializable;
import java.util.ArrayList;
public class Encomenda implements Serializable {
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private ArrayList<LinhaEncomenda> linhas;

    public Encomenda(){
        this.codEncomenda = new String();
        this.codUtilizador = new String();
        this.codLoja= new String();
        this.peso= 0;
        this.linhas= new ArrayList<LinhaEncomenda>();
    }

    public Encomenda(String ce, String cu, String cl, double p,ArrayList<LinhaEncomenda> encs){
        this.codEncomenda = ce;
        this.codUtilizador = cu;
        this.codLoja= cl;
        this.peso= p;
        this.linhas=new ArrayList<>();
        for(LinhaEncomenda s : encs){
            this.linhas.add(s);
        }
    }

    public Encomenda(Encomenda e){
        this.codEncomenda = e.getcodEncomenda();
        this.codUtilizador = e.getcodUtilizador();
        this.codLoja = e.getcodLoja();
        this.peso = e.getPeso();
        this.setLinhas(e.getLinhas());
    }

    public ArrayList<LinhaEncomenda> getLinhas(){
        ArrayList<LinhaEncomenda> aux = new ArrayList<>();
        for( LinhaEncomenda s : this.linhas){
            aux.add(s);
        }
        return aux;
    }

    public void setLinhas(ArrayList<LinhaEncomenda> enc){
        this.linhas= new ArrayList<>();
        for(LinhaEncomenda s : enc){
            this.linhas.add(s);
        }
    }

    public void setCodEncomenda(String cod){
        this.codEncomenda=cod;
    }

    public void setCodLoja(String cod){
        this.codLoja=cod;
    }

    public void setCodUtilizador(String cod){
        this.codUtilizador=cod;
    }

    public void setPeso(double p){
        this.peso=p;
    }

    public String getcodEncomenda(){
        return this.codEncomenda;
    }

    public String getcodUtilizador(){
        return this.codUtilizador;
    }

    public String getcodLoja(){
        return this.codLoja;
    }

    public double getPeso(){
        return this.peso;
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomendas:\n").append("Codigo Encomenda:").append(this.codEncomenda).append("\n").append("Codigo Utilizador:")
                .append(this.codUtilizador).append("\n").append("Codigo Loja:").append(this.codLoja)
                .append("\n").append("Peso:").append(this.peso)
                .append("\n").append("Produtos:").append(this.linhas).append("\n");
        return sb.toString();
    }

    public boolean equals(Object o){
        if (o==this) return true;
        if (o==null || o.getClass()!=this.getClass()) return false;
        Encomenda e= (Encomenda) o;

        return e.getcodEncomenda().equals(this.getcodEncomenda())
                && e.getcodUtilizador().equals(this.getcodUtilizador())
                && e.getcodLoja().equals(this.getcodLoja())
                && e.getPeso()==this.getPeso()
                && e.getLinhas().equals(this.getLinhas());
    }

}