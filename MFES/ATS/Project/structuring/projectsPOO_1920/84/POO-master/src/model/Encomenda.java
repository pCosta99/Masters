package model;

import java.io.Serializable;
import java.nio.MappedByteBuffer;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public  class Encomenda implements Serializable {

    private String idEncomenda;
    //private Voluntario  v; // informaçao relativa a quem a transportou
    //private Empresa e;
    private Cliente cliente; //informação
    private LocalDateTime data;
    private boolean especial;
    private Coordenadas destino;
    private double peso;
    private double taxa; //percentagem
    private List<Produto> produtos;
    private double custo;

    public Encomenda() {
        this.idEncomenda = "";
        this.data = LocalDateTime.now();
        this.especial = false;
        this.destino = new Coordenadas();
        this.peso = 0;
        this.taxa=0;
    }

    public Encomenda(String idEncomenda,Cliente c, LocalDateTime data, boolean especial, Coordenadas destino, double taxa, List<Produto> prod) { //retirei o peso do construtor
        this.idEncomenda = idEncomenda;
        this.cliente= c;
        this.data = data;
        this.especial = especial;
        this.destino = destino;
        this.peso = getPeso(prod);
        this.taxa=taxa;
        this.produtos = prod;
        this.custo = getCustoR();
    }

    public Encomenda(Encomenda enc) {
        this.idEncomenda = enc.getIdEncomenda();
        this.cliente = enc.getCliente();
        this.data = enc.getData();
        this.especial = enc.isEspecial();
        this.destino = enc.getDestino();
        this.peso = getPeso(enc.getProdutos());
        this.taxa = enc.getTaxa();
        this.produtos = enc.getProdutos();
        this.custo = enc.getCustoR();
    }

    public Cliente getCliente() {
        return cliente;
    }

    public void setCliente(Cliente cliente) {
        this.cliente = cliente;
    }

    public double getTaxa(){ //
        return this.taxa;
    }
    
    public void setTaxa(double t){ //
        this.taxa=t;
    }

    public void setPeso(double pes){
        this.peso = pes;
    }



    public String getIdEncomenda() {
        return idEncomenda;
    }

    public void setIdEncomenda(String idEncomenda) {
        this.idEncomenda = idEncomenda;
    }

    public boolean isEspecial() {
        return especial;
    }

    public void setEspecial(boolean especial) {
        this.especial = especial;
    }

    public LocalDateTime getData() {
        return data;
    }

    public void setData(int year, int month, int day, int hour, int minute) { //agr tem horas e minutos por ser LocalDateTime
        this.data = LocalDateTime.of(year, month, day, hour, minute);
    }

    public Coordenadas getDestino() {
        return this.destino;
    }

    public void setDestino(Coordenadas c) {
        this.destino = c;
    }





    public void setProdutos(ArrayList<Produto> prod){
        this.produtos = new ArrayList<>(prod.size());
        for(Produto p: prod){ //correr no prod
            this.produtos.add(p);
        }
    }

    public ArrayList getProdutos(){
        ArrayList<Produto> novo = new ArrayList<>(this.produtos);
        for(Produto prod: produtos){
            novo.add(prod);
        }
        return novo;

    }


    public String toString(){
        StringBuilder sb = new StringBuilder();


        sb.append(getIdEncomenda()).append("\n");
        sb.append(this.cliente.getId()).append("\n");
        sb.append(getData()).append("\n");
        sb.append(getCusto()).append("\n");
        sb.append(isEspecial()).append("\n");
        sb.append(getDestino()).append("\n");
        sb.append(getCustoR()).append("\n");
        sb.append(getTaxa()).append("\n");
        sb.append(getPeso(this.getProdutos())).append("\n");
        //sb.append(getProdutos().toString()).append("\n");
        return sb.toString();
    }


    public double getPeso(List<Produto> prod){
        double pesototal=0;
        for(Produto p: prod) {
            pesototal += p.getPeso();
        }
        return pesototal;
    }

    public double custoReal(){
        double r=0;
        r=this.custo+((this.custo*this.getTaxa()*getPeso(this.getProdutos()))/100);
        return r;
    }

    public void addProduto(Produto e){
        this.produtos.add(e);
    }

    public void removeProduto(Produto e){
        for(Produto s: produtos) {
            if(e.equals(s))this.produtos.remove(s);
        }
    }

    public double getCustoR (){
        double r=0;
        for(Produto s: produtos) r+=s.getPreco();
        return r;
    }

    public double getCusto(){
        return this.custo;
    }

    public void setCusto(double a){
        this.custo=a;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda encomenda = (Encomenda) o;
        return especial == encomenda.especial &&
                Double.compare(encomenda.peso, peso) == 0 &&
                Double.compare(encomenda.taxa, taxa) == 0 &&
                Double.compare(encomenda.custo, custo) == 0 &&
                Objects.equals(idEncomenda, encomenda.idEncomenda) &&
                Objects.equals(cliente, encomenda.cliente) &&
                Objects.equals(data, encomenda.data) &&
                Objects.equals(destino, encomenda.destino) &&
                Objects.equals(produtos, encomenda.produtos);
    }

    public String toParsableVoluntarioEncomenda(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.idEncomenda).append("\n");
        sb.append(this.cliente.getId()).append("\n");
        sb.append(this.data.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))).append("\n");
        sb.append(this.peso).append("\n");
        sb.append(this.isEspecial()).append("\n");
        sb.append(this.destino.toString()).append("\n");
        sb.append(this.taxa).append("\n");
        sb.append(this.custo).append("\n");

        return sb.toString();
    }

    public String toParsableClienteEncomenda(){
        StringBuilder str = new StringBuilder();
        str.append(this.idEncomenda).append("\n");
        str.append(this.cliente.getId()).append("\n");
        str.append(this.data.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))).append("\n");
        str.append(this.peso).append("\n");
        str.append(this.isEspecial()).append("\n");
        str.append(this.destino).append("\n");
        str.append(this.taxa).append("\n");
        str.append(this.custo).append("\n");
        return str.toString();
    }


    public String toParsableEmpresaEncomenda(){StringBuilder str = new StringBuilder();
        str.append(this.idEncomenda).append("\n");
        str.append(this.cliente.getId()).append("\n");
        str.append(this.data.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))).append("\n");
        str.append(this.peso).append("\n");
        str.append(this.isEspecial()).append("\n");
        str.append(this.destino).append("\n");
        str.append(this.taxa).append("\n");
        str.append(this.custo).append("\n");
        return str.toString();

    }




    public Encomenda clone(){
        return new Encomenda(this);
    }

}