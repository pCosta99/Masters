package model;

import exceptions.ExcecaoProdutoInexistente;

import java.io.Serializable;
import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;

public class Loja implements Serializable
{
    private String nome;
    private String cod;
    private Coordenadas local;
    private Boolean especial; // false=normal || true=especial
    private float faturacao;
    private int filaDeEspera;
    private double tempoMedioAtendimento;
    private List<Encomenda> Epending = new ArrayList<Encomenda>();
    private List<Encomenda> Eprontas = new ArrayList<Encomenda>();
    private List<Produto> produtos   = new ArrayList<Produto>();
    
    public Loja(){
        this.nome         = "NomeDaLoja";
        this.cod          = "codigoDaLoja";
        this.local        = new Coordenadas();
        this.especial     = false;
        this.faturacao    = 0;
        this.filaDeEspera = 0;
        this.tempoMedioAtendimento = 0;
        this.Epending     = new ArrayList<Encomenda>();
        this.Eprontas     = new ArrayList<Encomenda>();
        this.produtos     = new ArrayList<Produto>();
    }
    
    public Loja(Loja l){
        this.nome         = l.nome;
        this.cod          = l.cod;
        this.local        = l.local;
        this.especial     = l.especial;
        this.faturacao    = l.faturacao;
        this.tempoMedioAtendimento = l.getTempoMedioAtendimento();
        this.filaDeEspera = tamanhoFila();
        this.Epending     = l.Epending;
        this.Eprontas     = l.Eprontas;
        this.produtos     = l.produtos;
    }
    
    public Loja(String n, String c, int tf, Coordenadas l, boolean r,double tempoMedioAtendimento, List<Encomenda> pending,List<Encomenda> prontas, List<Produto> p){
        this.nome         = n;
        this.cod          = c;
        //this.faturacao    = f;
        this.filaDeEspera = tamanhoFila();
        this.tempoMedioAtendimento = tempoMedioAtendimento;
        this.local        = l;
        this.especial     = r;
        this.setEpending(pending);
        this.setEprontas(prontas);
        this.setProdutos(p);
    }
    
    public void setEpending (List<Encomenda> e){
        this.Epending = new ArrayList<>(this.Epending.size());
        for(Encomenda s:Epending) this.Epending.add(s); 
    }
    
    public void setEprontas (List<Encomenda> e){
        this.Eprontas = new ArrayList<>(this.Eprontas.size());
        for(Encomenda s:Eprontas) this.Eprontas.add(s); 
    }
    
    
    public void setProdutos (List<Produto> e){
        this.produtos = new ArrayList<>(this.produtos.size());
        for(Produto s:produtos) this.produtos.add(s); 
    }

    public double getTempoMedioAtendimento() {
        return tempoMedioAtendimento;
    }

    public void setTempoMedioAtendimento(double tempoMedioAtendimento) {
        this.tempoMedioAtendimento = tempoMedioAtendimento;
    }

    public void setNome(String x){
        this.nome=x;
    }
    
    public void setEstatuto(boolean x){
        this.especial=x;
    }
    
    public void setCod(String x){
        this.cod=x;
    }
    
    public void setFaturacao (Float f){
        this.faturacao=f;
    }
    
    public void setFilaE (int f){
        this.filaDeEspera=f;
    }
    
    public void setLocal (double x, double y){
      this.local = new Coordenadas(x,y);
    }
    
    public String getNome(){
        return this.nome;
    }
    
    public String getCod(){
        return this.cod;
    }
    
    public boolean getEstatuto(){
        return this.especial;
    }
    
    public float getFaturacao(){
        return this.faturacao;
    }
    
    public int getFilaE(){
        return this.filaDeEspera;
    }
    
    public Coordenadas getLocal(){
        return this.local;
    }
    
    public List<Encomenda> getEpending(){
       List<Encomenda> res = new ArrayList<>(this.Epending.size());
       
       for(Encomenda s:Epending) res.add(s.clone());
       return res;
    }
    
    public List<Encomenda> getEprontas(){
       List<Encomenda> res = new ArrayList<>(this.Eprontas.size());
       
       for(Encomenda s:Eprontas) res.add(s.clone());
       return res;
    }
    
    public List<Produto> getprodutos(){
       List<Produto> res = new ArrayList<>(this.produtos.size());
       
       for(Produto s:produtos) res.add(s.clone());
       return res;
    }
    
    public void addEprontas(Encomenda e){
        this.Eprontas.add(e);
    }
    
    public void removeEprontas(Encomenda e){
        for(Encomenda s:Eprontas) {
            if(e.equals(s))this.Eprontas.remove(s);
        }
    }
    
    public void addEpending(Encomenda e){
        this.Epending.add(e);
    }
    
    public void removeEpending(Encomenda e){
        for(Encomenda s:Epending) {
            if(e.equals(s))this.Epending.remove(s);
        }
    }
    
    public void addProduto(Produto e){
        this.produtos.add(e);
    }
    
    public void removeProduto(Produto e){
        for(Produto s:produtos) {
            if(e.equals(s))this.produtos.remove(s);
        }
    }

    public Produto getProduto(String id) throws ExcecaoProdutoInexistente {
        for(Produto p: this.produtos){
                if(p instanceof ProdutoNormal){
                    if(((ProdutoNormal) p).getIdNormal().equals(id)) return p.clone();

                }else{
                    if(((ProdutoMedico) p).getIdESpecial().equals(id)) return p.clone();
                }
            }
         throw new ExcecaoProdutoInexistente("Produto nao existe");

    }
    
    public Loja clone(){
      return new Loja(this);
    }


    @Override
    public String toString() {
        return new StringBuilder()
                .append(this.getNome()).append("\n")
                .append(this.getCod()).append("\n")
                .append(getLocal().toString()).append("\n")
                .append(this.getEstatuto()).append("\n")
                .append(this.getFaturacao()).append("\n")
                .append((this.getFilaE())).append("\n")
                .append(this.getTempoMedioAtendimento()).toString();
    }
    
    public int tamanhoFila(){
        int r=0;
        for(Encomenda s:Epending) r++;
        return r;
    }
    

}

