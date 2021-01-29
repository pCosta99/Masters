package model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Cliente extends User implements Serializable {
    
    private Coordenadas coor; //posicao exata
    private List<Encomenda> registoEncomendas; //Registo encomendas feitas
    private List<Encomenda> encomendaEmEspera; //encomendas em atualizaçao

    
    public Cliente() {
        super();
        this.coor = new Coordenadas();
        this.registoEncomendas = new ArrayList<>();
        this.encomendaEmEspera = new ArrayList<>();

    }
    
    public Cliente(String id, String nome, String email, String password,String morada,
                    Coordenadas c, List<Encomenda> reg ,List<Encomenda> espera) {
        super(id,nome,email,password,morada);
        this.coor = c;
        setRegistoEncomendasHistorico(reg);
        setRegistoEncomendasPending(espera);

    }
    
    public Cliente(Cliente novoCliente) {
        super(novoCliente);
        this.coor = novoCliente.getCoordenadas();
        setRegistoEncomendasHistorico(novoCliente.getRegistoEncomendasHistorico());
        setRegistoEncomendasPending(novoCliente.getRegistoEncomendasPending());
    }
    
    public Coordenadas getCoordenadas() {
        return this.coor.clone();
    }

    public void setCoordenadas(Coordenadas c) {
        this.coor = c;
    }

    public ArrayList<Encomenda> getRegistoEncomendasHistorico(){
        ArrayList<Encomenda> res = new ArrayList<Encomenda>();
        
        for(Encomenda e : this.registoEncomendas)
            res.add(e.clone());
        
        return res;
    }
    

    public void setRegistoEncomendasHistorico(List<Encomenda> r) {
        this.registoEncomendas = new ArrayList<Encomenda>();
        
        for(Encomenda e : r)
            this.registoEncomendas.add(e.clone());
    }


    public List<Encomenda> getRegistoEncomendasPending(){
        List<Encomenda> res = new ArrayList<>();

        for(Encomenda e : this.encomendaEmEspera)
            res.add(e.clone());

        return res;
    }

    public void setRegistoEncomendasPending(List<Encomenda> r) {
        this.encomendaEmEspera = new ArrayList<>();

        for(Encomenda e : r)
            this.encomendaEmEspera.add(e.clone());
    }
    
    public Cliente clone() {
        return new Cliente(this);
    }
    
    public void addEncomendaRegisto(Encomenda e){
        this.registoEncomendas.add(e);
    }

    public void removeEncomendaRegisto(Encomenda e){
        this.registoEncomendas.remove(e);
    }


    public void addEncomendaPending(Encomenda e){
        this.encomendaEmEspera.add(e);
    }

    public void removeEncomendaPending(Encomenda e){
        this.encomendaEmEspera.remove(e);
        }


    public List<Encomenda> registoEncomendaemPeriodo(LocalDateTime inicio, LocalDateTime fim){
        return this.getRegistoEncomendasHistorico().stream().filter(encomenda -> encomenda.getData().isEqual(inicio) && encomenda.getData().isAfter(inicio)
                && encomenda.getData().isEqual(fim) && encomenda.getData().isBefore(fim)).collect(Collectors.toList());
    }

    @Override
    public String toString() {
        return new StringBuilder()
                .append(this.getNome()).append("\n")
                .append(this.getId()).append("\n")
                .append(this.getEmail()).append("\n")
                .append(this.getMorada()).append("\n")
                .append(this.getCoordenadas()).append("\n").toString();


    }

    public int numeroEncomendas(){
        return this.registoEncomendas.size();
    }


    public double getTotalGasto() {
        double total=0;

        for(Encomenda e: this.registoEncomendas){
            total+=e.custoReal();
        }
        return total;
    }





}
