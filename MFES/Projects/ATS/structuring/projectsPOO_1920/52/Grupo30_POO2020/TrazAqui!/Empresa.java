import java.util.List;
import java.util.ArrayList;
import java.util.*;
import java.io.*;

/**
 * Escreva a descrição da classe Empresa aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Empresa extends Utilizador implements Serializable
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private int NIF;
    private double raiogeografico;
    private int velocidade;
    private double rating;
    private int nmrClassificacoes;
    private double taxa; //preco por km
    private int multitasking;//1 se fizer mais do q 1 entrega ao mesmo tempo 0 se nao
    private int indicador; //1 se estiver disposto a recolher 0 se nao
    private int capacidade; // quantas encomendas conseguem tratar ao mm tempo 
    private List<RealizadaEmpresa> re;

    /**
     * COnstrutor para objetos da classe Empresa
     */
    public Empresa()
    {
        // inicializa variáveis de instância
        super();
        this.NIF=0;
        this.raiogeografico=0;
        this.velocidade=0;
        this.rating=0.0;
        this.nmrClassificacoes=0;
        this.taxa=0;
        this.multitasking=0;
        this.indicador=1;
        this.capacidade=1;
        this.re = new ArrayList<>();
    }

    public Empresa(Empresa e){
        super(e);
        this.NIF=e.getNIF();
        this.raiogeografico=e.getRaiogeografico();
        this.velocidade=e.getVelocidade();
        this.rating=e.getRating();
        this.nmrClassificacoes=e.getnmrClassificacoes();
        this.taxa=e.getTaxa();
        this.multitasking=e.getMulti();
        this.indicador=e.getIndicador();
        this.capacidade=e.getCapacidade();
        this.re=e.getRe();
    }
    
    public Empresa(String email,String nome,String password,Localizacao localizacao,int nif, double raiogeografico, int velocidade, double rating,int nmrClassificacoes, double taxa,int multitasking, int indicador, int capacidade,List<RealizadaEmpresa> reAux){
        super(email,nome,password,localizacao);
        this.NIF=nif;
        this.raiogeografico=raiogeografico;
        this.velocidade=velocidade;
        this.rating=rating;
        this.nmrClassificacoes=nmrClassificacoes;
        this.taxa=taxa;
        this.multitasking=multitasking;
        this.indicador=indicador;
        this.capacidade=capacidade;
        this.re=reAux;
    }
    
    
    // getters 
    public int getNIF(){
        return this.NIF;
    }
    public double getRaiogeografico(){
        return this.raiogeografico;
    }
    
    public int getVelocidade(){
        return this.velocidade;
    }
    
    public double getRating(){
        return this.rating;
    }
    
    public int getnmrClassificacoes() {
        return this.nmrClassificacoes;
    }
    
    public double getTaxa(){
        return this.taxa;
    }
    

    public int getMulti(){
        return this.multitasking;
    }
    
    
    public int getIndicador(){
        return this.indicador;
    }
    
    
    public int getCapacidade(){
        return this.capacidade;
    }
    
    public List<RealizadaEmpresa> getRe(){
        List<RealizadaEmpresa> l = new ArrayList();
        for(RealizadaEmpresa r : this.re)
            l.add(r);
        return l;
    }
    
    // setters
    
    public void setNIF(int NIF){
         this.NIF=NIF;
    }
    
    public void setRaiogeografico(double raiogeografico){
        this.raiogeografico=raiogeografico;
    }
    
    public void setVelocidade(int velocidade){
        this.velocidade=velocidade;
    }
    
    public void setRating(double rating){
        this.rating=rating;
    }
    
    public void setnmrClassificacoes(int nmrClassificacoes){
        this.nmrClassificacoes=nmrClassificacoes;
    }
    
     public void setTaxa(double taxa){
        this.taxa=taxa;
    }

     public void setMulti(int multitasking){
        this.multitasking=multitasking;
    }
    
    public void setIndicador(int indicador){
    this.indicador=indicador;
    }
    
     public void setCapacidade(int capacidade){
        this.capacidade=capacidade;
    }
    
    public void setRe(List<RealizadaEmpresa> l){
        this.re = new ArrayList<>();
        for(RealizadaEmpresa r : l)
            this.re.add(r);
    }
    
    public boolean equals (Object o){
        if(this==o) return true;
        
        if((o==null) || (this.getClass()!=o.getClass())) return false;
        
        Empresa e = (Empresa) o;
        return(super.equals(e)&&this.raiogeografico==e.getRaiogeografico())
            &&(this.NIF==e.getNIF())
            &&(this.velocidade==e.getVelocidade())
            &&(this.rating==e.getRating())&&(this.nmrClassificacoes==e.getnmrClassificacoes()) &&(this.taxa==e.getTaxa())
            &&(this.multitasking==e.getMulti()&&(this.indicador==e.getIndicador())
            &&(this.capacidade==e.getCapacidade()) && this.getRe().equals(e.getRe()));
            
    }
    
    public String toString() {
        String s = new String();
 
        s = ("\n Empresa->" + 
               " Email: " + this.getEmail() + " | " +
               " Nome: " + this.getNome() +  " | " +
               " Raio Geografico(m) :" + this.getRaiogeografico() +" | " +
               " Velocidade m�dia (m/s): " + this.velocidade +" | " +
               " Rating: " + this.rating + " | " +
               " Nr de Classifica��es: " + this.nmrClassificacoes + " | " +
               " Multitasking(Sim:1/N�o:0): " + this.multitasking + " | " +
               " Localizacao: "+ this.getLocalizacao() +"\n") ;
 
        return s;
    }
    
    
    public Empresa clone() {
        return new Empresa(this);
    }
    
    //metodos para atualizar listas de encomendas entregues
    public void atualizaLE(RealizadaEmpresa r){
        ArrayList<RealizadaEmpresa> list = new ArrayList<RealizadaEmpresa>(this.getRe());
        list.add(r);
        this.setRe(list);
    }
   
}
