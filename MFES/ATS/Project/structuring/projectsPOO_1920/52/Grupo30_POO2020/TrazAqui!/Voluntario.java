import java.util.List;
import java.util.ArrayList;
import java.util.*;
import java.io.*;


public class Voluntario extends Utilizador implements Serializable
{
    // variÃ¡veis de instÃ¢ncia - substitua o exemplo abaixo pelo seu prÃ³prio
    private double raiogeografico;
    private int velocidade;
    private double rating;
    private int nmrClassificacoes;
    private int verificador;
    private List<RealizadaVoluntario> rv;
    

    /**
     * COnstrutor para objetos da classe Utilizador
     */
    public Voluntario()
    {
     super();
     this.raiogeografico=0;
     this.velocidade=0;
     this.rating=0.0;
     this.nmrClassificacoes=0;
     this.verificador=0; // 1 se o voluntario estiver ocupado; 0 cc
     this.rv = new ArrayList<>();
    }

    
      public Voluntario(Voluntario v) {
        super(v);
        this.raiogeografico=v.getRaiogeografico();
        this.velocidade=v.getVelocidade();
        this.rating=v.getRating();
        this.nmrClassificacoes=v.getnmrClassificacoes();
        this.verificador=v.getVerificador();
        this.rv=v.getRv();
    }  
    
    
    public Voluntario(String email,String nome,String password,Localizacao localizacao, double raiogeografico, int velocidade, double rating,int nmrClassificacoes,int verificador,List<RealizadaVoluntario> rvAux){
        super(email,nome,password,localizacao);
        this.raiogeografico=raiogeografico;
        this.velocidade=velocidade;
        this.rating=rating;
        this.nmrClassificacoes=nmrClassificacoes;
        this.verificador=verificador;
        this.rv=rvAux;

}
 


// getters


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
    
    public int getVerificador(){
        return this.verificador;
    }
    
    public List<RealizadaVoluntario> getRv(){
        List<RealizadaVoluntario> l = new ArrayList();
        for(RealizadaVoluntario r : this.rv)
            l.add(r);
        return l;
    }
    // setters 
    

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
    
    public void setVerificador(int verificador){
        this.verificador=verificador;
    }
    
    public void setRv(List<RealizadaVoluntario> l){
        this.rv = new ArrayList<>();
        for(RealizadaVoluntario r : l)
            this.rv.add(r);
    }
    
    public boolean equals (Object o){
        if(this==o) return true;
        
        if((o==null) || (this.getClass()!=o.getClass())) return false;
        
        Voluntario u = (Voluntario) o;
        return(super.equals(u)&&this.raiogeografico==u.getRaiogeografico()) && (this.velocidade==u.getVelocidade())
            &&(this.rating==u.getRating())&& (this.nmrClassificacoes==u.getnmrClassificacoes()) && (this.verificador==u.getVerificador()
            && this.getRv().equals(u.getRv()));
            
    }
    
    public String toString() {
        String s = new String();
 
        s = ("\n Voluntario->" + 
               "Email: " + this.getEmail() + " | " +
               " Nome: " + this.getNome() +  " | " +
               " Raio Geografico: " + this.getRaiogeografico() + " | " +
               " Velocidade média (m/s): " + this.velocidade +" | " +
               " Rating: " + this.rating + " | " +
               " Nr de Classificações: " + this.nmrClassificacoes +  " | " +
               " Localizacao: "+ this.getLocalizacao());
             
 
        return s;
    }
    
    
    public Voluntario clone() {
        return new Voluntario(this);
    }
    
    //metodos para atualizar listas de encomendas entregues
    public void atualizaLV(RealizadaVoluntario r){
        ArrayList<RealizadaVoluntario> list = new ArrayList<RealizadaVoluntario>(this.getRv());
        list.add(r);
        this.setRv(list);
    }
}

