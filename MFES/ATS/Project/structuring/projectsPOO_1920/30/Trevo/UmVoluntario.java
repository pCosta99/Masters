import java.lang.Object;
import java.time.LocalDate;
import java.time.*; 
import java.time.temporal.*; 
import java.util.ArrayList; 

public class UmVoluntario{
    private String codvoluntario;
    private String nome;
    private Coordenadas gps; 
    private double raio;
    private boolean aceita; 
    private String pass;
    private boolean disponivel; 
    private int ava; 
    private ArrayList<Integer> avaliacoes; 
    
    public UmVoluntario(){
    this.codvoluntario=new String ();
    this.nome=new String(); 
    this.gps=new Coordenadas();  
    this.raio=0.0;
    this.aceita = false; 
    this.disponivel = true;
    this.pass = new String(); 
    this.ava=0;
    this.avaliacoes = new ArrayList<Integer>();
    }
    
    public UmVoluntario(String codvoluntario,String n, Coordenadas gps,double raio,String e){
    this.codvoluntario=codvoluntario;
    this.nome= n;
    this.gps=gps;
    this.raio=raio;
    this.disponivel=true; 
    this.aceita=false;
    this.pass=e;
    this.ava=0;
    this.avaliacoes = new ArrayList<Integer>();
    }
    
    //cria voluntarios que podem entregar meds
    public UmVoluntario(String codvoluntario,String n, Coordenadas gps,double raio,boolean a,String e,int ava){
    this.codvoluntario=codvoluntario;
    this.nome= n;
    this.gps=gps;
    this.raio=raio;
    this.disponivel=true; 
    this.aceita=a;
    this.pass=e;
    this.ava=ava;
    this.avaliacoes = new ArrayList<Integer>();
    }
    
    public UmVoluntario(String codvoluntario,String n, Coordenadas gps,double raio,boolean a,String e){
    this.codvoluntario=codvoluntario;
    this.nome= n;
    this.gps=gps;
    this.raio=raio;
    this.aceita=a;
    this.disponivel=true; 
    this.ava=0;
    this.avaliacoes = new ArrayList<Integer>();
    this.pass = e; 
    }
    
    public UmVoluntario(UmVoluntario e){
    this.codvoluntario=e.getcodvoluntario();
    this.nome=e.getnome();
    this.gps=e.getgps();
    this.raio=e.getraio();
    this.pass=e.getpass();
    this.ava=e.mediaavaliaca();
    this.avaliacoes = e.getAvaliacoes(); 
    }
    
    public boolean getaceita(){
    return this.aceita;   
    }
    public String getpass(){
    return this.pass;   
    }
    public double getraio(){
    return this.raio;   
    }
    public void setraio(double e){
    this.raio=e;   
    }
    public String getnome(){
    return this.nome;   
    }
     
    public String getcodvoluntario(){
    return this.codvoluntario;   
    }
 
    public Coordenadas getgps(){
    return this.gps;    
    }
    public void setnome(String e){
    this.nome=e;   
    }
    public void setpass(String e){
    this.pass=e;   
    }
    public void setcodvoluntario(String e){
    this.codvoluntario=e;   
    }
    
    public void setgps(Coordenadas e){
    this.gps=e;    
    }
    
    public UmVoluntario clone(){
    return new UmVoluntario(this);   
    }
    
     public double distanciadoispontos(Coordenadas a, Coordenadas b){
     double x = a.getX();
     double xx = b.getX();
     double y = a.getY();
     double yy = b.getY();
     double distancia = 0.0;
     if(x>xx && y>yy){
     
     distancia= Math.sqrt((x - xx) * 2 + (y-yy)*2); 
     return distancia;}
     
     if(xx>x && yy>y){
     distancia= Math.sqrt((xx - x) * 2 + (yy-y)*2); 
     return distancia;}
        
     if(x>xx && yy>y){
     distancia= Math.sqrt((x - xx) * 2 + (yy-y)*2); 
     return distancia;}

     if(x>xx && y>yy){
     distancia= Math.sqrt((x - xx) * 2 + (y-yy)*2); 
     return distancia;}
     
     if(x==xx && y>yy){
     distancia= Math.sqrt((y-yy)*2); 
     return distancia;}
     
     if(x>xx && y==yy){
     distancia= Math.sqrt((x - xx) * 2 ); 
     return distancia;}
     
     if(x==xx && yy>y){
     distancia= Math.sqrt((yy-y)*2); 
     return distancia;}
     
     if(xx>x && y==yy){
     distancia= Math.sqrt((xx - x) * 2 ); 
     return distancia;}
     
     return distancia; 
     }

    

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nVoluntario: ");
        sb.append("\nNome: " + nome);
        sb.append("\nCodigo voluntario: " + codvoluntario);
        sb.append("\nCoordenadas: " + gps);
        sb.append("\nRaio de entrega: " + raio);
        return sb.toString();
     
        
    }
    public double getx(){
    return gps.getX();    
    }
    
    public double gety(){
    return gps.getY();           
    }   
    
    public boolean disponivel(){
    return this.disponivel;  
    }
    
    public void setdisponibilidade(boolean e){
    this.disponivel=e;     
    }
    
    public ArrayList<Integer> getAvaliacoes(){
    return new ArrayList<>(avaliacoes);
    }
    public void setAvaliacao(ArrayList<Integer> avaliacoes){
    this.avaliacoes = new ArrayList<>(avaliacoes);
    }
    
    public void addAvaliacao(int a){
    this.avaliacoes.add(a);  
    }
   
    public int mediaavaliaca(){
    int media = 0; 
    int quantos = 1; 
    for(int e : avaliacoes){
    media += e;   
    quantos +=1; 
    }
    if(quantos == 0) return 0; 
    return media/quantos;  
    }
   }
    