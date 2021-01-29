import java.lang.Object;
import java.time.LocalDate;
import java.time.*; 
import java.time.temporal.*; 
import java.util.ArrayList; 

public class Voluntario{
    private String codigoVoluntario;
    private String nome;
    private Coordenadas gps; 
    private double raio;
    private boolean meds;  
    private String password;
    private boolean disponivel; 
    private ArrayList<Integer> avaliacoes; 
    
    //construtores
    public Voluntario(){
    this.codigoVoluntario=new String ();
    this.nome=new String(); 
    this.gps=new Coordenadas();  
    this.raio=0.0;
    this.meds = false; 
    this.disponivel = true;
    this.password = new String(); 
    this.avaliacoes = new ArrayList<Integer>();
    }
    
    public Voluntario(String codigoVoluntario,String n, Coordenadas gps,double raio,String e){
    this.codigoVoluntario=codigoVoluntario;
    this.nome= n;
    this.gps=gps;
    this.raio=raio;
    this.disponivel=true; 
    this.meds=false;
    this.password=e;
    this.avaliacoes = new ArrayList<Integer>();
    }
    
    //criar voluntarios que aceitam medicamentos
    
    public Voluntario(String codigoVoluntario,String n, Coordenadas gps,double raio,boolean a,String e){
    this.codigoVoluntario=codigoVoluntario;
    this.nome= n;
    this.gps=gps;
    this.raio=raio;
    this.meds=a;
    this.disponivel=true; 
    this.avaliacoes = new ArrayList<Integer>();
    this.password = e; 
    }
    
    public Voluntario(Voluntario e){
    this.codigoVoluntario=e.getcodigoVoluntario();
    this.nome=e.getnome();
    this.gps=e.getgps();
    this.raio=e.getraio();
    this.password=e.getpassword();
    this.avaliacoes = e.getAvaliacoes(); 
    }
    
    //setters e getters
    public String getpassword(){
    return this.password;   
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
     
    public String getcodigoVoluntario(){
    return this.codigoVoluntario;   
    }
 
    public Coordenadas getgps(){
    return this.gps;    
    }
    
    public void setnome(String e){
    this.nome=e;   
    }
    
    public void setpassword(String e){
    this.password=e;   
    }
    
    public void setcodigoVoluntario(String e){
    this.codigoVoluntario=e;   
    }
    
    public void setgps(Coordenadas e){
    this.gps=e;    
    }
    
    public Voluntario clone(){
    return new Voluntario(this);   
    }
    
    public double distancia2pontos(Coordenadas a, Coordenadas b){ //calcula a distancia através das coordenadas
    double distancia= Math.sqrt((a.getX() - b.getX()) * 2 + (a.getY()-b.getY())*2); 
    return distancia;
    }
    

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nVoluntario: ");
        sb.append("\nNome: " + nome);
        sb.append("\nCodigo utilizador: " + codigoVoluntario);
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
    //disponibilidade
    public boolean disponivel(){
    return this.disponivel;  
    }
    
    public void setdisponibilidade(boolean e){
    this.disponivel=e;     
    }
    
    public ArrayList<Integer> getAvaliacoes(){
    return new ArrayList<>(avaliacoes);
    }
    //avaliacoes
    public void setAvaliacao(ArrayList<Integer> avaliacoes){
    this.avaliacoes = new ArrayList<>(avaliacoes);
    }
    
    public void addAvaliacao(int a){
    this.avaliacoes.add(a);  
    }
   
    public int media(){
    int media = 0; 
    int quantos = 0; 
    for(int e : avaliacoes){
    media = media + e;   
    quantos = quantos + 1; 
    }
    if(quantos == 0) return 0; 
    return media/quantos;  
    }
   }
    