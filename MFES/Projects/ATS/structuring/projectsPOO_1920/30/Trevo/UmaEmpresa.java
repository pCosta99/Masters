import java.util.ArrayList;

public class UmaEmpresa{
 private String cod;
 private String nome;
 private Coordenadas gps;
 private int nif;
 private double raio; 
 private double custotransporte;
 private String pass; 
 private ArrayList<Integer> avaliacoes;
 private boolean disponivel; 
  
 public UmaEmpresa(){
 this.cod=new String();
 this.nome=new String();
 this.gps=new Coordenadas();
 this.nif=0;
 this.raio=0.0;
 this.custotransporte=0.0;
 this.pass=new String();
 this.avaliacoes = new ArrayList<Integer>();
 this.disponivel = true; 
 }
 public UmaEmpresa(String cod,String nome,Coordenadas gps,int nif, double raio, double custo,String e){
 this.cod=cod;
 this.nome=nome;
 this.raio=raio;
 this.nif=nif;
 this.custotransporte=custo;
 this.gps=gps;
 this.pass=e;
 this.avaliacoes = new ArrayList<Integer>();
 this.disponivel = true; 

 }
 public UmaEmpresa(UmaEmpresa e){
 this.cod=e.getcod();
 this.nome=e.getnome();
 this.gps=e.getgps();
 this.nif=e.getnif();
 this.raio=e.getraio(); 
 this.custotransporte=e.getcustotransporte();
 this.pass=e.getpass();
 this.avaliacoes = e.getAvaliacoes();
 }
 
 public String getpass(){
 return this.pass;    
 }
 
 public void setpass(String e){
 this.pass=e;    
 }
 
 public String getcod(){
 return this.cod;    
 }
 
 public int getnif(){
 return this.nif;    
 }
 
 public String getnome(){
 return this.nome;    
 }
 public double getraio(){
 return this.raio;    
 }
 public double getcustotransporte(){
 return this.custotransporte;    
 }
 
 public Coordenadas getgps(){
 return this.gps;    
 }
 
 public void setcod(String e){
 this.cod=e;    
 }
 
 public void setnif(int e){
 this.nif=e;    
 }
 
 public void setnome(String e){
 this.nome=e;    
 }
 public void setraio(double e){
 this.raio=e;    
 }
 public void setcustotransporte(double e){
 this.custotransporte=e;    
 }

 public void setgps(Coordenadas e){
 this.gps=e;    
 }
 
 public double getx(){
 return gps.getX();    
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

    
 
 public double gety(){
 return gps.getY();           
 }
 public UmaEmpresa clone(){
 return new UmaEmpresa(this);    
 }
 
 public double sinalizar(){
 return 1;     
 }
 
 public String toString(){
     StringBuilder sb = new StringBuilder();
     sb.append("\nTransportadora: ");
     sb.append("\nCodigo: " + cod);
     sb.append("\nNome: " + nome);
     sb.append("\nGPS: " + gps);
     sb.append("\nNIF: " + nif);
     sb.append("\nRaio: " + raio);
     sb.append("\nPreço por KM: " + custotransporte);
     return sb.toString();
 }
 
 public ArrayList<Integer> getAvaliacoes(){
        return new ArrayList<>(avaliacoes);
 }
 public void setAvaliacao(ArrayList<Integer> avaliacoes){
    this.avaliacoes = new ArrayList<>(avaliacoes);
 }
    
 public void addAvaliacao(int a){
 avaliacoes.add(a);  
 }
   
 public int mediaavaliaca(){
 int media = 0; 
 int quantos = 0; 
 for(int e : avaliacoes){
 media += e;   
 quantos +=1; 
 }
 if(quantos == 0) return 0; 
 return media/quantos;  
 }
 
 public boolean disponivel(){
 return this.disponivel;  
 }
    
 public void setdisponibilidade(boolean e){
 this.disponivel=e;     
 }
}