import java.util.ArrayList;

public class Empresa{
 private String codigoEmpresa;
 private String nome;
 private Coordenadas gps;
 private int nif;
 private double raio; 
 private double custotransporte;
 private String password; 
 private ArrayList<Integer> avaliacoes;
 private boolean disponivel; 
 
 //construtores
 public Empresa(){
 this.codigoEmpresa=new String();
 this.nome=new String();
 this.gps=new Coordenadas();
 this.nif=0;
 this.raio=0.0;
 this.custotransporte=0.0;
 this.password=new String();
 this.avaliacoes = new ArrayList<Integer>();
 this.disponivel = true; 
 }
 public Empresa(String codigoEmpresa,String nome,Coordenadas gps,int nif, double raio, double custo,String e){
 this.codigoEmpresa=codigoEmpresa;
 this.nome=nome;
 this.raio=raio;
 this.nif=nif;
 this.custotransporte=custo;
 this.gps=gps;
 this.password=e;
 this.avaliacoes = new ArrayList<Integer>();
 this.disponivel = true; 

 }
 public Empresa(Empresa e){
 this.codigoEmpresa=e.getcodigoEmpresa();
 this.nome=e.getnome();
 this.gps=e.getgps();
 this.nif=e.getnif();
 this.raio=e.getraio(); 
 this.custotransporte=e.getcustotransporte();
 this.password=e.getpassword();
 this.avaliacoes = e.getAvaliacoes();
 }
 
 //setters e getters
 public String getpassword(){
 return this.password;    
 }
 
 public void setpassword(String e){
 this.password=e;    
 }
 
 public String getcodigoEmpresa(){
 return this.codigoEmpresa;    
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
 
 public void setcodigoEmpresa(String e){
 this.codigoEmpresa=e;    
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
    
 public double distancia2pontos(Coordenadas a, Coordenadas b){
 double distancia= Math.sqrt((a.getX() - b.getX()) * 2 + (a.getY()-b.getY())*2); 
 return distancia;
 }
 
 public double gety(){
 return gps.getY();           
 }
 
 public Empresa clone(){
 return new Empresa(this);    
 }
 
 public double sinalizar(){
 return 1;     
 }
 
 public String toString(){
     StringBuilder sb = new StringBuilder();
     sb.append("\nTransportadora: ");
     sb.append("\ncodigoEmpresaigo: " + codigoEmpresa);
     sb.append("\nNome: " + nome);
     sb.append("\nGPS: " + gps);
     sb.append("\nNIF: " + nif);
     sb.append("\nRaio: " + raio);
     sb.append("\nPreço por KM: " + custotransporte);
     return sb.toString();
 }
 
 //avaliacoes
 public ArrayList<Integer> getAvaliacoes(){
        return new ArrayList<>(avaliacoes);
 }
 
 public void setAvaliacao(ArrayList<Integer> avaliacoes){
    this.avaliacoes = new ArrayList<>(avaliacoes);
 }
    
 public void addAvaliacao(int a){
 avaliacoes.add(a);  
 }
   
 public int media(){
 int media = 0; 
 int quantos = 0; 
 for(int e : avaliacoes){
 media += e;   
 quantos +=1; 
 }
 if(quantos == 0) return 0; 
 return media/quantos;  
 }
 
 //disponibilidade
 public boolean disponivel(){
 return this.disponivel;  
 }
    
 public void setdisponibilidade(boolean e){
 this.disponivel=e;     
 }
}