import java.util.ArrayList;
public class Loja{
 private String codigoLoja;
 private String nome; 
 private Coordenadas gps; 
 private String password;
 private ArrayList<Integer> avaliacoes;
 
 //construtores
 public Loja(){
 this.codigoLoja=new String();
 this.nome=new String();
 this.gps=new Coordenadas(); 
 this.password=new String();
 this.avaliacoes = new ArrayList<Integer>();
 }
 
 public Loja(String codigoLoja, String nome ,Coordenadas gps,String e){
 this.codigoLoja=codigoLoja;
 this.nome=nome;
 this.gps=gps;
 this.password=e; 
 this.avaliacoes = new ArrayList<Integer>();

 }
 
 public Loja(Loja e){
 this.codigoLoja=e.getcodigoLoja(); 
 this.nome=e.getnome();
 this.gps=e.getgps();
 this.password=e.getpassword();
 this.avaliacoes = e.getAvaliacoes(); 
 }
 
 //getters e setters
 public String getpassword(){
 return this.password;    
 }
 
 public void setpassword(String e){
 this.password=e;    
 }
 
 public String getnome(){
 return this.nome;    
 }
 
 public String getcodigoLoja(){
 return this.codigoLoja;    
 }

 public Coordenadas getgps(){
 return this.gps;     
 }

 public void setnome(String e){
 this.nome=e;    
 }
 
 public double getx(){
 return gps.getX();    
 }
    
 public double gety(){
 return gps.getY();           
 }
 
 public void setcodigoLoja(String e){
 this.codigoLoja=e;    
 }

 public void setgps(Coordenadas e){
 this.gps=new Coordenadas(e);    
 }
 
 public Loja clone(){
 return new Loja(this);     
 }
 
 public boolean encomendaReady()throws InterruptedException{ //encomenda pronta
 Thread.sleep(1000);
 System.out.println("Encomenda pronta");
 return true;
 }
 
 public String toString(){
 StringBuilder sb = new StringBuilder();  
 sb.append("\nLoja: "); 
 sb.append("\nCodigo da loja: "+ codigoLoja);
 sb.append("\nNome da loja: " + nome);
 sb.append("\nCoordenadas: " + gps);
 return sb.toString();
 }
 
 public boolean estado (boolean e){ // estado da encomenda
 return e;    
 }
 
 public double distancia2pontos(Coordenadas a, Coordenadas b){
 double distancia= Math.sqrt((a.getX() - b.getX()) * 2 + (a.getY()-b.getY())*2); 
 return distancia;
 }
 
 //avaliacoes
 public ArrayList<Integer> getAvaliacoes(){
        return new ArrayList<>(avaliacoes);
 }
 
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
 media += e;   
 quantos++;
 }
 if(quantos == 0) return 0; 
 return media/quantos;
 }
}