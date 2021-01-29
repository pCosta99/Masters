import java.util.ArrayList;
public class UmaLoja{
 private String codloja;
 private String nome; 
 private Coordenadas gps; 
 private String pass;
 private ArrayList<Integer> avaliacoes;
 
 public UmaLoja(){
 this.codloja=new String();
 this.nome=new String();
 this.gps=new Coordenadas(); 
 this.pass=new String();
 this.avaliacoes = new ArrayList<Integer>();
 }
 
 public UmaLoja(String codloja, String nome ,Coordenadas gps,String e){
 this.codloja=codloja;
 this.nome=nome;
 this.gps=gps;
 this.pass=e; 
 this.avaliacoes = new ArrayList<Integer>();

 }
 
 public UmaLoja(UmaLoja e){
 this.codloja=e.getcodloja(); 
 this.nome=e.getnome();
 this.gps=e.getgps();
 this.pass=e.getpass();
 this.avaliacoes = e.getAvaliacoes(); 
 }
 
 public String getpass(){
 return this.pass;    
 }
 
 public void setpass(String e){
 this.pass=e;    
 }
 
 public String getnome(){
 return this.nome;    
 }
 public String getcodloja(){
 return this.codloja;    
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
 
 public void setcodloja(String e){
 this.codloja=e;    
 }

 public void setgps(Coordenadas e){
 this.gps=new Coordenadas(e);    
 }
 
 public UmaLoja clone(){
 return new UmaLoja(this);     
 }
 
 public boolean encomendapronta()throws InterruptedException{
 Thread.sleep(1000);
 System.out.println("Encomenda pronta");
 return true;
 }
 
 public String toString(){
 StringBuilder sb = new StringBuilder();  
 sb.append("\nLoja: "); 
 sb.append("\nCodigo da loja: "+ codloja);
 sb.append("\nNome da loja: " + nome);
 sb.append("\nCoordenadas: " + gps);
 return sb.toString();
 }
 
 public boolean estado (boolean e){
 return e;    
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
 int quantos = 0; 
 for(int e : avaliacoes){
 media += e;   
 quantos +=1; 
 }
 if(quantos == 0) return 0; 
 return media/quantos;  
 }

}