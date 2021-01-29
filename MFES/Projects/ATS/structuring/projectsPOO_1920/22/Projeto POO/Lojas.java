
/**
 * Escreva a descrição da classe Lojas aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.*;
import java.util.stream.Collectors;
public class Lojas
{    private List<String> entregues;
     private List<String> porEntregar;
     private List<String> apreparar;
     private List<LinhaDeEncomenda> stock;
     
     private boolean disponivel;
     private String codloja,nomeloja;
     private double x,y;
     
    public Lojas(){
       this.nomeloja=""; 
       this.codloja="";
       this.x=0;
       this.y=0;
       this.disponivel=true;
       this.porEntregar=new ArrayList<>();
       this.entregues=new ArrayList<>();
       this.apreparar=new ArrayList<>();
       this.stock=new ArrayList<>();
    }

    public Lojas(String name,String codl,double latitude ,double longitude,
        boolean disp,List<String> nentregues,List<String> nporentregar,
        List<String> napreparar,List<LinhaDeEncomenda> nstock){
       this.nomeloja=name; 
       this.codloja=codl;
       this.x=latitude;
       this.y=longitude;
       this.disponivel=disp;
       this.setPE(nentregues);
       this.setE(nporentregar);
       this.setAP(napreparar);
       this.setStock(nstock);
    }

    //copy
    public Lojas(Lojas loja) {
       this.nomeloja=loja.getNomeloja(); 
       this.codloja=loja.getCodloja();
       this.x=loja.getX();
       this.y=loja.getY();
       this.disponivel=loja.getDisp();
       this.porEntregar=loja.getPE();
       this.entregues=loja.getE();
       this.apreparar=loja.getAP();
       this.stock=loja.getStock();
      }

      //retorna variaveis dentro da classe
      public String getNomeloja() {
        return this.nomeloja;
      }
      public String getCodloja() {
        return this.codloja;
      }
      public double getX() {
        return this.x;
      }
      public double getY() {
        return this.y;
      }
      
      public boolean getDisp(){
        return this.disponivel;
        }
      
      public List<String> getPE(){
        return new ArrayList<>(this.porEntregar);
        }
        
       public List<String> getE(){
        return new ArrayList<>(this.entregues);
        }
       
       public List<String> getAP(){
        return new ArrayList<>(this.apreparar);
        }
        
       public List<LinhaDeEncomenda> getStock(){
        return this.stock.stream().map(LinhaDeEncomenda::clone).collect(Collectors.toList());
        } 
      
      //recebe uma variavel e poe na classe

        public void setNome(String novoNome) {
          this.nomeloja = novoNome;
        }
        public void setCodloja(String novoCodloja) {
          this.codloja = novoCodloja;
        }

        public void setX(double novoX) {
          this.x = novoX;
        }
        public void setY(double novoY) {
          this.y = novoY;
        }
        
        public void setDisp(boolean novodisp){
            this.disponivel=novodisp;
        }
        
        public void setPE(List<String> novape){
          this.porEntregar=new ArrayList<>(novape);
        } 
    
       public void setE(List<String> novae){
          this.entregues=new ArrayList<>(novae);
        }
        
        public void setAP(List<String> novaap){
          this.apreparar=new ArrayList<>(novaap);
        } 
    
       public void setStock(List<LinhaDeEncomenda> novost){
          this.stock=novost.stream().map(LinhaDeEncomenda::clone).collect(Collectors.toList());
        }
        
        public void mudarparaDisponivel()
        {
            this.setDisp(true);
        }
        
        public void mudarparaIndisponivel()
        {
            this.setDisp(false);
        }
        
        public int TempodeEspera(){
            return (this.apreparar.size())*24+24;
        }
        
        public void adicionaLinha(LinhaDeEncomenda l){
        this.stock.add(l);
        }
        
        public void adicionaLinha1(String l){
        this.apreparar.add(l);
        }
        public void adicionaLinha2(String l){
        this.porEntregar.add(l);
        }
        public void adicionaLinha3(String l){
        this.entregues.add(l);
        }
        
        public boolean equals(Object o){
        if(this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        
        Lojas umLojas = (Lojas) o;
        return (this.codloja.equals(umLojas.getCodloja()));
    }

        public String toString() {
            StringBuilder sb = new StringBuilder();
      sb.append("Loja:");
      sb.append(this.codloja);
      sb.append(","+this.nomeloja); 
      sb.append(","+this.x);
      sb.append(","+this.y);
      sb.append(","+this.disponivel);
      sb.append(","+"Entregues");
      for( String lj:this.entregues){
      sb.append(","+lj.toString());}
      
      sb.append(","+"Por Entregar");
      for( String lj:this.porEntregar){
      sb.append(","+lj.toString());}
            
     sb.append(","+"A preparar"); 
       for( String lj:this.apreparar){
      sb.append(","+lj.toString());}
      
      sb.append(","+"Stock");
      for( LinhaDeEncomenda lj:this.stock){
      sb.append(","+lj.toString());}
      
      
      return sb.toString();
         } 

        public Lojas clone(){
        return new Lojas(this);
        }
    }