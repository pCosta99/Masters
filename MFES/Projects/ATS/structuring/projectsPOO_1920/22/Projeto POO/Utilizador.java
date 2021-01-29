
/**
 * Escreva a descrição da classe Utilizador aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.*;
import java.util.stream.Collectors;
public class Utilizador
{
    private String nome,codigo;
    private double x,y;
    private List<Pedidos> p;
    public Utilizador(){
    this.nome=""; 
      this.codigo="";
      this.x=0;
      this.y=0; 
      this.p=new ArrayList<Pedidos>();
    
    }
    
     public Utilizador(String name,String cod,double x,double y,List<Pedidos> pi){
       this.nome=name; 
       this.codigo=cod;
       this.x=x;
       this.y=y;
       this.setPedidos(pi);
    }

    //copy
    public Utilizador(Utilizador u) {
       this.nome=u.getNome(); 
       this.codigo=u.getCodigo();
       this.x=u.getX();
       this.y=u.getY();
       this.p=u.getPedidos();
      }

      //retorna variaveis dentro da classe
      public String getNome() {
        return this.nome;
      }
      public String getCodigo() {
        return this.codigo;
      }
      public double getX() {
        return this.x;
      }
      public double getY() {
        return this.y;
      }
      
      public List<Pedidos> getPedidos(){
        return this.p.stream().map(Pedidos::clone).collect(Collectors.toList());
    } 
      //recebe uma variavel e poe na classe


        public void setNome(String novoNome) {
          this.nome = novoNome;
        }
        public void setCodigo(String novoCodigo) {
          this.codigo = novoCodigo;
        }

        public void setX(double novoX) {
          this.x = novoX;
        }
        public void setY(double novoY) {
          this.y= novoY;
        }

        public void setPedidos(List<Pedidos> novosp){
          this.p=novosp.stream().map(Pedidos::clone).collect(Collectors.toList());
        }
        
        
        public boolean equals(Object o){
        if(this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        
        Utilizador umUtilizador = (Utilizador) o;
        return (this.codigo.equals(umUtilizador.getCodigo()));
    } 

        public String toString() {
            StringBuilder sb = new StringBuilder();
      sb.append("Utilizador:");
      sb.append(this.codigo);
      sb.append(","+this.nome); 
      sb.append(","+this.x);
      sb.append(","+this.y);
       for( Pedidos pj:this.p){
      sb.append(","+pj.toString());}
      
      return sb.toString();
         }

        public Utilizador clone(){
        return new Utilizador(this);
        }
    
        public int AllEncomendasEntreguesUtilizador(Parse p){
    List<Encomendas> encomenda = new ArrayList<Encomendas>();
    List<Encomendas> encomendas = new ArrayList<Encomendas>();
    for (Encomenda enc: p.getAE()){
        if (enc.getCodutilizador().equals(codigo)) encomenda.add(enc);
    }
    
    for (EncomendaMedica enc: p.getAEM()){
        if (enc.getCodutilizador().equals(this.codigo)) encomenda.add(enc);
    }
    for(Encomendas enc:encomenda){
        for(Lojas loj:p.getAL()){
            if(loj.getE().equals(enc.getCodencomenda()))encomendas.add(enc);
        }
    }
    
     return encomenda.size();
}
        

public void adicionaLinha(Pedidos l){
        this.p.add(l);
        }
}

    

