import java.io.BufferedWriter;
import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.FileReader;
import java.util.List;
import java.util.ArrayList;
import java.time.LocalDate;
import java.time.Duration;
/**
 * Escreva a descrição da classe Encomenda aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Encomenda
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private String codEncomenda;
    private String codloja;
    private double peso;
    private List<LinhaEncomenda> produto; 
    private String codbuyer;
    private String codDeliver; //codigo de quem entrega o produto (empresa ou voluntariado)
    private Duration duracao; //still gotta do 
    private int estado; // 1 entregue
                        // 2 em fase de preparação pela loja para voluntario
                        // 3 disponivel na loja para voluntario ir buscar
                        // 4 disponivel na loja transportadora ir buscar  
                        // 5 em fase de preparação pela loja para transportadora
                        // 6 empresa ou voluntario esta a entregar encomenda 
                        // 7 entregue e avaliada
                        // 8 pedido ao voluntario --not sure if i got the time
                        
    private LocalDate data; //data de entrega 
    
    
    /**
     * Construtor para objetos da classe Produto
     */
    public Encomenda()
    {
        this.codEncomenda="";
        this.peso=0;
        this.produto= new ArrayList<>() ;
        this.codbuyer = "";
        this.codloja = "";
        this.codDeliver = "";
        this.estado=2;
        this.data =null ; 
    }   
    
    public Encomenda(String codEncomenda, String buyer ,String loja,String transport,double peso, 
      LocalDate data, int estado,List<LinhaEncomenda> produto){
        this.codEncomenda =codEncomenda;
        this.peso=peso;
        this.produto = produto;
        this.codbuyer = buyer;
        this.codloja = loja;
        this.codDeliver = transport;
        this.data=data;
        this.estado=estado;
    }
    
    public Encomenda(Encomenda e){
        this.codEncomenda = e.getcodEncomenda();
        this.peso=e.getpeso();
        this.produto=e.getproduto(); 
        this.codbuyer = e.getBuyer();
        this.codloja = e.getLoja();
        this.codDeliver = e.getcodDeliver();
        this.estado=e.getestado();
    }

    public String getBuyer(){
        return this.codbuyer;
    }
            
    public void setcodDeliver(String transport){
        this.codDeliver = transport;
    }
    
    public String getcodDeliver(){
        return this.codDeliver;
    }
            
    public void setBuyer(String buyer){
        this.codbuyer = buyer;
    }
    
    public void setLoja(String loja){
        this.codloja = loja;
    }
    
    public String getLoja(){
        return this.codloja;
    }
    
    public double getpeso(){
           return this.peso;
    }
    
    public void setpeso(double peso){
           this.peso=peso;
    }
    
    public int getestado(){
           return this.estado;
    }
    
    public void setestado(int estado){
           this.estado=estado;
    }
    
    public String getcodEncomenda(){
           return this.codEncomenda;
    }
    
    public void setcodEncomenda(String codEncomenda){
           this.codEncomenda=codEncomenda;
    }
    
    public List<LinhaEncomenda> getproduto(){
           return this.produto;
    }
    
    public void setproduto(List<LinhaEncomenda> produto){
           this.produto=produto;
    }
    
    public Encomenda clone(){
        return new Encomenda(this);
    }
    
    public LocalDate getData(){
        return this.data;
    }
    
    public void setData(LocalDate d){
        this.data=d;
    }
    
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass()!=this.getClass()) return false;
        Encomenda v=(Encomenda) obj;
        return this.getcodEncomenda()==v.getcodEncomenda() &&
               this.getpeso()==v.getpeso() &&
               this.getcodEncomenda()==v.getcodEncomenda();
    }
    
    public String toString(){
        StringBuilder res = new StringBuilder();       
        res.append(this.getcodEncomenda());
        res.append("\n");
        res.append(this.getpeso());
        res.append("\n");
        res.append(this.getBuyer().toString());
        res.append("\n");
        System.out.println(res.toString());
        return(res.toString());
    }
    
    public String stringtoFile(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda:"+this.getcodEncomenda());
        sb.append(","+this.getBuyer());
        sb.append(","+this.getLoja());
        sb.append(","+this.getcodDeliver());
        sb.append(","+this.getpeso());
        sb.append(","+this.getData());
        sb.append(","+this.getestado());
        
        for(LinhaEncomenda l : this.produto){
            sb.append(","+l.stringtoFile());
        }
        return(sb.toString());
    }

}
