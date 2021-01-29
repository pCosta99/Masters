
/**
 * Classe abstrata Encomendas - escreva a descrição da classe aqui
 * 
 * @author (seu nome aqui)
 * @version (versão ou data)
 */
import java.util.*;
import java.time.LocalDate;
import java.util.stream.*;
public abstract class Encomendas
{
    private String codencomenda,codutilizador,codloja;
    private double peso; 
    private LocalDate tempo,tempo1;
    
   
    private List<LinhaDeEncomenda> linha;
    
   public Encomendas(){
       this.codencomenda="";
       this.codutilizador=""; 
       this.codloja="";
       this.peso=0;
       this.linha=new ArrayList<>();
       this.tempo=LocalDate.now();
       this.tempo1=LocalDate.now();
       
    }
       
   public Encomendas(String coden,String codut,String codlo,double auxpeso,List<LinhaDeEncomenda> l, LocalDate t,LocalDate t1){
       this.codencomenda=coden;
       this.codutilizador=codut; 
       this.codloja=codlo;
       this.peso=auxpeso;
       this.setLinhas(l);
       this.tempo=t;
       this.tempo1=t1;
       
    }
    
    
   public Encomendas(Encomendas enc) {
       this.codencomenda=enc.getCodencomenda();
       this.codutilizador=enc.getCodutilizador(); 
       this.codloja=enc.getCodloja();
       this.peso=enc.getPeso();
       this.linha=enc.getLinha();
       this.tempo=enc.getTempo();
       this.tempo1=enc.getTempo1();
      
      }
      
   public String getCodencomenda() {
        return this.codencomenda;
      }
      
   public String getCodutilizador() {
        return this.codutilizador;
      }
   
     
      
   public String getCodloja() {
        return this.codloja;
      }
   
   public double getPeso() {
        return this.peso;
      }
   
   public List<LinhaDeEncomenda> getLinha(){
        return this.linha.stream().map(LinhaDeEncomenda::clone).collect(Collectors.toList());
    } 
    
   public LocalDate getTempo(){
    return this.tempo;
    }
   public LocalDate getTempo1(){
    return this.tempo1;
    }
   public void setCodencomenda(String novoCodencomenda) {
          this.codencomenda = novoCodencomenda;
        }
   
   public void setCodutilizador(String novoCodutilizador) {
          this.codutilizador = novoCodutilizador;
        }
   
   public void setCodloja(String novoCodloja) {
          this.codloja = novoCodloja;
        }
   
       
        
   public void setPeso(double novoPeso) {
          this.peso = novoPeso;
        }
   
   public void setLinhas(List<LinhaDeEncomenda> novaLinha){
          this.linha=novaLinha.stream().map(LinhaDeEncomenda::clone).collect(Collectors.toList());
    } 
   
   public void setTempo(LocalDate novotempo) {
          this.tempo = novotempo;
        }
        
   public void setTempo1(LocalDate novotempo1) {
          this.tempo1 = novotempo1;
        }     
        
   public boolean equals(Object o){
        if(this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        
        Encomendas umEncomenda = (Encomendas) o;
        return (this.codencomenda.equals(umEncomenda.getCodencomenda()));
    } 
       
   public abstract Encomendas clone();
   
   public abstract String toString();
        
   public void adicionaLinha(LinhaDeEncomenda l){
        this.linha.add(l);
        }
        
   public double calculaValorTotal(){
        Iterator<LinhaDeEncomenda> it = this.linha.iterator();
        double total = 0;
        LinhaDeEncomenda l = new LinhaDeEncomenda();
        while(it.hasNext()){
            l = it.next();
            total += l.calculaValorLinhaEnc();
        }
        return total;
    }
    
   public int numeroTotalProdutos(){
        Iterator<LinhaDeEncomenda> it = this.linha.iterator();
        int produtos = 0;
        LinhaDeEncomenda l = new LinhaDeEncomenda();
        while(it.hasNext()){
            l = it.next();
            produtos += l.getQuantidade();
        }
        return produtos;
    }
     
   public void removeProduto(String codProd){
        Iterator<LinhaDeEncomenda> it = this.linha.iterator();
        boolean encontrado = false;
        LinhaDeEncomenda l = new LinhaDeEncomenda();
        while(it.hasNext() && !encontrado){
            l = it.next();
            if(l.getCod().equals(codProd)){
                encontrado = true;
                it.remove();
            }
        }
    }
}
