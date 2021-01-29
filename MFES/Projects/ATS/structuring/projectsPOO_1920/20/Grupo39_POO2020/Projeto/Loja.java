
/**
 * Write a description of class Loja here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.util.ArrayList;
import java.util.Optional;
import java.io.Serializable;

public class Loja implements Serializable
{
    
    private GestaoEnc encomendas;
    private ArrayList<LinhaEncomenda> linhas; 
    
    private String cod;
    private String nome;
    private GPS local;   
    private double tempo; //tempo de espera
    
    public Loja(String cod, String nome, GPS local)
    {
        this.encomendas = new GestaoEnc();
        
        this.cod = cod;
        this.nome = nome;
        this.local = local;
        this.tempo = 0; //tempo de espera
        this.linhas = new ArrayList<LinhaEncomenda> ();
    }

    public String getNome(){
        return this.nome;
    }
    
    public String getCod(){
        return this.cod;
    }
    
    public ArrayList<LinhaEncomenda> getLinhas(){
        return this.linhas;
    }
    
    public void setTempo(double tempo){
        //cada unidade corresponde a uma hora
        this.tempo = tempo;
    }
    
    public double getTempo(){
        return this.tempo;
    }
    
    public GPS getLocal(){
        return this.local;
    }
    
    public void add (Encomenda e){
        this.encomendas.addEncomenda(e);
    }
    
    public void remove (String cod){
        this.encomendas.removeEncomenda (cod);
    }

    public boolean inLoja (String cod){
        return this.encomendas.contains(cod);        
    }
    
    public Encomenda getEncomenda (String cod){
        return this.encomendas.getEncomenda(cod).cloneEncomenda();
    }
    
    public int adicionarProdutoDisponivel(LinhaEncomenda produto){
        if (this.linhas.stream().anyMatch(linha -> produto.equals(linha))){
            return -1;
        }
        this.linhas.add(produto);
        return 0;
    }
    
    public LinhaEncomenda getLinha (String produto){
        //recebe nome do produto;
        Optional <LinhaEncomenda> res = this.linhas.stream().filter(linha -> produto.equals(linha.getDescricao())).findFirst();
        
        if (res.isPresent()){
            return res.get().clone();            
        }
        else{
            return null ;
        }
    }
}
