import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.Random;
import java.util.stream.Collectors;
import java.lang.Object;
import java.io.Serializable;

public class Transportador extends Utilizador implements Serializable{
    
    //Variaveis de instância
    private double classificacao; //classificaçao do serviço de entrega
    private double raio; //raio de açao
    private List<Avaliacao> avaliacoes; //avaliacoes
    private List<Encomenda> historico; //historico de encomendas entregues
    

    /**
     * Construtor vazio
     */
    public Transportador(){
       super("Transportador","",null,"","","");
       classificacao = 0;
       raio = 0;
       avaliacoes = new ArrayList<Avaliacao>();
       historico = new ArrayList<Encomenda>();
      
    }

    
    /**
     * Construtor por parametrizado
     */
    public Transportador(String id, String nome, Coordenadas coordenadas, String email, 
    String password, String morada, double classificacao, double raio, ArrayList<Avaliacao> avaliacoes,
    ArrayList<Encomenda> historico){
        super("Transportador","",null,"","",""); 
        this.classificacao = classificacao;
        this.raio = raio;
        this.avaliacoes = avaliacoes;
        this.historico = historico;
       ;
    }

    /**
     * Construtor por cópia
     */
    public Transportador(Transportador t){
        super(t);
        this.classificacao = t.getClassificacao();
        this.raio = t.getRaio();
        this.avaliacoes = t.getAvaliacoes();
        this.historico = t.getHistorico();
       
    }

    //Getters

    public double getClassificacao(){
        return this.classificacao;
    }

    public double getRaio(){
        return this.raio;
    }

    public List<Avaliacao> getAvaliacoes(){
        if(this.avaliacoes == null) 
        return new ArrayList<Avaliacao>();
        
        else return this.avaliacoes;
    }

    public List<Encomenda> getHistorico(){
        if(this.historico == null)
        return new ArrayList<Encomenda>();
        
        else return this.historico;
    }
    
    
    //Setters

    public void setClassificacao(double c){
        this.classificacao = c;
    }

    public void setAvaliacoes(List<Avaliacao> a){
        this.avaliacoes = a;
    }

    public void setHistorico(ArrayList<Encomenda> h){
        this.historico = h.stream().collect(Collectors.toList());
    }
    
    public void addEntrega(Encomenda e){
        this.getHistorico().add(e);
    }
    
   

    public double getMediaAvaliacoes(){
        double n = 0;
    
        n = avaliacoes.stream().mapToDouble(o -> o.getAvaliacao()).sum();
        return (n/(avaliacoes.size()));
    }

    public void addAvaliacao(Avaliacao a){
        if(a != null){
            avaliacoes.add(a);
            setClassificacao(getMediaAvaliacoes());
        }
        else return;
    }
    
    /**
     * Metodo Equals
     */
    public boolean equals(Object o){
        if (this == o) 
            return true;
    
        if (o == null || o.getClass() != this.getClass()) 
            return false;
    
        Transportador t = (Transportador) o;

        return this.classificacao == t.getClassificacao() &&
        this.raio == t.getRaio() &&
        t.getAvaliacoes().equals(avaliacoes) &&
        t.getHistorico().equals(historico);
     
    }
  
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("A classificação do transportador é: " + this.getClassificacao() + "\n");
        sb.append("O histórico de transporte de encomendas é: " + this.getHistorico() + "\n");
        return sb.toString();
    }
    
    //Clone
    
    public Transportador clone(){
        return new Transportador(this);
    }
    
} 