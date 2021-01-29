/**
 * Classe abstrata Transporte - escreva a descrição da classe aqui
 * 
 * @author (seu nome aqui)
 * @version (versão ou data)
 */
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.HashSet;
import java.util.Set;
import java.io.Serializable;
public abstract class Transporte implements Serializable
{
    // variáveis de instância
    private String codigo;
    private String nome;
    private double raio;
    private GPS localizacao;
    private List<Integer> avaliacoes;
    private Set<Encomenda> registos;
    private boolean aptoMed;
    
    
    /**
     * Construtor por omissão da classe Transporte.
     */
    public Transporte()
    {
       this.codigo = new String();
       this.nome = new String();
       this.raio = 0.0;
       this.localizacao = new GPS();
       this.avaliacoes = new ArrayList<>();
       this.registos = new HashSet<>();
       this.aptoMed = false;
       
      
    }
    
    /**
     * Construtor parametrizado da classe Transporte.
     */
    public Transporte(String codigo, String nome, double raio, GPS localizacao, List<Integer> avaliacoes, Set<Encomenda> registos, 
                        boolean apto)
    {
       this.setCodigo(codigo);
       this.setNome(nome);
       this.setRaio(raio);
       this.setLocalizacao(localizacao);
       this.setAvaliacoes(avaliacoes);
       this.setRegistos(registos);
       this.setAptoMed(apto); 
    }
    
    /**
     * Construtor de cópia da classe Transporte.
     */
    public Transporte(Transporte t)
    {
       this.setCodigo(t.getCodigo());
       this.setNome(t.getNome());
       this.setRaio(t.getRaio());
       this.setLocalizacao(t.getLocalizacao());
       this.setAvaliacoes(t.getAvaliacoes());
       this.setRegistos(t.getRegistos());
       this.setAptoMed(t.getAptoMed());  
    }
    
    //Getters
    public String getCodigo(){
        return this.codigo;
    }
    
    public String getNome(){
        return this.nome;
    }
    
    public double getRaio(){
        return this.raio;
    }
    
    public GPS getLocalizacao(){
        return this.localizacao;
    }
    
    public List<Integer> getAvaliacoes(){
        List<Integer> novo = new ArrayList<Integer>();
        Iterator<Integer> iter = this.avaliacoes.iterator(); 
        while (iter.hasNext()){
                Integer elem = iter.next();
                novo.add(elem);
        }
        return novo;
    }
    
    public Set<Encomenda> getRegistos(){
        Set<Encomenda> novo = new HashSet<Encomenda>();
        Iterator<Encomenda> iter = this.registos.iterator(); 
        while (iter.hasNext()){
                Encomenda elem = iter.next();
                novo.add(elem.clone());
        }
        return novo;
    }
    
    public boolean getAptoMed(){
        return this.aptoMed;
    }
    
    

    //Setters
    public void setCodigo(String codigo){
        this.codigo = codigo;
    }
    
    public void setNome(String nome){
        this.nome = nome;
    }
    
    public void setRaio(double raio){
        this.raio = raio;
    }
    
    public void setLocalizacao(GPS localizacao){
        this.localizacao = localizacao;
    }
    
    public void setAvaliacoes(List<Integer> avaliacoes){
        List<Integer> novo = new ArrayList<Integer>();
        for (Integer avaliacao : avaliacoes){
            novo.add(avaliacao);
        }
        this.avaliacoes = novo;
    }
    
    public void setRegistos(Set<Encomenda> registos){
        Set<Encomenda> novo = new HashSet<Encomenda>();
        for (Encomenda registo : registos){
            novo.add(registo.clone());
        }
        this.registos = novo;
    }
    
    public void setAptoMed(boolean apto){
        this.aptoMed = apto;
    }
    
    /**
     * Metodo que faz uma copia do objecto receptor da mensagem.
     * Para tal invoca o construtor de copia.
     */
    public abstract Transporte clone();
    
    /**
     *  Metodo que devolve a representaçao em String da Transporte.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\nCodigo: ").append(this.codigo).append("\n");
        sb.append("Nome: ").append(this.nome).append("\n");
        sb.append("Raio: ").append(this.raio).append("\n");
        sb.append("localizacao: ").append(this.localizacao).append("\n");
        sb.append("Classificaçoes: ").append(this.avaliacoes).append("\n");
        sb.append("Registos: ").append(this.registos).append("\n");
        sb.append("Apto para receber encomendas medicas: ").append(this.aptoMed).append("\n");
        return sb.toString();
    }
    
    /**
     * Metodo que determina se ois transportes sao iguais.
     * 
     */
    public boolean equals(Object obj) {
      if(this == obj) return true;
      if(obj == null && this.getClass() != obj.getClass()) return false;
      Transporte t = (Transporte) obj;     
      return this.codigo.equals(t.getCodigo()) &&
             this.nome.equals(t.getNome()) &&
             this.raio == t.getRaio() &&
             this.localizacao.equals(t.getLocalizacao()) &&
             this.avaliacoes.equals(t.getAvaliacoes()) &&
             this.aptoMed == t.getAptoMed() &&
             this.registos.equals(t.getRegistos());       
    }
    
    /**
     * Método que dá a classificaçao do transporte.
     * Calcula a media da lista de avaliaçoes.
     */
    public double classificacao(){
        double media = 0;
        double soma = 0;
        for (Integer a : this.avaliacoes){
            soma += a; 
        }
        if (!this.avaliacoes.isEmpty())
            media = soma / (this.avaliacoes.size());
        return media;
    }

    public abstract boolean recolheEncomenda(Encomenda e,Lojas l,Utilizadores u);
    public abstract double tempoDeEntrega(Encomenda e,Lojas l,Utilizadores u);
    public abstract boolean aceitoTransporteMedicamentos();
    public abstract void aceitaMedicamentos(boolean state);
    public abstract String tipoTransporte();
    public abstract String toStringCSV();
}
