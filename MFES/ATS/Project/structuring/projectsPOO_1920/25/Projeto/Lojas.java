
import java.util.Map;
import java.util.LinkedHashMap;
import java.io.Serializable;
public class Lojas implements Login, Serializable
{
    // variáveis de instância 
    private String nome;
    private GPS localizaçao;
    private Map<String,Encomenda> filaEspera;
    private String codigo;
    private double tempoMedioPessoa; 
    private String password;

    /**
     * Construtor por omissão da classe Lojas
     */
    public Lojas()
    {
       this.nome = new String();
       this.localizaçao = new GPS();
       this.codigo = new String();
       this.filaEspera= new LinkedHashMap<>();
       this.tempoMedioPessoa = 5.0;
       this.password = new String();
    }
    
    /**
     * Construtor parametrizado da classe Lojas
     */
    public Lojas(final String nome, final String codigo, final GPS localizaçao, final Map<String,Encomenda> filaEspera, final double tempo, final String pass)
    {
       this.setNome(nome);
       this.setCodigo(codigo);
       this.setLocalizaçao(localizaçao);
       this.setFilaEspera(filaEspera);
       this.setTempoMedioPessoa(tempo);
       this.setPassword(pass);
    }
    
    /**
     * Construtor de cópia da classe Lojas
     */
    public Lojas(final Lojas l)
    {
        this.setNome(l.getNome());
        this.setCodigo(l.getCodigo());
        this.setLocalizaçao(l.getLocalizaçao());
        this.setFilaEspera(l.getFilaEspera());
        this.setTempoMedioPessoa(l.getTempoMedioPessoa());
        this.setPassword(l.getPassword());
    }
    
    //getters
    
    public String getNome(){
        return this.nome;
    }
    
    public String getCodigo(){
        return this.codigo;
    }
    
    public GPS getLocalizaçao(){
        return this.localizaçao;
    }
    
    public Map<String,Encomenda> getFilaEspera(){
        final Map<String,Encomenda> ret = new LinkedHashMap<>();
        for (final Map.Entry<String,Encomenda> e : this.filaEspera.entrySet()){
            ret.put(e.getKey(), e.getValue().clone());
        }
        return ret;   
    }
    
    public double getTempoMedioPessoa(){
        return this.tempoMedioPessoa;
    }
    
    public String getPassword(){
        return this.password;
    }
    
    //setters
    
    public void setNome(final String nome){
        this.nome = nome;
    }
    
    public void setCodigo(final String codigo){
        this.codigo = codigo;
    }
    
    public void setLocalizaçao(final GPS localizaçao){
        this.localizaçao = localizaçao;
    }
    
    public void setFilaEspera(final Map<String,Encomenda> f){
        this.filaEspera = new LinkedHashMap<>();
        f.entrySet().forEach(e -> this.filaEspera.put(e.getKey(), e.getValue().clone()));
    }
    
    public void setTempoMedioPessoa(final double t){
        this.tempoMedioPessoa = t;
    }
    
    public void setPassword(final String pass){
        this.password = pass;
    }
    
    /**
     * Metodo que faz uma copia do objecto receptor da mensagem.
     * Para tal invoca o construtor de copia.
     */
    public Lojas clone() {
        return new Lojas(this);
    }
    
    /**
     *  Metodo que devolve a representaçao em String da EmpresasTransportadora.
     */
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Nome da loja:   " + this.nome + "\n");
        sb.append("Codigo da loja:   " + this.codigo + "\n");
        sb.append("Localizaçao da loja:   " + this.localizaçao + "\n");
        sb.append("Fila de Espera da loja:   " + this.filaEspera + "\n");
        sb.append("Tempo que cada pessoa demora a ser atendida em média:  " + this.tempoMedioPessoa + "\n");
        sb.append("\n");
        return sb.toString();
    }
    
    /**
     * Metodo que determina se duas empresas transporadoras sao iguais.
     *
     */
    public boolean equals(final Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;      
        final Lojas l = (Lojas) obj;
        return this.nome.equals(l.getNome()) &&
               this.codigo.equals(l.getCodigo()) &&
               this.localizaçao.equals(l.getLocalizaçao()) &&
               this.filaEspera.equals(l.getFilaEspera()) &&
               this.tempoMedioPessoa == l.getTempoMedioPessoa() &&
               this.password.equals(l.getPassword());
    }
    
    /**
     * Método que adiciona encomenda à fila de espera.
     */
    public void adicionaFilaEspera(final Encomenda e){
        this.filaEspera.put(e.getDestinatario(),e.clone());
    }
    
    /**
     * Método que retira encomenda da fila de espera.
    */
    public void retiraFilaEspera(final Encomenda e){
        final Map<String,Encomenda> novo = new LinkedHashMap<>();
        for (final Map.Entry<String,Encomenda> enc : this.filaEspera.entrySet()){
            if(!enc.getValue().equals(e)){
                novo.put(enc.getKey(),enc.getValue().clone());
            }
        }
        this.setFilaEspera(novo);
    }
    
    /**
     * Método que diz quantas pessoas estão na fila de espera.
     */
    public int numeroPessoasFila(){
        return this.filaEspera.size();
    }
    
    /**
     * Método que vê se uma dada encomenda se encontra na fila de espera.
     */
    public boolean encomendaPronta(final Utilizadores u){
        if(filaEspera.containsKey(u.getCodigo())){
            return true;
        }
        else{return false;}   
        
    }
    
    /**
     * Método que verifica credenciais de um login de uma loja.
     */
    public boolean verificaCredenciais(final String cod, final String pass){
        return (this.codigo.equals(cod) && this.password.equals(pass));
    }
    
    /**
     * Método que representa uma loja num ficheiro CSV.
     */
    public String toStringCSV(){
      final StringBuilder sb = new StringBuilder();
      sb.append("Loja:");
      sb.append(this.codigo).append(",").append(this.nome).append(",").append(this.localizaçao);
      return sb.toString();
    }
}