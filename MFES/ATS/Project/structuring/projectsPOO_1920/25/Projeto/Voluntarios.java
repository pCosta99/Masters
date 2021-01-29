
/**
 * Escreva a descrição da classe Voluntários aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.HashSet;
import java.time.LocalDateTime; 
import java.util.List;
import java.util.Iterator;
import java.util.Set;
public class Voluntarios extends Transporte implements Login
{
    //Variaveis de Instancia
    private boolean disponivel;
    private String password;
    
    /**
     * Construtor por omissão da classe Voluntários
     */
    public Voluntarios()
    {
       super();
       this.disponivel = true;
       this.password = new String();
    }
    
    /**
     * Construtor parametrizado da classe Voluntários
     */
    public Voluntarios(String codigo, String nome, GPS localizacao, double raio,  boolean disponivel, boolean apto, List<Integer> avaliacoes, Set<Encomenda> registos, String pass)
    {
        super(codigo, nome, raio, localizacao, avaliacoes, registos, apto);
        this.setPassword(pass);
        this.setDisponivel(disponivel);
    }
    
    /**
     * Construtor de cópia da classe Voluntários
     */
    public Voluntarios(Voluntarios v)
    {
        super(v);
        this.setPassword(v.getPassword());
        this.setDisponivel(v.getDisponivel());
    }
    
    //Getters
    
    public String getPassword(){
        return this.password;
    }

    public boolean getDisponivel(){
        return this.disponivel;
    }
    
    //Setters
    
    public void setPassword(String pass){
        this.password = pass;
    }

    public void setDisponivel(boolean disp){
        this.disponivel = disp;
    }
    
    /**
     * Método que faz uma copia do objecto receptor da mensagem.
     * Para tal invoca o construtor de copia.
     */
    public Voluntarios clone() {
        return new Voluntarios(this);
    }
    
    /**
     *  Metodo que devolve a representaçao em String dos Voluntários.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString()).append("\n");
        sb.append("Disponibilidade: ").append(this.disponivel).append("\n");
        return sb.toString();
    }
    
    /**
     * Metodo que determina se dois voluntarios sao iguais.
     * 
     */
    public boolean equals(Object obj) {
        if(this == obj) return true;
        if(obj == null && this.getClass() != obj.getClass()) return false;
        Voluntarios v = (Voluntarios) obj;     
        return super.equals(v) && this.disponivel == v.getDisponivel() && this.password.equals(v.getPassword());
    }
    
    /**
     * Método que sinaliza que um voluntario esta disposto para recolher encomendas.
     */
    public boolean verDisp(){
        return this.getDisponivel();
    }
    
    /**
     * Método que escolhe ir buscar uma encomenda de um utilizador que é disponibilizada por uma loja.
     */
    public boolean recolheEncomenda(Encomenda e,Lojas l,Utilizadores u){
        if (this.getLocalizacao().distancia(this.getLocalizacao(),l.getLocalizaçao()) <= this.getRaio() && this.getLocalizacao().distancia(this.getLocalizacao(),u.getLocalizaçao()) <= this.getRaio()){
            this.setDisponivel(false);
            return true;
        }
        return false;
    }
    
    /**
     * Método que faz o transporte da encomenda.
     */
    public void fazTransporteEncomenda(Encomenda e){
        Set<Encomenda> novo = new HashSet<Encomenda>();
        Iterator<Encomenda> iter = this.getRegistos().iterator();
        while (iter.hasNext()){
            Encomenda enc = iter.next();
            novo.add(enc.clone());
        }
        novo.add(e.clone());
        this.setRegistos(novo);
        this.setDisponivel(true); 
    }
    
    /**
     * Méetodo que calcula o tempo de transporte da encomenda.
     */
    public double tempoDeEntrega(Encomenda e,Lojas l,Utilizadores u) {
        double distancia = this.getLocalizacao().distancia(this.getLocalizacao(),l.getLocalizaçao()) + 
                           this.getLocalizacao().distancia(l.getLocalizaçao(), u.getLocalizaçao()); 
        double velocidadeMedia = 10; 
        double tempoFinal = distancia/velocidadeMedia;
        return Math.round(tempoFinal); 
    }
    
    /**
     * Método que determina se no momento as empresas aceitam transporte de encomendas de medicamentos.
     */
    public boolean aceitoTransporteMedicamentos(){
     return this.getAptoMed();
    }
    
    /**
     *Método que muda o estado de aceitação desse tipo de encomendas.
     */
    public void aceitaMedicamentos(boolean state){
        this.setAptoMed(state);
    }
    
    /**
     * Método que verifica credenciais de um login de um voluntario.
     */
    public boolean verificaCredenciais(String cod, String pass){
        return (this.getCodigo().equals(cod) && this.password.equals(pass));
    }
    
    /**
     * Método que retorna as encomendas transportadas num determinado periodo de tempo por um voluntário.
     */
    public Set<String> getEncomendasTempo(LocalDateTime inicio, LocalDateTime fim){
        Set<String> set = new HashSet<String>();
        for(Encomenda e: this.getRegistos()){
            if(e.getData().isAfter(inicio) && e.getData().isBefore(fim)){
                set.add(e.getCodEncomenda());
            }
        }
        return set;    
    }

    /**
     * Método que diz o tipo de transporte.
     */
    public String tipoTransporte(){
        return "Voluntarios";
    }
    
    /**
     * Método que representa em ficheiro CSV um voluntário.
     */
    public String toStringCSV(){
      StringBuilder sb = new StringBuilder();
      sb.append("Voluntario:");
      sb.append(this.getCodigo()).append(",").append(this.getNome()).append(",").append(this.getLocalizacao());
      sb.append(",").append(this.getRaio());
      return sb.toString();
    }
}