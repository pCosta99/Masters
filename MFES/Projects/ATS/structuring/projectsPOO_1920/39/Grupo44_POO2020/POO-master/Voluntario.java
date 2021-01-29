
/**
 * Write a description of class Voluntario here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Voluntario extends Registo
{
    /** Código do voluntário **/
    private String codVoluntario;
    /** Nome do voluntário **/
    private String nome;
    /** Localização do voluntário **/
    private Localizacao posicao;
    /** Raio da movimentação do voluntario **/
    private double raio;
    
    
    /** Construtor nulo **/
    public Voluntario(){
        super();
        this.codVoluntario= "";
        this.nome= "";
        Localizacao novaposicao= new Localizacao();
        this.raio= 0.0;
    }
    /** Construtor parametrizado para a classe Voluntario **/
    public Voluntario(String ema, String pas, String codV, String nom, Localizacao l, double vraio){
        super(ema,pas);
        this.codVoluntario=codV;
        this.nome=nom;
        this.posicao=l.clone();
        this.raio=vraio;
    }
    /** Construtor de cópia **/
    public Voluntario(Voluntario v){
        super(v);
        this.codVoluntario= v.getCodVoluntario();
        this.nome= v.getNome();
        this.posicao= v.getPosicao();
        this.raio= v.getRaio();
    }
    /** Retorna o código do Voluntario **/
    public String getCodVoluntario(){
        return this.codVoluntario;
    }
    /** Define o código do Voluntario **/
    public void setCodVoluntario(String novocod){
        this.codVoluntario= novocod;
    }
    /** Retorna o nome do Voluntario **/
    public String getNome(){
        return this.nome;
    }
    /** Define o nome do Voluntario **/
    public void setNome(String nom){
        this.nome=nom;
    }
    /** Retorna a localização do Voluntario **/
    public Localizacao getPosicao(){
        return this.posicao.clone();
    }
    /** Define a localização do Voluntario **/
    public void setPosicao(Localizacao pos){
        this.posicao=pos;
    }
    /** Retorna o raio do Voluntario **/
    public double getRaio(){
        return this.raio;
    }
    /** Define o raio do Voluntario **/
    public void setRaio(double nraio){
        this.raio=nraio;
    }
    /** Método que clona um Voluntario **/
    public Voluntario clone(){
        return new Voluntario(this);
    }
    /** Método que devolve um boolean true caso os Voluntarios sejam iguais e false caso não sejam **/
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        
        Voluntario v= (Voluntario) o;
        return this.codVoluntario.equals(v.getCodVoluntario()) &&
               this.nome.equals(v.getNome()) &&
               this.posicao.equals(v.getPosicao()) &&
               this.raio == v.getRaio();
    }
    /** Método que cria uma string com a informação do Voluntario **/
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("Código do Voluntário: ").append(this.codVoluntario+"\n");
        sb.append("Nome: ").append(this.nome+"\n");
        sb.append("GPS: ").append(this.posicao+"\n");
        sb.append("Raio: ").append(this.raio+"\n");
        return sb.toString();
    }
}
