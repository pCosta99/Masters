import java.util.Date;
/**
 * Write a description of class Transportadora here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Transportadora extends Registo
{
    /** Código da empresa transportadora **/
    private String codEmpresa;
    /** Nome da empresa transportadora **/
    private String nomempresa;
    /** Localização da empresa transportadora **/
    private Localizacao posicao;
    /** NIf do  **/
    private int nif;
    /** Raio da empresa transportadora **/
    private double raio;
    /** Preço da entraga por km **/
    private double precokm;
    /** Codigo da encomenda*/
    private String codenc;
    /** Data em que foi realizado a encomenda */
    private Date data;
    
    /** Construtor nulo **/
    public Transportadora(){
        super();
        this.codEmpresa="";
        this.nomempresa="";
        Localizacao novaposicao = new Localizacao();
        this.nif= 0;
        this.raio=0.0;
        this.precokm=0.0;
        this.codenc="";
        this.data = new Date();
    }
    
    /** Construtor parametrizado para a classe Transportadora **/
    public Transportadora(String ema, String pas, String codE, String nomem, Localizacao l, int ni, double nraio, double nprecokm, String enc, Date date){
        super(ema,pas);
        this.codEmpresa=codE;
        this.nomempresa=nomem;
        this.posicao=l.clone();
        this.nif=ni;
        this.raio=nraio;
        this.precokm=nprecokm;
        this.codenc=enc;
        this.data = date;
    }
    /** Construtor de cópia **/
    public Transportadora(Transportadora t){
        super(t);
        this.codEmpresa= t.getCodEmpresa();
        this.nomempresa= t.getNome();
        this.posicao= t.getPosicao();
        this.nif= t.getNif();
        this.raio= t.getRaio();
        this.precokm= t.getPrecokm();
        this.codenc= t.getCodEnc();
        this.data = t.getData();
    }
    /** Retorna o código da Transportadora **/
    public String getCodEmpresa(){
        return this.codEmpresa;
    }
    /** Define o código da Transportadora **/
    public void setCodEmpresa(String novocod){
        this.codEmpresa=novocod;
    }
    /** Retorna o nome da Transportadora **/
    public String getNome(){
        return this.nomempresa;
    }
    /** Define o nome da Transportadora **/
    public void setNome(String nome){
        this.nomempresa=nome;
    }
    /** Retorna a localização da Transportadora **/
    public Localizacao getPosicao(){
        return this.posicao.clone();
    }
    /** Define a localização da Transportadora **/
    public void setPosicao(Localizacao pos){
        this.posicao= pos;
    }
    /** Retorna o nif da Transportadora **/
    public int getNif(){
        return this.nif;
    }
    /** Define o nif da Transportadora **/
    public void setNif(int niff){
        this.nif=niff;
    }
    /** Retorna o raio da Transportadora **/
    public double getRaio(){
        return this.raio;
    }
    /** Define o raio da Transportadora **/
    public void setRaio(double nraio){
        this.raio=nraio;
    }
    /** Retorna o preço por km da Transportadora **/
    public double getPrecokm(){
        return this.precokm;
    }
    /** Define o preço por km da Transportadora **/
    public void setPrecokm(double preco){
        this.precokm=preco;
    } 
    /** Retorna o código da encomenda **/
    public String getCodEnc(){
        return this.codenc;
    }
    /** Define o código da encomenda **/
    public void setCodEnc(String novoenc){
        this.codenc=novoenc;
    }
    /** Retorna a data da entrega */
    public Date getData(){
        return this.data;
    }
    /** Define a data da entrega */
    public void setData(Date d){
        this.data = d;
    }
    /** Método que clona uma Transportadora **/
    public Transportadora clone(){
        return new Transportadora(this);
    }
    /** Método que devolve um boolean true caso as Transportadoras sejam iguais e false caso não sejam **/
    public boolean equals(Object o){
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        
        Transportadora t= (Transportadora) o;
        return this.codEmpresa.equals(t.getCodEmpresa()) &&
               this.nomempresa.equals(t.getNome()) &&
               this.posicao.equals(t.getPosicao()) &&
               this.nif == t.getNif() &&
               this.raio == t.getRaio() && 
               this.precokm == t.getPrecokm() &&
               this.data.equals(t.getData()) &&
               this.codenc.equals(t.getCodEnc());
    }
    /** Método que cria uma string com informação da Transportadora **/
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("Código da Empresa Transportadora: ").append(this.codEmpresa+"\n");
        sb.append("Nome da Empresa Transportadora: ").append(this.nomempresa+"\n");
        sb.append("GPS: ").append(this.posicao+"\n");
        sb.append("Nif: ").append(this.nif+"\n");
        sb.append("Raio: ").append(this.raio+"\n");
        sb.append("Preço por km: ").append(this.precokm+"\n");
        sb.append("Data: ").append(this.data.toString()+"\n");
        sb.append("Código da Encomenda: ").append(this.codenc+"\n");
        return sb.toString();
    }
}
