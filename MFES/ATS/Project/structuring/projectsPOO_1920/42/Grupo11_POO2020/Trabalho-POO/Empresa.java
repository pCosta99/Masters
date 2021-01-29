import java.util.ArrayList;

/**
 * Escreva a descrição da classe Empresas aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Empresa
{
    private String id;
    private String nome;
    private String password;
    private int nif;
    private float latitude;
    private float longitude;
    private float raio;
    private float taxa; //taxa por km
    private float custo; //custo de transporte (em funçao do peso e distancia)
    private int nEncomendas; //numero de encomendas que pode transportar
    private boolean encMedicas;
    private float classificacao;
    private boolean disponivel;
    
    //construtor vazio
    
    public Empresa(){
    this.id= "";
    this.nome = "";
    this.password = "";
    this.nif = 0;
    this.latitude = 0;
    this.longitude = 0;
    this.raio = 0;
    this.taxa = 0;
    this.custo = 0;
    this.nEncomendas = 0;
    this.encMedicas = false;
    this.classificacao = 0;
    this.disponivel = false;
    }

    public Empresa(String id, String nome, float lat,float longi){
        this.id = id;
        this.nome = nome;
        this.password = "1";
        this.nif = 0;
        this.latitude = lat;
        this.longitude = longi;

    }

    //construtor parametrizado
    
    public Empresa (String id, String nome, String p, int nif, float longi, float lat, float raio, float taxa, float custo, int n, boolean enc, float c, boolean disponivel){
    this.id = id;
    this.nome = nome;
    this.password = p;
    this.nif = nif;
    this.latitude = lat;
    this.longitude = longi;
    this.raio = raio;
    this.taxa = taxa;
    this.custo = custo;
    this.nEncomendas = n;
    this.encMedicas = enc;
    this.classificacao = c;
    this.disponivel = disponivel;
    }
    
    //contrutor de cópia
    
    public Empresa (Empresa e){
    this.id = e.getId();
    this.nome = e.getNome();
    this.password = e.getPassword();
    this.nif = e.getNif();
    this.latitude = e.getLatitude();
    this.longitude = e.getLongitude();
    this.raio = e.getRaio();
    this.taxa = e.getTaxa();
    this.custo = e.getCusto();
    this.nEncomendas = e.getNEncomendas();
    this.encMedicas = e.getEncMedicas();
    this.classificacao = e.getClassificacao();
    this.disponivel = e.getDisponivel();
    }
    
    //metodo que devolve o ID da empresa em causa
    
    public String getId(){
       return this.id;
    }
    
    //metodo que devolve o nome da empresa em causa
    
    public String getNome(){
       return this.nome;
    }
    
    //metodo que devolve a password da empresa em causa

    public String getPassword(){
        return this.password;
    }
    
    //metodo que devolve o NIF da empresa em causa
    
    public int getNif(){
        return this.nif;
    }
    
    //metodo que devolve a latitude da empresa em causa
    
    public float getLatitude(){
        return this.latitude;
    }
    
    //metodo que devolve a longitude da empresa em causa
    
    public float getLongitude(){
        return this.longitude;
    }
    
    //metodo que devolve o raio de açao da empresa em causa
    
    public float getRaio(){
        return this.raio;
    }
    
    //metodo que devolve a taxa por kilometro da empresa em causa
    
    public float getTaxa(){
        return this.taxa;
    }
    
    //metodo que devolve o custo fixo de transporte de uma encomenda
    
    public float getCusto(){
        return this.custo;
    }
    
    //metodo que devolve o numero de encomendas que a empresa tem como máximo
    
    public int getNEncomendas(){
        return this.nEncomendas;
    }
    
    //metodo que verifica se uma empresa recebe encomendas medicas
    
    public boolean getEncMedicas(){
        return this.encMedicas;
    }
    
    //metodo que devolve a classificaçao de uma empresa

    public float getClassificacao() {
        return classificacao;
    }

    public boolean getDisponivel(){ return disponivel;}
    
    //metodo que da set ao ID da empresa em causa
    
    public void setId(String id){
    this.id = id;
    }
    
    //metodo que da set ao nome da empresa em causa
    
    public void setNome(String nome){
    this.nome = nome;
    }
    
    //metodo que da set à password da empresa em causa 

    public void setPassword(String password){
        this.password = password;
    }
    
    //metodo que da set ao NIF da empresa em causa
    
    public void setNif(int nif){
    this.nif = nif;
    }
    
    //metodo que da set à latitude da empresa em causa
    
    public void setLatitude(float latitude){
    this.latitude = latitude;
    }
    
    //metodo que da set longitude da empresa em causa
    
    public void setLongitude(float longitude){
    this.longitude = longitude;
    }
    
    //metodo que da set ao raio da empresa em causa
    
    public void setRaio(float raio){
    this.raio = raio;
    }
    
    //metodo que da set à taxa por kilómetro da empresa em causa
    
    public void setTaxa(float taxa){
    this.taxa = taxa;
    }
    
    //metodo que da set ao custo de transporte da empresa em causa
    
    public void setCusto(float custo){
    this.custo = custo;
    }
    
    //metodo que da set ao número máximo de encomendas da empresa em causa
    
    public void setNEncomendas(int n){
    this.nEncomendas = n;
    }
    
    //metodo que da set à aceitação por parte da empresa de encomendas médicas
    
    public void setEncMedicas(boolean enc){
    this.encMedicas = enc;
    }

    //metodo que dá set à classificação da empresa em causa 
    
    public void setClassificacao(float classificacao) {
        this.classificacao = classificacao;
    }

    public void setDisponivel(boolean disponivel){this.disponivel = disponivel;}
    
    //metodo que verifica de 2 objetos são iguais
    
    public boolean equals (Object o){
       if (o == this) return true;
       if (o == null || o.getClass() != this.getClass()) return false;
       Empresa e = (Empresa) o;
       return ( this.id.equals(e.getId()) &&
                this.nome.equals(e.getNome()) &&
                this.password.equals(e.getPassword()) &&
                this.nif == e.getNif() &&
                this.latitude == e.getLatitude() &&
                this.longitude == e.getLongitude() &&
                this.raio == e.getRaio() &&
                this.taxa == e.getTaxa() &&
                this.custo == e.getCusto() &&
                this.nEncomendas == e.getNEncomendas() &&
                this.encMedicas == e.getEncMedicas()) &&
               this.disponivel == e.getDisponivel() &&
                this.classificacao == e.getClassificacao();
    }
    
    //metodo que devolve a classe numa string
    
    public String toString() {
       StringBuilder s = new StringBuilder();
       s.append("Voluntario: ").append(this.id)
                               .append("\n")
                               .append(this.nome)
                               .append("\n")
                               .append(this.password)
                               .append("\n")
                               .append(this.nif)
                               .append("\n")
                               .append(this.latitude)
                               .append("\n")
                               .append(this.longitude)
                               .append("\n")
                               .append(this.raio)
                               .append("\n")
                               .append(this.taxa)
                               .append("\n")
                               .append(this.custo)
                               .append("\n")
                               .append(this.nEncomendas)
                               .append("\n")
                               .append(this.encMedicas)
                               .append("\n")
                               .append(this.classificacao)
                               .append("\n")
                               .append(this.disponivel);

       return s.toString();
    }
    
    //metodo que faz um clone da classe em questão
    
    public Empresa clone(){
    return new Empresa(this);
    }


    public boolean aceitoTransporteMedicamentos() {
        return this.encMedicas;
    }

    public void aceitaMedicamentos (boolean state){
        this.encMedicas = state;
    }



}
