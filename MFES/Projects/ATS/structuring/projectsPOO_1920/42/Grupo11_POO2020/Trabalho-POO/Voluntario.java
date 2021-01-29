import java.util.ArrayList;

/**
 * Escreva a descrição da classe Voluntario aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Voluntario
{
    private String id;
    private String nome;
    private String email;
    private String password;
    private float latitude;
    private float longitude;
    private float raio;
    private boolean encMedicas;
    private boolean disponivel;
    private float classificacao;

//construtor vazio

    public Voluntario(){
    this.id= "";
    this.nome = "";
    this.email = "";
    this.password = "";
    this.latitude = 0;
    this.longitude = 0;
    this.raio = 0;
    this.encMedicas = false;
    this.disponivel = true;
    this.classificacao = 0;
    }


    public Voluntario(String id, String nome, float lat,float longi){
        this.id = id;
        this.nome = nome;
        this.email = "";
        this.password = "1";

        this.latitude = lat;
        this.longitude = longi;
        this.raio = 0;
        this.encMedicas = false;
        this.disponivel = true;
        this.classificacao=0;
    }
//construtor parametrizado

    public Voluntario (String id, String nome, String email, String password, float longitude, float latitude, float raio, boolean encMedicas, boolean disp, float c){
    this.id = id;
    this.nome = nome;
    this.email = email;
    this.password = password;
    this.latitude = latitude;
    this.longitude = longitude;
    this.raio = raio;
    this.encMedicas = encMedicas;
    this.disponivel = disp;
    this.classificacao = c;
    }
    
//construtor de cópia

    public Voluntario (Voluntario v){
    this.id = v.getId();
    this.nome = v.getNome();
    this.email = v.getEmail();
    this.password = v.getPassword();
    this.latitude = v.getLatitude();
    this.longitude = v.getLongitude();
    this.raio = v.getRaio();
    this.encMedicas = v.getEncMedicas();
    this.disponivel = v.getDisp();
    this.classificacao = v.getClassificacao();
    }
    
//metodo que devolve o ID do voluntario em causa

    public String getId(){
       return this.id;
    }

//metodo que devolve o nome do voluntario em causa
    
    public String getNome(){
       return this.nome;
    }
    
//metodo que devolve o email do voluntario em causa

    public String getEmail(){
       return this.email;
    }
    
//metodo que devolve a password do voluntario em causa

    public String getPassword(){
       return this.password;
    }
    
//metodo que devolve a latitude do voluntario em causa

    public float getLatitude(){
        return this.latitude;
    }
    
//metodo que devolve a longitude do voluntario em causa

    public float getLongitude(){
        return this.longitude;
    }
    
//metodo que devolve o raio do voluntario em causa

    public float getRaio(){
        return this.raio;
    }
    
//metodo que verifica se o voluntario em causa aceita encomendas médicas

    public boolean getEncMedicas(){
        return this.encMedicas;
    }
    
//metodo que verifica se o voluntario em causa está disponível no momento para transporte de encomendas

    public boolean getDisp(){
        return this.disponivel;
    }
    
//metodo que devolve a classificação do voluntario em causa

    public float getClassificacao() {
        return this.classificacao;
    }
    
//metodo que dá set ao ID do voluntário em causa

    public void setId(String id){
    this.id = id;
    }
    
//metodo que dá set ao nome do voluntário em causa

    public void setNome(String nome){
    this.nome = nome;
    }
    
//metodo que dá set ao email do voluntário em causa

    public void setEmail(String email){
    this.email = email;
    }
    
//metodo que dá set da password do voluntário em causa

    public void setPassword(String password){
    this.password = password;
    }
    
//metodo que dá set à latitude do voluntário em causa

    public void setLatitude(float latitude){
    this.latitude = latitude;
    }
    
//metodo que dá set à longitude do voluntário em causa

    public void setLongitude(float longitude){
    this.longitude = longitude;
    }
    
//metodo que dá set so raio de ação do voluntário em causa

    public void setRaio(float raio){
    this.raio = raio;
    }

//metodo que determina se o voluntário em causa aceita encomendas médicas
    
    public void setEncMedicas(boolean enc){
    this.encMedicas = enc;
    }
    
//metodo que determina se o voluntário em causa se encontra disponível para transporte de encomendas medicas

    public void setDisp(boolean d){
    this.disponivel = d;
    }
    
//metodo que dá set da classificação do voluntário em causa

    public void setClassificacao(float cla) {
        this.classificacao = cla;
    }
    
//metodo que verifica se 2 objetos são iguais

    public boolean equals (Object o){
       if (o == this) return true;
       if (o == null || o.getClass() != this.getClass()) return false;
       Voluntario v = (Voluntario) o;
       return ( this.id.equals(v.getId()) &&
                this.nome.equals(v.getNome()) &&
                this.email.equals(v.getEmail()) &&
                this.password.equals(v.getPassword()) &&
                this.latitude == v.getLatitude() &&
                this.longitude == v.getLongitude() &&
                this.raio == v.getRaio() &&
                this.encMedicas == v.getEncMedicas() &&
                this.disponivel == v.getDisp() &&
                this.classificacao == v.getClassificacao());
    }
    
//metodo que devolve a classe numa string

    public String toString() {
       StringBuilder s = new StringBuilder();
       s.append("Voluntario: ").append(this.id)
                               .append("\n")
                               .append(this.nome)
                               .append("\n")
                               .append(this.email)
                               .append("\n")
                               .append(this.password)
                               .append("\n")
                               .append(this.latitude)
                               .append("\n")
                               .append(this.longitude)
                               .append("\n")
                               .append(this.raio)
                               .append("\n")
                               .append(this.encMedicas)
                               .append("\n")
                               .append(this.disponivel)
                               .append("\n")
                               .append((this.classificacao));
       return s.toString();
    }
    
//metodo que devolve um clone da classe em causa

    public Voluntario clone(){
    return new Voluntario(this);
    }


    public boolean aceitoTransporteMedicamentos() {
        return this.encMedicas;
    }

    public void aceitaMedicamentos (boolean state){
        this.encMedicas = state;
    }


}
