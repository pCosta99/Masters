import java.util.ArrayList;
import java.util.List;

/**
 * Escreva a descrição da classe Utilizador aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Utilizador
{
    private String id;
    private String nome;
    private String email;
    private String password;
    private int nif;
    private float latitude;
    private float longitude;
    private List<Encomenda> encomendas;

//construtor vazio
    
    public Utilizador(){
    this.id= "";
    this.nome = "";
    this.email = "";
    this.password = "";
    this.nif = 0;
    this.latitude = 0;
    this.longitude = 0;
    this.encomendas = new ArrayList<>(); //encomendas que o utilizador faz
    }

    //construtor parametrizado
    public Utilizador(String id, String nome, float lat,float longi){
        this.id = id;
        this.nome = nome;
        this.email = "";
        this.password = "1";
        this.nif = 0;
        this.latitude = lat;
        this.longitude = longi;
        this.encomendas = new ArrayList<>();
    }

//construtor parametrizado

    
    public Utilizador (String id, String nome, String email, String password, int nif, float lat, float longi, ArrayList<Encomenda> enc){
    this.id = id;
    this.nome = nome;
    this.email = email;
    this.password = password;
    this.nif = nif;
    this.latitude = lat;
    this.longitude = longi;
    this.encomendas = enc;
    }
    
//construtor de cópia
    
    public Utilizador (Utilizador u){
    this.id = u.getId();
    this.nome = u.getNome();
    this.email = u.getEmail();
    this.password = u.getPassword();
    this.nif = u.getNif();
    this.latitude = u.getLatitude();
    this.longitude = u.getLongitude();
    this.encomendas = u.getEncomendas();
    }
    
//metodo que devolve o ID do utilizador em questão
    
    public String getId(){
       return this.id;
    }
    
//metodo que devolve o nome do utilizador em questão
    
    public String getNome(){
       return this.nome;
    }
    
//metodo que devolve o email do utilizador em questão
    
    public String getEmail(){
       return this.email;
    }
    
//metodo que devolve a password do utilizador em questão
    
    public String getPassword(){
       return this.password;
    }
    
//metodo que devolve o NIF do utilizador em questão
    
    public int getNif(){
       return this.nif;
    }
    
//metodo que devolve a latitude do utilizador em questão

    public float getLatitude(){ return this.latitude;}
    
//metodo que devolve a longitude do utilizador em questão

    public float getLongitude(){ return this.longitude;}

    public ArrayList<Encomenda> getEncomendas(){
        return (ArrayList<Encomenda>) this.encomendas;
    }
    
//metodo que da set do ID do utilizador em questão
    
    public void setId(String id){
    this.id = id;
    }
    
//metodo que da set do nome do utilizador em questão
    
    public void setNome(String nome){
    this.nome = nome;
    }
    
//metodo que da set do email do utilizador em questão
    
    public void setEmail(String email){
    this.email = email;
    }
    
//metodo que da set da password do utilizador em questão
    
    public void setPassword(String password){
    this.password = password;
    }
    
//metodo que da set do NIF do utilizador em questão

    public void setNif(int nif){
    this.nif = nif;
    }
    
//metodo que da set da latitude do utilizador em questão

    public void  setLatitude(float lat) {this.latitude = lat;}
    
//metodo que da set da longitude do utilizador em questão

    public void  setLongitude(float longi) {this.longitude = longi;}

    public void setEncomendas(ArrayList<Encomenda> enc){
        this.encomendas = new ArrayList<Encomenda>(enc.size());
        for(Encomenda e : enc){
            this.encomendas.add(e.clone());
        }
    }
    
//metodo que verifica se 2 objetos são iguais

    public boolean equals (Object o){
       if (o == this) return true;
       if (o == null || o.getClass() != this.getClass()) return false;
       Utilizador t = (Utilizador) o;
       return (this.id.equals(t.getId())) &&
              this.nome.equals(t.getNome()) &&
              this.email.equals(t.getEmail()) &&
              this.password.equals(t.getPassword()) &&
              (this.nif == t.getNif()) &&
               this.latitude == t.getLatitude() &&
               this.longitude == t.getLongitude();
    }
    
//metodo que devolve a classe numa classe
    
    public String toString() {
       StringBuilder s = new StringBuilder();
       s.append("Utilizador: ").append(this.id)
                               .append("\n")
                               .append(this.nome)
                               .append("\n")
                               .append(this.email)
                               .append("\n")
                               .append(this.password)
                               .append("\n")
                               .append(this.nif)
                               .append("\n")
                               .append(this.latitude)
                               .append("\n")
                               .append(this.longitude);
       return s.toString();
    }
    
//metodo que devolve um clone da classe em causa
    
    public Utilizador clone(){
    return new Utilizador(this);
    }

}
