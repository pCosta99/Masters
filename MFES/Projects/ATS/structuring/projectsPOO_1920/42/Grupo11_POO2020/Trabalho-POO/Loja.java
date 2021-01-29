
/**
 * Escreva a descrição da classe Loja aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

public class Loja 
{
    private String nome;
    private String idLoja;
    private String password;
    private float latitude;
    private float longitude;
    private Map <String,Encomenda> encomendas;  //a key é o id do utilizador que efetuou a encomenda
    private float tempoAtend;

//construtor vazio

public Loja(){
this.nome = "";
this.idLoja = "";
this.password = "";
this.latitude = 0;
this.longitude = 0;
this.encomendas = new HashMap<>();
this.tempoAtend = 0;
}

    public Loja(String id, String nome, float lat,float longi){
        this.idLoja = id;
        this.nome = nome;
        this.tempoAtend = 0;
        this.password = "1";
        this.encomendas = new HashMap<String,Encomenda>();
        this.latitude = lat;
        this.longitude = longi;

    }

//construtor parametrizado

public Loja(String nome, String idLoja, String password, float lat, float lon, Map<String,Encomenda> enc, float temp){
this.nome = nome;
this.idLoja = idLoja;
this.password = password;
this.latitude = lat;
this.longitude = lon;
setEnc(enc);
this.tempoAtend = temp;
}
    
//construtor de cópia

public Loja(Loja e){
this.nome = e.getNome();
this.idLoja = e.getId();
this.password = e.getPassword();
this.latitude = e.getLat();
this.longitude = e.getLong();
setEnc(e.getEnc());
this.tempoAtend = e.getTemp();
}
    
//metodo que devolve o nome da loja em causa

public String getNome(){
       return this.nome;
    }
  

//metodo que devolve o ID da loja em causa
    
public String getId(){ 
       return this.idLoja;
    }

//metodo que devolve a password da loja em causa
    
public String getPassword(){
        return this.password;
    }
    
//metodo que devolve a latitude da loja em causa
    
public float getLat(){ 
       return this.latitude;
    }
    
//metodo que devolve a longitude da loja em causa
    
public float getLong(){ 
       return this.longitude;
    }
    
//metodo que devolve a meádia do tempo de atendimento da loja em causa

public float getTemp(){ 
       return this.tempoAtend;
    }
    
//metodo que devolve as encomendas da loja em causa

public Map<String,Encomenda> getEnc(){
       return this.encomendas;
    }
    
//metodo de dá set ao nome da loja em causa

public void setNome(String nome){
this.nome = nome;
}
    
//metodo de dá set ao ID da loja em causa

public void setId(String id){
this.idLoja = id;
}
    
//metodo de dá set à password da loja em causa

public void setPassword(String p) { this.password = password;}

//metodo de dá set à latitude da loja em causa
  
public void setLat(float l){
this.latitude = l;
}
    
//metodo de dá set à longitude da loja em causa

public void setLong(float lg){
this.longitude = lg;
}
    
//metodo de dá set à media do tempo de atendimento da loja em causa

public void setTemp(float time){
this.tempoAtend = time;
}
    
//metodo de dá set às encomendas da loja em causa
    
public void setEnc(Map<String,Encomenda> enc){
       this.encomendas = new HashMap<String,Encomenda>();
       enc.entrySet().forEach( e -> this.encomendas.put(e.getKey(),e.getValue().clone()));
    }
    
//metodo que verifica se 2 objetos sao iguais
    
public boolean equals (Object o){
       if (o == this) return true;
       if (o == null || o.getClass() != this.getClass()) return false;
       Loja t = (Loja) o;
       return this.nome.equals(t.getNome()) &&
              this.idLoja.equals(t.getId()) &&
              this.password.equals(t.getPassword()) &&
              this.latitude == (t.getLat()) &&
              this.longitude == (t.getLong()) &&
              this.encomendas.equals(t.getEnc()) &&
              this.tempoAtend == (t.getTemp());
    }
    
//metodo que devolve a classe numa string

public String toString() {
       StringBuilder s = new StringBuilder();
       s.append("Loja: ").append(this.nome)
                          .append("\n")
                          .append(this.idLoja)
                          .append("\n")
                          .append(this.latitude)
                          .append("\n")
                          .append(this.longitude)
                          .append("\n")
                          .append(this.encomendas)
                          .append("\n")
                          .append(this.tempoAtend);
       return s.toString();
    }
    
//metodo que devolve um clone da classe em causa

public Loja clone(){
   return new Loja (this);
    }
}
