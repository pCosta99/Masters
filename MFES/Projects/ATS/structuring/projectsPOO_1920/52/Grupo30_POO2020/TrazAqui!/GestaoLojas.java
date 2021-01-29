import java.util.*;
import java.io.*;


public class GestaoLojas implements Serializable
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private HashMap<String,Loja> lojas;


    public GestaoLojas()
    {
       this.lojas=new HashMap();
    }

   public GestaoLojas(HashMap<String,Loja> lojas2){
    this.lojas=new HashMap();
    for(Loja a:lojas2.values())
        this.lojas.put(a.getEmail(),a.clone());
    }
    
    public GestaoLojas(GestaoLojas gcs){
        this.lojas=gcs.getLoja();
    }
    
    //get
    
    public HashMap<String,Loja> getLoja() {
        HashMap<String,Loja> aux = new HashMap<>();
        for(Loja a: this.lojas.values())
            aux.put(a.getEmail(),a.clone());
        return aux;
    }

    public void setLoja(HashMap<String,Loja> lojas2) {
        this.lojas = new HashMap<>();
        for(Loja a : lojas2.values())
            this.lojas.put(a.getEmail(),a.clone());
    }
    
    
     public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        GestaoLojas aux = (GestaoLojas) object;
        return aux.getLoja().equals(this.getLoja());
    }
    
      public GestaoLojas clone(){
        return new GestaoLojas(this);
    }

    public String toString(){
        return "As lojas são: \n" + this.getLoja();
    }
    
    
    //adiciona voluntario
    
    public void addLoja(Loja a){
        this.lojas.put(a.getEmail(),a.clone());
    }
    
    // verifica se a loja com email e existe
    public boolean verifica(String e){
        return(lojas.containsKey(e));
    }
    
    // confirma login
    
     public boolean login(String email, String password){
        if((this.verifica(email)) == false) return false;
        if ((this.getLoja().get(email).getPassword().equals(password) == true) && (this.getLoja().get(email) instanceof Loja)) return true;
        else return false;

    }
    
    // busca loja com mail 
     public Loja buscaLoja(String mail){
        return this.lojas.get(mail).clone();
    }
}
