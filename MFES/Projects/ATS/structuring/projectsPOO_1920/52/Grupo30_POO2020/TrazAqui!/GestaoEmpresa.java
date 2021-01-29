import java.util.*;
import java.io.*;
import java.time.LocalDate;
import java.time.LocalDateTime;

public class GestaoEmpresa implements Serializable
{
    // variáveis de instância - substitua o exemplo abaixo pelo seu próprio
    private HashMap<String,Empresa> empresas;


    public GestaoEmpresa()
    {
       this.empresas=new HashMap();
    }

   public GestaoEmpresa(HashMap<String,Empresa> empresas2){
    this.empresas=new HashMap();
    for(Empresa a:empresas2.values())
        this.empresas.put(a.getEmail(),a.clone());
    }
    
    public GestaoEmpresa(GestaoEmpresa gcs){
        this.empresas=gcs.getEmpresa();
    }
    
    //get
    
    public HashMap<String,Empresa> getEmpresa() {
        HashMap<String,Empresa> aux = new HashMap<>();
        for(Empresa a: this.empresas.values())
            aux.put(a.getEmail(),a.clone());
        return aux;
    }

    public void setEmpresa(HashMap<String,Empresa> empresas) {
        this.empresas = new HashMap<>();
        for(Empresa a : empresas.values())
            this.empresas.put(a.getEmail(),a.clone());
    }
    
    
     public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        GestaoEmpresa aux = (GestaoEmpresa) object;
        return aux.getEmpresa().equals(this.getEmpresa());
    }
    
      public GestaoEmpresa clone(){
        return new GestaoEmpresa(this);
    }

    public String toString(){
        return "As empresas são: \n" + this.getEmpresa();
    }
    
    public Empresa buscaEmpresa(String mail){
        return this.empresas.get(mail).clone();
    }
    
    //adiciona empresa
    
    public void addEmpresa(Empresa a){
        this.empresas.put(a.getEmail(),a.clone());
    }
    
    // verifica se a empresa com email e existe
    public boolean verifica(String e){
        return(empresas.containsKey(e));
    }
    
    // confirma login
    
     public boolean login(String email, String password){
        if((this.verifica(email)) == false) return false;
        if ((this.getEmpresa().get(email).getPassword().equals(password) == true) && (this.getEmpresa().get(email) instanceof Empresa)) return true;
        else return false;

    }
    
    //metodo para atualizar listas de encomendas entregues
     public void atualizaELE(RealizadaEmpresa r, String mail) {((this.empresas.get(mail))).atualizaLE(r);}
     
   
    //metodo para atualizar classficacao
      public void atualizaClassificacaoEmpresa(double c, Empresa cl){
        if (this.empresas.get(cl.getEmail()).getnmrClassificacoes() == 0 ) {
            this.empresas.get(cl.getEmail()).setRating(c);
            this.empresas.get(cl.getEmail()).setnmrClassificacoes(1);
        }
        else {
            this.empresas.get(cl.getEmail()).setRating(((c)+ (this.empresas.get(cl.getEmail()).getRating() * this.empresas.get(cl.getEmail()).getnmrClassificacoes())) / (this.empresas.get(cl.getEmail()).getnmrClassificacoes() +1)) ;
            this.empresas.get(cl.getEmail()).setnmrClassificacoes(this.empresas.get(cl.getEmail()).getnmrClassificacoes() + 1);
        }
    }
    
    //metodo que devolve lista de encomendas entregues por uma empresa num determinado periodo
    public List<RealizadaEmpresa> EncEmpresaPorPeriodo(Empresa e, LocalDate inicio, LocalDate fim){
        List<RealizadaEmpresa> re= new ArrayList<RealizadaEmpresa>(this.empresas.get(e.getEmail()).getRe());
        List<RealizadaEmpresa> aux = new ArrayList<RealizadaEmpresa>();
        for (RealizadaEmpresa a : re){
             if (a.getData().isAfter(inicio) && a.getData().isBefore(fim)) aux.add(a);
        }
           return aux; 
        }
}
