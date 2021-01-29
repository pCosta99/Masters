import java.util.HashMap;
import java.util.Map;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.io.Serializable;



public class TrazAqui implements Serializable {
    
    private Map<String, AllUsers> infoAll;
    private Map<String, Encomenda> allEnc;
    private Map<String, EncomendasAceites> encAceites;
    private Map<String, String> allLogins;
    

    public TrazAqui() {
        this.infoAll = new HashMap<>();
        this.allEnc = new HashMap<>();
        this.encAceites = new HashMap<>();
        this.allLogins = new HashMap<>();
    }

    public TrazAqui(Map<String,AllUsers> info, Map<String,Encomenda> enc, Map <String, EncomendasAceites> aceites, Map <String, String> alllogins){
        setInfoAll(info);
        setAllEnc(enc);
        setEncAceites(aceites);
        setAllLogins(alllogins);
    }

    public TrazAqui(TrazAqui t){
        setInfoAll(t.getInfoAll());
        setAllEnc(t.getAllEnc());
        setEncAceites(t.getEncAceites());
        setAllLogins(t.getAllLogins());
    }

    //GET methods
    public Map<String,AllUsers> getInfoAll(){
        /*
        Map<String,AllUsers> ret = new HashMap<>();
        for (Map.Entry<String,AllUsers> a : this.infoAll.entrySet())
            ret.put(a.getKey(),a.getValue());
            */

        return this.infoAll;
  
    }
    
    public Map<String,Encomenda> getAllEnc(){
        Map<String,Encomenda> enc = new HashMap<>();
        for (Encomenda e : this.allEnc.values())
            enc.put(e.getOrderCode(),e);

        return enc;
  
    }
    
    public Map<String,EncomendasAceites> getEncAceites(){
        Map<String,EncomendasAceites> aceites = new HashMap<>();
        for (Map.Entry<String,EncomendasAceites> e : this.encAceites.entrySet())
            aceites.put(e.getKey(),e.getValue());

        return aceites;
  
    }

    public Map<String,String> getAllLogins(){
        Map<String,String> newlogins = new HashMap<>();
        for (Map.Entry<String,String> l : this.allLogins.entrySet())
            newlogins.put(l.getKey(),l.getValue());

        return this.allLogins;
  
    }


    //SET methods
    public void setInfoAll(Map<String,AllUsers> infoAll){
        this.infoAll = new HashMap<>(); //diamond notation
        infoAll.entrySet().forEach(a -> this.infoAll.put(a.getKey(), a.getValue().clone()));
    }
    
    public void setAllEnc(Map<String,Encomenda> enc){
        HashMap<String,Encomenda> novo= new HashMap<>(); //diamond notation
        enc.entrySet().forEach(a -> novo.put(a.getKey(), a.getValue()));
        this.allEnc=novo;
    }
    
    public void setEncAceites(Map<String,EncomendasAceites> aceites){
        this.encAceites = new HashMap<>(); //diamond notation
        aceites.entrySet().forEach(a -> this.encAceites.put(a.getKey(), a.getValue().clone()));
    }

    public void setAllLogins(Map<String,String> newlogins){
        this.allLogins = new HashMap<>(); //diamond notation
        newlogins.entrySet().forEach(a -> this.allLogins.put(a.getKey(), a.getValue()));
    }


    public TrazAqui clone(){
        return new TrazAqui(this);
    }

    public boolean equals (Object o){
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;

        TrazAqui t = (TrazAqui) o;
        return(t.getInfoAll().equals(this.infoAll));

    }

    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("\nTodos os utilizadores:\n").append(this.infoAll);
        return sb.toString();

    }

    /*
    public List<AllUsers> getAllUsers(){
        List<AllUsers> listaA = new ArrayList<>();
        for(Map.Entry<String,AllUsers> a : this.infoAll.entrySet())
            listaA.add(a.getValue().clone());
        
        return listaA;
    }
    */

    public void classifica(String cod, int classificacao) //classificar voluntario ou empresa

    {
        for (Map.Entry<String,AllUsers> t : this.infoAll.entrySet()){
            if (t.getValue().getCode().equals(cod)){
                t.getValue().getClassificacao().add(classificacao);
            }
        }
    }
    
    public void gravaCSV(String nomeFicheiro) throws FileNotFoundException
    {
        PrintWriter pw = new PrintWriter (nomeFicheiro);
        //pw.println("Utilizadores: " +this.infoAll);
        for (AllUsers a : this.infoAll.values()){
            pw.println(a.toStringCSV(a));
        }
        //print encomendas
        for (Encomenda e : this.allEnc.values()){
            pw.println(e.toStringCSV());
        }
        //print aceites
        for (EncomendasAceites ea : this.encAceites.values()){
            pw.println(ea.toStringCSV());
        }
        pw.flush();
        pw.close();
    }

    public void gravaEmFicheiro(String nameFile) throws FileNotFoundException, IOException{
        FileOutputStream fos = new FileOutputStream(nameFile);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }

    public TrazAqui leFicheiro(String nameFile) throws FileNotFoundException, IOException, ClassNotFoundException{
        FileInputStream fis = new FileInputStream(nameFile);
        ObjectInputStream ois = new ObjectInputStream(fis);
        TrazAqui t = (TrazAqui) ois.readObject();
        ois.close();
        return t;
    }


    public boolean verificaLogin(String mail, String pw){
        String k=this.allLogins.get(mail);
        if (k!= null){
            return (this.allLogins.get(mail).equals(pw)); //u7@email.com
        }
        else {
            return false;
        }
    }

    public void addEncomendaAceite (String codenc, Encomenda e){
        Map<String, Encomenda> newmap = new HashMap<>();
        newmap=this.getAllEnc();
        newmap.put(codenc, e);
        setAllEnc(newmap);
    }

    public void removeEnc(String codEnc){
        this.allEnc.remove(codEnc);
    }

}