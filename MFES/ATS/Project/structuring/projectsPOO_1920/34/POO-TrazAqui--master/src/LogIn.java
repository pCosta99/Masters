
import java.util.HashMap;
import java.util.Map;
import java.io.Serializable;

public class LogIn implements Serializable {
    private Map<String,Logger> logs;

    public LogIn() {
        this.logs = new HashMap<String,Logger>();
    }

    public LogIn(Map<String, Logger> logs) {
        setLogs(logs);
    }

    public LogIn(LogIn l){
        setLogs(l.getLogs());
    }

    public Map<String, Logger> getLogs() {
        Map<String,Logger> ret = new HashMap<String,Logger>();
        logs.forEach((a,b)->ret.put(a,b.clone()));
        return ret;
    }

    public void setLogs(Map<String, Logger> logs) {
        this.logs = new HashMap<>();
        logs.entrySet().forEach(e->this.logs.put(e.getKey(),e.getValue().clone()));
    }

    public LogIn clone(){
        return new LogIn(this);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.logs.values().toString());
        return sb.toString();
    }

    public boolean equals (Object o)
    { if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        LogIn a = (LogIn) o;
        return this.logs.equals(a.getLogs());
    }

    public void addLogger(Logger l){
        this.logs.put(l.getEmail(),l.clone());
    }

    public Logger getLogger(String email){
        if(this.logs!=null) {
            Logger a = this.logs.get(email);
            if (a != null) return a;
            else return  null;
        }
        else return null;
    }

    public boolean equalsPassword(Logger l , String password){
        return l.getPassword().equals(password);
    }

    public boolean verificaLogin(String email,String pass){
        Logger l = getLogger(email);
        if(l!=null && equalsPassword(l,pass)) return true;
        else return false;
    }

    public boolean logContainsMail(String email){
        if(this.logs == null) return false;
        return this.logs.containsKey(email);
    }

    public void addLogIn (String mail,Logger a){
        this.logs.put(mail.toLowerCase(), a);
    }



    /*public void leFicheiro()
    {
        try{
            File toRead=new File("Dados.ppI");
            FileInputStream fis=new FileInputStream(toRead);
            ObjectInputStream ois=new ObjectInputStream(fis);
            HashMap<String,Logger> mapInFile=(HashMap<String,Logger>)ois.readObject();
            ois.close();
            fis.close();
            //print All data in MAP
            this.logs=mapInFile;
        }catch(Exception e){
            System.out.println(e.getMessage());
        }
    }*/
}
