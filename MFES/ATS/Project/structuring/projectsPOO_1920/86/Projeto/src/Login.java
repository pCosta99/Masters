import javax.swing.text.html.parser.Parser;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Método que contém a informação de todas as entidades de programa assim como as suas passwords.
 */
public class Login implements Serializable {
    private Map<String,Entidade> user;
    private Map<String,String> pass;
    private Entidade ent;

    public Login(){
        this.user = new HashMap<>();
        this.pass = new HashMap<>();
        this.ent = null;
    }

    public Login(Map<String, Entidade> user, Map<String, String> pass,Entidade e) {
        this.user = user;
        this.pass = pass;
        this.setEnt(e);
    }

    public Login(Login e){
        this.setPass(e.getPass());
        this.setUser(e.getUser());
        this.setEnt(e.getEnt());
    }

    private Map<String, Entidade> getUser() {
        Map<String,Entidade> aux = new HashMap<>();
        for(Map.Entry<String,Entidade> e:this.user.entrySet())
            aux.put(e.getKey(),e.getValue().clone());
        return aux;
    }

    private Map<String,String> getPass(){
        Map<String,String> aux = new HashMap<>();
        for(Map.Entry<String,String> e:this.pass.entrySet())
            aux.put(e.getKey(),e.getValue());
        return aux;
    }

    public Entidade getEnt() {
        return ent;
    }

    private void setUser(Map<String, Entidade> user) {
        this.user = new HashMap<>();
        for(Map.Entry<String,Entidade> e:user.entrySet())
            this.user.put(e.getKey(),e.getValue().clone());
    }

    private void setPass(Map<String, String> pass) {
        this.pass = new HashMap<>();
        for(Map.Entry<String,String> e:pass.entrySet())
            this.pass.put(e.getKey(),e.getValue());
    }

    public void setEnt(Entidade ent) {
        this.ent = ent.clone();
    }
    /**
     * Método que adiciona um utilizador aos logins.
     */
    public void addUtilizador(Utilizador e){
        this.user.put(e.getCodigo(),e);
        this.pass.put(e.getCodigo(),e.getCodigo());
    }

    /**
     * Método que adiciona um voluntario aos logins.
     */
    public void addVoluntario(Voluntario e){
        this.user.put(e.getCodigo(),e);
        this.pass.put(e.getCodigo(),e.getCodigo());
    }

    /**
     * Método que adiciona um transportador aos logins.
     */
    public void addTransportadora(Transportadoras e){
        this.user.put(e.getCodigo(),e);
        this.pass.put(e.getCodigo(),e.getCodigo());
    }

    /**
     * Método que adiciona uma loja aos logins.
     */
    public void addLoja(Loja e){
        this.user.put(e.getCodigo(),e);
        this.pass.put(e.getCodigo(),e.getCodigo());
    }

    /**
     * Método que adiciona um utilizador e a sua password aos logins.
     */
    public void addUtilizadorPass(Utilizador e,String pass){
        this.user.put(e.getCodigo(),e);
        this.pass.put(e.getCodigo(),pass);
    }

    /**
     * Método que adiciona um voluntario e a sua password aos logins.
     */
    public void addVoluntarioPass(Voluntario e,String pass){
        this.user.put(e.getCodigo(),e);
        this.pass.put(e.getCodigo(),pass);
    }

    /**
     * Método que adiciona um transportador e a sua password aos logins.
     */
    public void addTransportadoraPass(Transportadoras e,String pass){
        this.user.put(e.getCodigo(),e);
        this.pass.put(e.getCodigo(),pass);
    }

    /**
     * Método que adiciona uma loja e a sua password aos logins.
     */
    public void addLojaPass(Loja e,String pass){
        this.user.put(e.getCodigo(),e);
        this.pass.put(e.getCodigo(),pass);
    }

    /**
     * Método que permite fazer login dando um mail e uma passwords.
     * @param mail Mail para fazer logins.
     * @param pass Palavra passe da entidade
     * @return Retorna a entidade que fez login, em caso de falhar o login retorna null.
     * @throws ArrayIndexOutOfBoundsException Exeção para caso de não ser dado um email para o login.
     */
    public Entidade loginPass(String mail,String pass) throws ArrayIndexOutOfBoundsException{
        String[] aux = mail.split("@");
        String passs = this.pass.get(aux[0]);
        if(!aux[1].equals("gmail.com")) return null;
        if(passs!= null ) {
            if (passs.equals(pass)) {
                this.ent = this.user.get(aux[0]);
                return this.ent;
            }
        }
        return null;
    }
}
