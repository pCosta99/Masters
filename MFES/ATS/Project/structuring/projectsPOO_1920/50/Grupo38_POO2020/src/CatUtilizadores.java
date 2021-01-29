import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

public class CatUtilizadores implements Serializable  {
    /**Map que representa o catálogo de users sendo as keys os seus codigos e os values os respectivos users*/
    private Map<String,Utilizador> users;

    public CatUtilizadores () {
        this.users = new HashMap<>();
    }

    /**Método que adiciona um novo Utilizador ao catálogo de users
     *
     * @param a Utilizador
     */
    public void adicionaUser (Utilizador a){
        this.users.put(a.getCod(),a.clone());
    }

    /**Método que retorna o codigo de um Utilizador caso o username e password derem match com um dos utilizadores registados
     *
     * @param e Email
     * @param p Password
     */
    public String loginU (String e, String p){
        for (Utilizador a : this.users.values()){
            if (a.getEmail().equals(e) && a.getPass().equals(p)) return a.getCod();
        }
        return null;
    }

    /**Método que regista um novo Utilizador no sistema (sign up)
     *
     * @param cod Código Utilizador
     * @param nome Nome Utilizador
     * @param cord Coordenadas
     * @param pass Password
     * @param email Email
     */
    public void signupU (String cod, String nome, Coordenadas cord, String pass, String email){
        this.users.put(cod,new Utilizador(cod, nome, cord, pass, email));
    }

    /**Método que gera um código único de Utilizador*/
    public String codUnicoU (){
        String a = null;
        while (this.users.containsKey(a) || a == null) {
            StringBuilder sb = new StringBuilder();
            sb.append("u").append(ThreadLocalRandom.current().nextInt(10, 99));
            a = sb.toString();
        }
        return a;
    }

    /**Método que retorna um Utilizador do catálogo procurando pelo seu código
     *
     * @param codigo Código Utilizador
     */
    public Utilizador procUser(String codigo){
        return this.users.get(codigo).clone();
    }

    /** Método que retorna as Coordenadas de um determinado Utilizador
     *
     * @param user Código Utilizador
     */
    public Coordenadas getCord (String user){
        return this.users.get(user).getCord().clone();
    }

    /**Método que retorna true se o email do Utilizador estiver registado no catálogo
     *
     * @param email Email
     */
    public boolean verMail (String email){
        return this.users.values().stream().anyMatch(x->x.getEmail().equals(email));
    }
}
