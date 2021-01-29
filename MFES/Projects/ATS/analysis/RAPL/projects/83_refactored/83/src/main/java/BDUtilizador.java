import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

public class BDUtilizador implements Serializable {
    private Map<String, Utilizador> users;
    private Set<String> codigos;

    public BDUtilizador() {
        this.users = new HashMap<>();
        this.codigos = new TreeSet<>();
    }

    public BDUtilizador(BDUtilizador r) {
        setUsers(r.getUsers());
        setCodigos(r.getCodigos());
    }

    public Map<String, Utilizador> getUsers() {
        return this.users.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, r -> r.getValue().clone()));
    }

    public Set<String> getCodigos() {
        return new HashSet<>(this.codigos);
    }

    public void setCodigos(Set<String> codigos) {
        this.codigos = new TreeSet<>();
        this.codigos.addAll(codigos);
    }

    public void setUsers(Map<String, Utilizador> users) {
        this.users = new HashMap<>();
        users.forEach((key, value) -> this.users.put(key, value.clone()));
    }


    public BDUtilizador clone() {
        return (new BDUtilizador(this));
    }

    public String toString() {
        return "Total de utilizadores: " + "\n" +
                this.users;
    }

    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        BDUtilizador r = (BDUtilizador) obj;
        return this.users.equals((r.getUsers()));
    }

    /**
     * Método que verifica se um email já está registado
     */
    public boolean existeEmail(String email){
        return this.users.containsKey(email);
    }

    /**
     * Método que verifica se um utiizador existe
     */

    public boolean existe(Utilizador v){
        return this.users.containsKey(v.getEmail());
    }

    /**
     * Método que adiciona um utilizador
     */
    public void add(Utilizador u) {
        this.users.put(u.getEmail(), u.clone());
        this.codigos.add(u.getCodigo());
    }

    /**
     * Método que verifica se um código de utilizador existe
     */
    public boolean existeCodigo(String s){
        return this.codigos.contains(s);
    }

    /**
     * Método que adiciona uma encomenda a um utilizador
     */
    public void updateUser(Encomenda e, Utilizador u){
        u.addEncomenda(e);
        this.users.put(u.getEmail(), u);
    }

    /**
     * Método que efetua o login de um utilizador
     */
    public Utilizador tryLogin(String email, String password){
        if(!this.users.containsKey(email)) return null;
        else{
            Utilizador aux = this.users.get(email).clone();
            if(aux.getPassword().equals(password)){
                System.out.println("Login feito com sucesso");
                return aux;
            }
            else{
                System.out.println("Password incorreta");
                return null;
            }
        }
    }

    public String getEmail(String cod) throws UserNotFoundException{
        for(Utilizador u : users.values()){
            if(u.getCodigo().equals(cod)) return u.getEmail();
        }
        throw new UserNotFoundException();
    }

    /**
     * Método que atualiza um utilizador
     */
    public void updateUser2(Utilizador u){
        this.users.put(u.getEmail(), u.clone());
    }
}
