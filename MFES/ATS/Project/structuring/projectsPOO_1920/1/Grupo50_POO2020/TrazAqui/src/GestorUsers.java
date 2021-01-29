import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class GestorUsers implements Serializable {
    private Map<String, User> users;

    public GestorUsers() {
        this.users = new HashMap<String, User>();
    }

    public GestorUsers(Map<String, User> users) {
        this.users = users;
    }

    public GestorUsers(GestorUsers umaBaseDados) {
        this.users = umaBaseDados.getUsers();
    }


    /**
     * Getters & Setters
     */

    /**
     * Obter uma copia do mapeamento das Users
     *
     * @return Map<String: código da User, User>
     */
    public Map<String, User> getUsers() {
        return this.users;
    }

    /**
     * @param novasUsers
     */
    public void setUsers(Map<String, User> novasUsers) {
        this.users = novasUsers;
    }

    /**
     * Verificar a existência de uma encomenda dado o seu codigo
     *
     * @param codigo da encomenda
     * @return True se a encomenda existir
     */
    public boolean existeUser(String codigo) {
        return this.users.containsKey(codigo);
    }

    /**
     * @return Número de users
     */
    public int numUsers() {
        return this.users.size();
    }

    /**
     * Adiciona a informação de uma nova encomenda
     *
     * @param e nova encomenda a inserir
     */
    public void addUser(User e) throws UserJaExisteException {
        if (this.users.putIfAbsent(e.getEmail(), e.clone()) != null)
            throw new UserJaExisteException("Já existe uma user com o essa referência " + e.getEmail() + " na App");
    }

    /**
     * @param cod código da encomenda a procurar
     * @return cópia da encomenda encontrada
     * @throws UserInexistenteException caso o código da encomenda não exista
     */
    public User getUser(String cod) throws UserInexistenteException {
        User l;

        try {
            l = this.users.get(cod);
        } catch (NullPointerException e) {
            throw new UserInexistenteException("Não existe user com nome " + cod);
        }
        return l;
    }

    public int tamanho(){
        return this.users.size();
    }

    /**
     * @return Lista das users
     */
    public List<User> getUserAsList() {
        return users.values()
                .stream()
                .map(User::clone)
                .collect(Collectors.toList());
    }

    @Override
    public String toString() {
        return "GestorUsers{" +
                "users=" + users +
                '}';
    }
}