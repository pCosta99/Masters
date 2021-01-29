package Model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import Exceptions.*;

public class UsersDB implements Serializable{
    private static final long serialVersionUID = -988907197124157895L;
    private Map<String, User> baseDadosUti;

    UsersDB() {
        this.baseDadosUti = new HashMap<>();
    }

    private UsersDB(UsersDB u) {
        this.baseDadosUti = u.baseDadosUti.values()
                                          .stream()
                                          .collect(Collectors
                                          .toMap(User::getCodUtilizador, User::clone));
    }


    /**
     * Adiciona um utilizador à base de dados
     */
    void addUtilizador(User u) throws UserJaExisteException{

       if(this.baseDadosUti.putIfAbsent(u.getCodUtilizador(), u) !=null)
           throw new UserJaExisteException();
    }

    /**
     * Metodo que devolve a lista de todos os codigos de utiliazadores da base de dados
     */
    List<String> getClientIDS() {
        return new ArrayList<>(this.baseDadosUti.keySet());
    }

    /**
     * metodo que dado um codigo de utilizador devolve o user correspondente
     */
    User getUser(String id) throws UserInexistenteException {
        User a = baseDadosUti.get(id);
        if(a == null)
            throw new UserInexistenteException();
        return a;
    }

    /**
     * metodo que dada uma encomenda a adiciona ao respetivo utilizador 
     */
    void addEncomenda (Encomendas e){
        String uID = e.getCodUtilizador();
        this.baseDadosUti.get(uID).addEncHist(e); 
    }

    /**
     * Metodo que devolve todos os utilizadores da base de dados
     */
    public Set<User> getUsers(){
        return this.baseDadosUti.values().stream().collect(Collectors.toSet());
    }

    /**
     * Metodo que devolve um utilizador dado o sei email
     */
    public User getUserEmail(String email) throws NaoExisteEmailException{
        List<User> l = new ArrayList<>();
        l = this.baseDadosUti.values().stream().filter(u -> u.getEmail().equals(email)).collect(Collectors.toList());
        if(l.size()>0){
            return l.get(0);
        }
        else {
            throw new NaoExisteEmailException();
        }
    }

    /**
     * Metodo que verifica se existe um utilizador com um dado email
     */
    public void verificaMail(String email) throws EmailExistenteException{
        List<User> l = new ArrayList<>();
        l = this.baseDadosUti.values().stream().filter(u -> u.getEmail().equals(email)).collect(Collectors.toList());
        if (l.size()>0){
            throw new EmailExistenteException();
        }
    }

    public UsersDB clone() { return new UsersDB(this);}

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        UsersDB users = (UsersDB) o;
        return this.baseDadosUti.equals(users.baseDadosUti);
    }

    @Override
    public int hashCode() {
        return 1;
    }

}