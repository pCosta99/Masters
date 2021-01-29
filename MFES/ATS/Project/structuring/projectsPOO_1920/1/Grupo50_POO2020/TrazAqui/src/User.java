import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Classe que implementa um Usuário para efeitos de log in
 *
 * @author monteiro06
 * @version 20200404
 */

public class User  implements Serializable {
    //variáveis de instância
    private String nome;
    private String email;
    private String password;
    private String cliente;
    private String estafeta;
    private List<String> lojas;
    private List<String> distribuidoras;

    /**
     * Construtores da classe User
     * Declaração dos construtores por omissão (vazio), parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de User.
     */
    public User(){
        this.nome = "n/a";
        this.email = "n/a";
        this.password = "n/a";
        this.cliente = "n/a";
        this.estafeta = "n/a";
        this.lojas = new ArrayList<>();
        this.distribuidoras = new ArrayList<>();
    }

    public User(String umUsername, String umEmail, String umaPassword){
        this.nome = umUsername;
        this.email = umEmail;
        this.password = umaPassword;
        this.cliente = "n/a";
        this.estafeta = "n/a";
        this.lojas = new ArrayList<>();
        this.distribuidoras = new ArrayList<>();
    }

    /**
     * Construtor parametrizado de User
     */

    public User(String umNome, String umEmail, String umaPassword, String umCliente, String umEstafeta, List<String> umasLojas, List<String> umasDistribuidoras){
        this.nome = umNome;
        this.email = umEmail;
        this.password = umaPassword;
        this.cliente = umCliente;
        this.estafeta = umEstafeta;
        this.lojas = umasLojas;
        this.distribuidoras = umasDistribuidoras;
    }


    /**
     * Construtor de cópia de User.
     * Aceita como parâmetro outro User e utiliza os métodos de acesso aos valores das variáveis de instância.
     * @param newUser
     */
    public User(User newUser){
        this.nome = newUser.getName();
        this.email = newUser.getEmail();
        this.password = newUser.getPassword();
        this.cliente = newUser.getCliente();
        this.estafeta = newUser.getEstafeta();
        this.lojas = newUser.getLojas();
        this.distribuidoras = newUser.getDistribuidoras();
    }

    public User clone(){
        return new User(this);
    }

    /**
     * Métodos de instância
     */

    /**
     * Devolve o valor do nome do user
     * @return name
     */
    public String getName() {
        return this.nome;
    }

    public String getEmail() {
        return this.email;
    }

    public String getPassword() {
        return this.password;
    }

    public String getCliente() {
        return this.cliente;
    }

    public String getEstafeta(){
        return this.estafeta;
    }

    public List<String> getLojas() {
        return this.lojas;
    }

    public List<String> getDistribuidoras() {
        return this.distribuidoras;
    }

    /**
     * Atualiza o valor do nome.
     * @param newName novo valor do nome
     */
    public void setName(String newName){
        this.nome = newName;
    }

    public void setEmail(String novoEmail) {
        this.email = novoEmail;
    }

    public void setPassword(String novaPassword) {
        this.password = novaPassword;
    }

    public void setCliente(String novoCliente) {
        this.cliente = novoCliente;
    }

    public void setEstafeta(String novoEstafeta) {
        this.estafeta = novoEstafeta;
    }

    public void setLojas(List<String> novasLojas) {
        this.lojas = novasLojas;
    }

    public void setDistribuidoras(List<String> novasDistribuidoras) {
        this.distribuidoras = novasDistribuidoras;
    }

    public void adicionaLoja(String umaLoja){
        if(!this.lojas.contains(umaLoja))
            this.lojas.add(umaLoja);
    }

    public void adicionaDistribuidora(String umaDistribuidora) throws EntidadeRepetidaException {
        if(this.distribuidoras.contains(umaDistribuidora))
            throw new EntidadeRepetidaException("Já existe uma empresa com o código " + umaDistribuidora + " na aplicação!");
        else{
            this.distribuidoras.add(umaDistribuidora);
        }
    }

    @Override
    public String toString() {
        return "User{" + '\n' + '\t' +
                "Nome='" + nome + '\n' + '\t' +
                "Email='" + email + '\n' + '\t' +
                "Password='" + password + '\n' + '\t' +
                "Cliente='" + cliente + '\n' + '\t' +
                "Estafeta='" + estafeta + '\n' + '\t' +
                "Lojas=" + lojas + '\n' + '\t' +
                "Distribuidoras=" + distribuidoras + '\n' +
                '}';
    }

    /**
     * Método que determina se dois users são iguais.
     * @return booleano que é verdadeiro se o valor do email for igual
     */



    public boolean equals(Object o){
        if (this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        User u = (User) o;
        return (this.email.equals(u.getEmail()));
    }

}
