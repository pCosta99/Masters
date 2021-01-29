import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * Implementação do controlador principal do programa
 */
public class Contas implements Serializable {
    private Map<String, String> emails;
    private Map<String, String> passwords;

    /**
     * Construtor por omissão
     */
    public Contas() {
        this.emails = new HashMap<>();
        this.passwords = new HashMap<>();
    }

    /**
     * Construtor parametrizado
     */
    public Contas(Map<String, String> emails, Map<String, String> passwords) {
        setEmails(emails);
        setPasswords(passwords);
    }

    /**
     * Construtor parametrizado
     */
    public Contas(Contas c) {
        this.emails = c.getEmails();
        this.passwords = c.getPasswords();
    }

    /**
     * Devolve emails
     */
    private Map<String, String> getEmails() {
        Map<String, String> res = new HashMap<>();
        for (Map.Entry<String, String> par : this.emails.entrySet()) {
            res.put(par.getKey(), par.getValue());
        }
        return res;
    }

    /**
     * Carrega os emails
     * @param emails Emails a adicionar
     */
    private void setEmails(Map<String, String> emails) {
        this.emails = new HashMap<>(emails);
    }


    /**
     * Devolve passwords
     */
    private Map<String, String> getPasswords() {
        Map<String, String> res = new HashMap<>();
        for (Map.Entry<String, String> par : this.emails.entrySet()) {
            res.put(par.getKey(), par.getValue());
        }
        return res;
    }


    /**
     * Carrega passwords
     * @param passwords Passwords a adicionar
     */
    private void setPasswords(Map<String, String> passwords) {
        this.passwords = new HashMap<>(passwords);
    }


    /**
     * Carrega um Registo
     * @param codigo Codigo da entidade
     * @param email Email a adicionar
     * @param password Password a adicionar
     */
    public void adicionarRegisto(String codigo, String email, String password) {
        emails.put(codigo, email);
        passwords.put(codigo, password);
    }

    /**
     * Verifica se um login está correto
     * @param email Email a testar
     * @param password Password a testar
     * @return boolean que indica se está correto
     */
    public boolean loginCorreto(String email, String password) {
        String[] parse = email.split("@");
        return (this.emails.containsKey(parse[0]) && this.passwords.containsKey(parse[0]));
    }

    /**
     * Verifica se existe uma conta
     * @param codigo Email a testar
     * @return boolean que indica se existe
     */
    public boolean existeConta(String codigo) {
        return emails.containsKey(codigo); /* Se existe nos emails também existe nas passwords */
    }

    /**
     * Verifica se existe uma password
     * @param codigo codigo de utilizador a testar
     * @param pass password a testar
     * @return boolean que indica se existe
     */
    public boolean existePass(String codigo, String pass) {
        return passwords.get(codigo).equals(pass);
    }


    /**
     * Devolve info da conta
     */
    public void info() {
        for (Map.Entry<String, String> par : emails.entrySet()) System.out.println(par);
        for (Map.Entry<String, String> par : passwords.entrySet()) System.out.println(par);
    }

    /**
     * Coloca em String
     */
    public String toString() {
        final StringBuilder sb = new StringBuilder("Contas{");
        sb.append("emails=").append(emails);
        sb.append(", passwords=").append(passwords);
        sb.append('}');
        return sb.toString();
    }
}
