import java.io.Serializable;

/**
 * Classe com informação sobre Entidades.
 *
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public abstract class Entidade implements Serializable {
    // Defaults
    static private final String defaultNome = "n/d";
    static private final String defaultCodigo = "n/d";
    static private final String defaultSuffixMail = "@mail.com";
    static private final String defaultPassword = "1234";

    // Instance Variable
    private final String codigo;
    private final String nome;
    private String mail;
    private String password;
    private Localizacao localizacao;

    // Constructors

    /**
     * Construtor de uma Entidade.
     */
    public Entidade() {
        this.codigo = Entidade.defaultCodigo;
        this.nome = Entidade.defaultNome;
        this.mail = this.codigo + Entidade.defaultSuffixMail;
        this.password = Entidade.defaultPassword;
        this.localizacao = new Localizacao();
    }

    /**
     * Construtor de uma Entidade.
     * @param codigo Código da Entidade.
     * @param nome Nome da Entidade.
     * @param localizacao Localização da Entidade.
     */
    public Entidade(String codigo, String nome, Localizacao localizacao) {
        this.codigo = codigo;
        this.nome = nome;
        this.mail = codigo + Entidade.defaultSuffixMail;
        this.password = Entidade.defaultPassword;
        this.localizacao = localizacao.clone();
    }

    /**
     * Construtor de uma Entidade.
     * @param codigo Código da Entidade.
     * @param nome Nome da Entidade.
     * @param mail E-mail da Entidade.
     * @param password Password da Entidade.
     * @param localizacao Localização da Entidade.
     */
    public Entidade(String codigo, String nome, String mail, String password, Localizacao localizacao){
        this.codigo = codigo;
        this.nome = nome;
        this.mail = mail;
        this.password = password;
        this.localizacao = localizacao.clone();
    }

    /**
     * Construtor de uma Entidade por cópia.
     * @param e Entidade a copiar.
     */
    public Entidade(Entidade e) {
        this.codigo = e.getCodigo();
        this.nome = e.getNome();
        this.mail = e.getMail();
        this.password = e.getPassword();
        this.localizacao = e.getLocalizacao();
    }

    // Gets

    /**
     * Função que retorna o Nome da Entidade.
     * @return Nome da Entidade.
     */
    public String getNome() {
        return this.nome;
    }

    /**
     * Função que retorna o código da Entidade.
     * @return Código da Entidade.
     */
    public String getCodigo() {
        return this.codigo;
    }

    /**
     * Função que retorna o Mail da Entidade.
     * @return Mail da Entidade.
     */
    public String getMail() {
        return this.mail;
    }

    /**
     * Função que retorna a Password da Entidade.
     * @return Password da Entidade.
     */
    private String getPassword() {
        return this.password;
    }

    /**
     * Função que retorna a Localização da Entidade.
     * @return Localização da Entidade.
     */
    public Localizacao getLocalizacao() {
        return this.localizacao.clone();
    }

    // Sets

    /**
     * Função que modifica o Mail de uma Entidade.
     * @param mail Novo Mail.
     */
    public void setMail(String mail) {
        this.mail = mail;
    }

    /**
     * Função que modifica a Password de uma Entidade.
     * @param password Nova Password.
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * Função que modifica a Localização de uma Entidade.
     * @param gps Nova Localização.
     */
    public void setLocalizacao(Localizacao gps) {
        this.localizacao = gps;
    }

    //

    /**
     * Função que cria um clone de uma Entidade.
     * @return Entidade clonada.
     */
    public abstract Entidade clone();

    /**
     * Função que compara Entidades.
     * @param o Objeto a comparar.
     * @return true se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Entidade e = (Entidade) o;
        return this.nome.equals(e.getNome()) &&
               this.codigo.equals(e.getCodigo()) &&
               this.mail.equals(e.getMail()) &&
               this.password.equals(e.getPassword()) &&
               this.localizacao.equals(e.getLocalizacao());
    }

    /**
     * Função que converte os parâmetros de uma Entidade em String.
     * @return String com os parâmetros de uma Entidade.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Nome: ").append(this.nome).append("\n");
        sb.append("Codigo: ").append(this.codigo).append("\n");
        sb.append("GPS: ").append(this.localizacao).append("\n");
        return sb.toString();
    }
    //

    /**
     * Função que verifica a Password.
     * @param password password a verificar.
     * @return true se forem iguais.
     */
    public boolean checkPassword(String password){
        return this.password.equals(password);
    }
}
