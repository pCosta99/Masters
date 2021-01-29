package Model;


import java.util.Set;
import java.util.TreeSet;

public abstract class AtorSistema implements java.io.Serializable{

    /**
     * Código da loja
     */
    private String cod;
    /**
     * Email da loja
     */
    private String email;
    /**
     * Nif da loja
     */
    private String nif;
    /**
     * Nome da loja
     */
    private String nome;
    /**
     * Password da loja
     */
    private String password;
    /**
     * Localização da loja
     */
    private Coordenadas localizacao;
    /**
     * Conjunto de encomendas feitas pela loja
     */
    private Set<String> encomendas;

    /**
     * Construtor vazio
     */
    public AtorSistema(){
        this.cod = "";
        this.email = "";
        this.nif = "";
        this.nome = "";
        this.password = "";
        this.localizacao = new Coordenadas();
        this.encomendas = new TreeSet<String>();
    }

    /**
     * Construtor parametrizado
     *
     * @param cod
     * @param email
     * @param nif
     * @param nome
     * @param password
     * @param localizacao
     */
    public AtorSistema(String cod, String email, String nif, String nome, String password, Coordenadas localizacao) {
        this.cod = cod;
        this.email = email;
        this.nif = nif;
        this.nome = nome;
        this.password = password;
        this.localizacao = localizacao.clone();
        this.encomendas = new TreeSet<String>();
    }

    /**
     * Construtor parametrizado
     *
     * @param cod
     * @param email
     * @param nif
     * @param nome
     * @param password
     * @param localizacao
     * @param encomendas
     */
    public AtorSistema(String cod, String email, String nif, String nome, String password, Coordenadas localizacao, Set<String> encomendas) {
        this.cod = cod;
        this.email = email;
        this.nif = nif;
        this.nome = nome;
        this.password = password;
        this.localizacao = localizacao.clone();
        this.encomendas = new TreeSet<String>();
        this.encomendas.addAll(encomendas);
    }

    /**
     * Construtor por cópia
     *
     * @param atorSistema
     */
    public AtorSistema(AtorSistema atorSistema) {
        setCod(atorSistema.getCod());
        setEmail(atorSistema.getEmail());
        setNif(atorSistema.getNif());
        setNome(atorSistema.getNome());
        setPassword(atorSistema.getPassword());
        setLocalizacao(atorSistema.getLocalizacao());
        setEncomendas(atorSistema.getEncomendas());
    }

    /**
     * Devolve o cod
     *
     * @return
     */
    public String getCod() {
        return this.cod;
    }

    /**
     * Atualiza o cod
     *
     * @param cod
     */
    public void setCod(String cod) {
        this.cod = cod;
    }

    /**
     * Devolve o email
     *
     * @return
     */
    public String getEmail() {
        return this.email;
    }

    /**
     * Atualiza o email
     *
     * @param email
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * Devolve o nif
     *
     * @return
     */
    public String getNif() {
        return this.nif;
    }

    /**
     * Atualiza o nif
     *
     * @param nif
     */
    public void setNif(String nif) {
        this.nif = nif;
    }

    /**
     * Devolve o nome
     *
     * @return
     */
    public String getNome() {
        return this.nome;
    }

    /**
     * Atualiza o nome
     *
     * @param nome
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Devolve a password
     *
     * @return
     */
    public String getPassword() {
        return this.password;
    }

    /**
     * Atualiza a password
     *
     * @param password
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * Devolve a localizacao
     *
     * @return
     */
    public Coordenadas getLocalizacao() {
        return this.localizacao.clone();
    }

    /**
     * Atualiza a localizacao
     *
     * @param localizacao
     */
    public void setLocalizacao(Coordenadas localizacao) {
        this.localizacao = localizacao.clone();
    }

    /**
     * Devolve as encomendas
     *
     * @return
     */
    public Set<String> getEncomendas() {
        return new TreeSet<>(this.encomendas);
    }

    /**
     * Atualiza as encomendas
     *
     * @param encomendas
     */
    public void setEncomendas(Set<String> encomendas) {
        this.encomendas = new TreeSet<>();
        this.encomendas.addAll(encomendas);
    }

    /**
     * Cria uma cópia da instância
     *
     * @return
     */
    public abstract AtorSistema clone();

    /**
     * Verifica a igualdade com outro objeto
     *
     * @param o
     * @return
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AtorSistema atorSistema = (AtorSistema) o;

        return this.cod.equals(atorSistema.getCod()) &&
                this.email.equals(atorSistema.getEmail()) &&
                this.nif.equals(atorSistema.getNif()) &&
                this.nome.equals(atorSistema.getNome()) &&
                this.password.equals(atorSistema.getPassword()) &&
                this.localizacao.equals(atorSistema.getLocalizacao()) &&
                this.encomendas.equals(atorSistema.getEncomendas());
    }

    /**
     * Devolve uma representação textual de Utilizador
     *
     * @return
     */
    @Override
    public String toString() {
        return "AtorSistema{" +
                "cod='" + cod + '\'' +
                ", email='" + email + '\'' +
                ", nif='" + nif + '\'' +
                ", nome='" + nome + '\'' +
                ", password='" + password + '\'' +
                ", localizacao=" + localizacao +
                '}';
    }

    /**
     * Método que adiciona o código de uma encomenda a
     * uma coleção de encomendas
     *
     * @param encomenda
     */
    public void addEncomenda(String encomenda) {
        this.encomendas.add(encomenda);
    }
}
