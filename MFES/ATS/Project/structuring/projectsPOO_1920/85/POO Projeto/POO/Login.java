/**
 * Escreva a descrição da classe Login aqui.
 *
 * @author (seu nome)
 * @version (número de versão ou data)
 */

public class Login implements LoginI, java.io.Serializable{
    // Variaveis de instancia
    private String codigo;
    private String nome;
    private String password;

    /**
     * Construtores para objetos da classe Login
     */
    public Login(){
        this.codigo = "";
        this.nome = "";
        this.password = "";
    }

    public Login(String codigo, String email, String password) {
        this.codigo = codigo;
        this.nome = email;
        this.password = password;
    }

    public Login(Login lg){
        this.codigo = lg.getCodigo();
        this.nome = lg.getNome();
        this.password = lg.getPassword();
    }

    /**
     * Metodos gets e sets,
     * clone, equals e toString
     */
    public String getCodigo() {
        return codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public String getNome() {
        return this.nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public String getPassword() {
        return this.password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public Login clone(){return new Login(this);}

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Login login = (Login) o;
        return login.getCodigo().equals(this.codigo) &&
                login.getNome().equals(this.nome) &&
                login.getPassword().equals(this.password);
    }

    public String toString() {
        StringBuilder sb;
        sb = new StringBuilder();
        sb.append("Login\n" + "Nome: ")
                .append(this.nome).append('\n')
                .append("Password: ");
        return sb.toString();
    }

    /**
     * Metodo que le o conteudo de uma String e cria um login numa string
     */
    public void leTA(String cod, String[] p){
        setCodigo(cod);
        setNome(p[1]);
        setPassword(p[2]);
    }
}
