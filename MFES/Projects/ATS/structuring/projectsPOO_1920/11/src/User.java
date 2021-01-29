import java.io.Serializable;
import java.time.LocalDate;
/**
 *Classe abstrata dos Utilizadores
 * 
 * @author Rui
 * @version 06/04/2020
 */
public abstract class User implements Serializable
{
    private String username; //nome de utilizador para aceder a aplicacao
    private String nome; //nome do utilizador
    private String password;
    private Localizacao posicao; //posicao atual do utilizador
    //raio de acao
    
    //Construtor por omissao
    public User()
    {
        this.posicao = new Localizacao();
        this.username = "N/A";
        this.nome = "N/A";
        this.password = "N/A";
    }
    
    //Construtor por parametros
    public User(String username, String nome, String password,Localizacao local)
    {
        this.username = username;
        this.nome = nome;
        this.password = password;
        this.posicao = local.clone();
    }
    
    //Construtor copia
    public User(User newUser)
    {
        this.username = newUser.getUsername();
        this.nome = newUser.getNome();
        this.password = newUser.getPassword();
        this.posicao = newUser.getPosicao();
    }
    
    //Gets
    public String getUsername()
    {
        return this.username;
    }
    
    public String getNome()
    {
        return this.nome;
    }

    public String getPassword() {
        return this.password;
    }

    public Localizacao getPosicao()
    {
        return this.posicao.clone();
    }
    
    //Sets
    public void setUsername(String username)
    {
        this.username = username;
    }
    
    public void setNome(String nome)
    {
        this.nome = nome;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public void setPosicao(Localizacao local)
    {
        this.posicao = local.clone();
    }
    
    //Equals
    public boolean equals(Object o)
    {
        if(o==this) return true;
        if(o==null||o.getClass() != this.getClass()) return false;
        User t = (User) o;
        return this.username.equals(t.getUsername()) && 
               this.nome.equals(t.getNome()) &&
                this.password.equals(t.getNome()) &&
               this.posicao.equals(t.getPosicao());
    }
    
    //toString
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("\nUsername: ").append(this.username + "\n")
          .append("\nNome: ").append(this.nome + "\n")
          .append("\nPassword: ").append(this.password + "\n")
            .append(this.posicao.toString() + "\n");
        return sb.toString();
    }
    
    //Clone
    public abstract User clone();
}
