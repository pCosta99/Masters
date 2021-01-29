package Perfis;

import java.io.Serializable;

public abstract class Perfil implements Serializable
{
    private String email;
    private String password;
    private String nome;
    private Ponto2D local;
    //
    /**
     * COnstrutor para objetos da classe Utilizador.Perfil
     */
    public Perfil(){
        this.email= "";
        this.nome= "";
        this.password= "";
        this.local=new Ponto2D();
    }
    /**
     * Construtor parametrizado de Utilizador.Perfil.
     * Aceita como parâmetros os valores para cada variavel.
     */
    public Perfil(String email, String nome, String passoword, Ponto2D local){
        this.email=email;
        this.nome=nome;
        this.password=passoword;
        this.local=local;
    }
    /**
     * Construtor de cópia de Utilizador.Perfil.
     * Aceita como parâmetro outro Utilizador.Perfil e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */
    public Perfil(Perfil umPerfil){
        this.email=umPerfil.getEmail();
        this.nome=umPerfil.getNome();
        this.password=umPerfil.getPassword();
        this.local=umPerfil.getLocal();
    }

    /**
     * Devolve o valor do Email.
     * 
     * @return o email.
     */
    public String getEmail(){
        return this.email;
    }
    public String getNome(){
        return this.nome;
    }
    public String getPassword(){
        return this.password;
    }
    public Ponto2D getLocal(){
        return this.local;
    }
    
    /**
     * Actualiza o Email.
     * 
     * @param nEmail novo endereço de email.
     */
    public void setEmail(String nEmail){
        this.email=nEmail;
    }
    public void setNome(String nNome){
        this.nome=nNome;
    }
    public void setPassword(String nPassoword){
        this.password=nPassoword;
    }
    public void setLocal(Ponto2D nLocal){
        this.local=nLocal;
    }

    public abstract Perfil clone();
    
    /**
     * Método que devolve a representação em String do Utilizador.Perfil.
     * 
     * @return String com a Email, Passoword, Nome e Local. 
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizador.Perfil: ").append(this.email).append("\n")
                             .append(this.nome).append("\n")
                             .append(this.password).append("\n")
                             .append(this.local.toString()).append("\n");
        return sb.toString();
    }
    
    public boolean equals (Object o){
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Perfil a = (Perfil) o;
        return this.email.equals(a.getEmail()) &&
               this.password.equals(a.getPassword())&&
               this.local.equals(a.getLocal());
    }
    
    public boolean validaLogin(String pass){
        return pass.equals(this.password);
    }
}
