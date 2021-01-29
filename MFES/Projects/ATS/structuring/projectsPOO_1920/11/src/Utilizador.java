import java.util.Map;
import java.util.HashMap;
import java.util.Random;

/**
 * Classe que tratas das pessoas que querem encomendar algo
 * 
 * @author Rui Cunha
 * @version 06/04/2020
 */
public class Utilizador extends User
{
    private int idade;
    private String sexo;
    
    //Construtor vazio
    public Utilizador()
    {
        super();
        this.idade = 0;
        this.sexo = "N/A";
    }
    
    //Construtor por parametros
    public Utilizador(String username, String nome, String password,Localizacao local, int idade, String sexo)
    {
        super(username,nome,password,local);
        this.idade = idade;
        this.sexo = sexo;
    }
    
    //Construtor por copia
    public Utilizador(Utilizador util)
    {
        super(util);
        this.idade = util.getIdade();
        this.sexo = util.getSexo();
    }
    
    //Getters
    public int getIdade()
    {
        return this.idade;
    }
    
    public String getSexo()
    {
        return this.sexo;
    }
    
    //Clone
    public Utilizador clone()
    {
        return new Utilizador(this);
    }
    
    //Setters
    public void setIdade(int idade)
    {
        this.idade = idade;
    }
    
    public void setSexo(String sexo)
    {
        this.sexo = sexo;
    }

    public String getRandomSexo(){
        Random r = new Random();
        boolean s = r.nextBoolean();
        if(s) {
            return "Feminino";
        }
        else{
            return "Masculino";
        }
    }

    //Metodo toString
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
        .append("Idade : ").append(this.idade + "\n")
        .append("Sexo : ").append(this.sexo + "\n");
        return sb.toString();
    }
    
    //Metodo Equals
    public boolean equals(Object o)
    {
        if(this == o)
            return true;
        if((o==null) || (this.getClass() != o.getClass()))
            return false;
            
        Utilizador p = (Utilizador) o;
        return(super.equals(p) &&
                this.idade ==p.getIdade() &&
                this.sexo.equals(p.getSexo()));
    }
}
