import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Random;

/**
 * Classe que trata dos voluntarios que entregam as encomendas
 * 
 * @author Rui Cunha
 * @version 06/04/2020
 */
public class Voluntario extends Transportador
{
    private int idade;
    private String sexo;
    
    //Construtor vazio
    public Voluntario()
    {
        super();
        this.idade = 0;
        this.sexo = "N/A";
    }
    
    //Construtor por parametros
    public Voluntario(String username, String nome, String password,Localizacao posicao,double raio,
                      boolean transport, boolean transporte_medico, Map<String,Integer> classificacao, HashMap<String,Encomenda> encomendas, int idade, String sexo)
    {
        super(username,nome,password,posicao,raio, transport,transporte_medico,classificacao,encomendas);
        this.idade = idade;
        this.sexo = sexo;
    }
    
    //Construtor por copia
    public Voluntario(Voluntario vol)
    {
        super(vol);
        this.idade = vol.getIdade();
        this.sexo = vol.getSexo();
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
    public Voluntario clone()
    {
        return new Voluntario(this);
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
        .append("Sexo : ").append(this.sexo).append("\n");
        return sb.toString();
    }
    
    //Metodo Equals
    public boolean equals(Object o)
    {
        if(this == o)
            return true;
        if((o==null) || (this.getClass()!=o.getClass()))
            return false;
        
        Voluntario p = (Voluntario) o;
        return(super.equals(p) &&
                this.idade == p.getIdade() &&
                this.sexo.equals(p.getSexo()));
    }
}
