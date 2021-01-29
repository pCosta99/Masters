import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public abstract class AllUsers implements Serializable
{
    // instance variables - replace the example below with your own
    private String code;
    private String user;
    private Coordenadas gps;//todo
    private String email;
    private String password;
    private List <Integer> classificacao;
    


    public AllUsers()
    {
        // put your code here
        this.user="";
        this.code="";
        this.gps= new Coordenadas();
        this.email="";
        this.password="";
        this.classificacao=new ArrayList<>();
    }
    
    public AllUsers (String codigo, String nome, Coordenadas xyr, String mail, String pw, List <Integer> reviews )
    {
        this.user=nome;
        this.code=codigo;
        this.gps=xyr;
        this.email=mail;
        this.password=pw;
        setClassificacao(reviews);
    }
    
    public AllUsers (AllUsers t)
    {
        this.user=t.getUser();
        this.code=t.getCode();
        this.gps=t.getGps();
        this.email=t.getEmail();
        this.password=t.getPassword();
        setClassificacao(t.getClassificacao());
        
    }
    
    public String getUser()
    {
        return this.user;
    }
    
    public String getCode()
    {
        return this.code;
    }
    
    public Coordenadas getGps()
    {
        return this.gps;
    }

    public String getEmail(){
        return this.email;
    }

    public String getPassword(){
        return this.password;
    }
    
    public List<Integer> getClassificacao()
    {
        return this.classificacao;
    }

    public void setUser(String nome)
    {
        this.user=nome;
    }
    
    public void setCode(String codigo)
    {
        this.code=codigo;
    }
    
    public void setGps(Coordenadas xyr)
    {
        this.gps=xyr;
    }

    public void setEmail(String mail){
        this.email=mail;
    }

    public void setPassword(String pw){
        this.password=pw;
    }
    
    public void setClassificacao(List<Integer> classificacao) {
        this.classificacao = classificacao;
    }

    public boolean equals (Object o)
    {
        if (o==this) return true;
        if (o==null || o.getClass() != this.getClass()) return false;
        
        AllUsers t = (AllUsers) o;
        return (this.code.equals(t.getCode()));
    }
    
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("\nDescrição de utilizador:\n");
        sb.append("\nCodigo do Utilizador: "+this.code);
        sb.append("\nNome do Utilizador: "+this.user);
        sb.append("\nCoordenadas: "+this.gps);
        
        return sb.toString();
    }
    
    public void addclassificacao(int classificacao) //classificar voluntario ou empresa
    {
        List<Integer> novacl = new ArrayList<>();
        novacl = this.getClassificacao();
        novacl.add(classificacao);
        setClassificacao(novacl);
    }
    
    
    
    public String toStringCSV(AllUsers a)
    {
        StringBuilder sb = new StringBuilder();
        if (a.tipoUtilizador().equals("Utilizador")){
            sb.append("Utilizador:").append(this.code).append(",").append(this.user).append(",").append(this.gps);
        }
        if (a.tipoUtilizador().equals("Voluntario")){
            Voluntarios v = (Voluntarios) a;
            sb.append("Voluntario:").append(this.code).append(",").append(this.user).append(",").append(this.gps).append(",").append(v.getVRaio());
        }
        if (a.tipoUtilizador().equals("Transportadoras")){
            Transportadoras t = (Transportadoras) a;
            sb.append("Transportadora:").append(this.code).append(",").append(this.user).append(",").append(this.gps).append(",").append(t.getNIF()).append(",").append(t.getTRaio()).append(",").append(t.getPricekm());
        }
        if (a.tipoUtilizador().equals("Loja")){
            sb.append("Loja:").append(this.code).append(",").append(this.user).append(",").append(this.gps);
        }
        
        return sb.toString();
    }
    
    
    public abstract String tipoUtilizador();
    
    public abstract AllUsers clone();
    

    

    
}