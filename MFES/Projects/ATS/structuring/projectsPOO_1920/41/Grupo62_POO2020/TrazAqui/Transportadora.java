import java.io.*;

public class Transportadora extends Entidade implements Serializable
{
    /**NIF da Empresa*/
    private int nifEmp;
    /**Raio de ação no qual uma empresa poderá atuar*/
    private double raioEmp;
    /**Preço da entrega por quilómetro percorrido*/
    private double precoPorKm;
    
     /** Construtor vazio de Transportadora*/
    public Transportadora(){
        super();
        this.nifEmp = 0;
        this.raioEmp = 0.0;
        this.precoPorKm = 0.0;
    }
    
     /** Construtor com todas as variaveis de instancia da Transportadora*/
    public Transportadora(String ce, String ne, double gpsx, double gpsy, String e, String pass, int nife, double re, double ppkm){
        super(ce,ne,gpsx,gpsy,e,pass);
        this.nifEmp = nife;
        this.raioEmp = re;
        this.precoPorKm = ppkm;
    }
    
     /** Construtor com uma Transportadora*/
    public Transportadora(Transportadora t){
        super(t);
        this.nifEmp = t.getNifEmp();
        this.raioEmp = t.getRaioEmp();
        this.precoPorKm = t.getPrecoPorKm();
    }
    
     /** 
      * Metodo que retorna o NIF de uma empresa
     * 
     * @return NIF de uma empresa
     */
    public int getNifEmp(){
        return this.nifEmp;
    }

     /**
     * Metodo que altera o NIF de uma Empresa
     * 
     * @param nife valor no qual o NIF de uma empresa vai ser alterado
     */
    public void setNifEmp(int nife){
        this.nifEmp = nife;
    }
    
     /** 
      * Metodo que retorna o raio de ação de uma empresa
     * 
     * @return Raio de ação de uma empresa
     */
    public double getRaioEmp(){
        return this.raioEmp;
    }

     /**
     * Metodo que altera o raio de ação de uma empresa
     * 
     * @param re valor no qual o raio de ação de uma empresa vai ser alterado
     */
    public void setRaioEmp(double re){
        this.raioEmp = re;
    }
    
     /** 
      * Metodo que retorna o preço por km numa entrega
     * 
     * @return Preço por km
     */
    public double getPrecoPorKm(){
        return this.precoPorKm;
    }

     /**
     * Metodo que altera o preço por km
     * 
     * @param ppkm valor no qual o preço por km vai ser alterado
     */
    public void setPrecoPorKm(double ppkm){
        this.precoPorKm = ppkm;
    }
    
    /** 
     * Metodo retorna uma copia da Transportadora
     * 
     * @return Uma copia da Transportadora
     */
    public Transportadora clone(){
        return new Transportadora(this);
    }
    
    /** 
     * Metodo que "transforma" uma Transportadora numa String
     * 
     * @return Uma String da Transportadora
     */
    public String toString(){
        String s = "Código: " + getCod() + 
        "\nNome: " + getNome() + 
        "\nCoordenadas: " + getX() + ", " + getY() +
        "\nEmail: " + getEmail() +
        "\nPassword: " + getPassword() +
        "\nNIF: " + nifEmp + 
        "\nRaio da Transportadora: " + raioEmp + 
        "\nPreço por Km: " + precoPorKm;
        
        return s;
    }
    
     /** 
     * Metodo que verifica se um Objeto é igual a uma Transportadora
     * 
     * @param o Objeto que irá ser comparado com a transportadora
     * 
     * @return true se forem iguais, false caso contrário
     */
    public boolean equals(Object o){
        
        boolean b = false;
        if(this == o){
            return true;
        }
        
        if(o == null || this.getClass() != o.getClass()){
            return false;
        }
        
        Transportadora t = (Transportadora) o;
        
        if(super.equals(t) && this.nifEmp == t.getNifEmp() && this.raioEmp == t.getRaioEmp() && this.precoPorKm == t.getPrecoPorKm()) 
        return true;
        
        return b;
    }
    
    public double precoTransporte(Sistema s, Loja l, Utilizador u, Transportadora t){
        double p = 0.0;
        p = l.tempoEspera() * 0.2 + (s.distanciaEntidade(l, t) + s.distanciaEntidade(t,u)) * t.getPrecoPorKm();
        return p;
    }
}
