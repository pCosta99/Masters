import java.io.*;

public class Voluntario extends Entidade implements Serializable
{
    /**Raio de ação no qual um voluntário poderá atuar*/
    private double raioVol;
    /**Estado de um voluntário, que varia entre inativo, ocupado e ativo*/
    private String estado;
    
     /** Construtor vazio de Voluntario*/
    public Voluntario(){
        super();
        this.raioVol = 0.0;
        this.estado = "Inativo";
    }
    
     /** Construtor com todas as variaveis de instancia do Voluntario*/
    public Voluntario(String cv, String nv, double gpsx, double gpsy, String e, String pass, double rv, String est){
        super(cv,nv,gpsx,gpsy,e,pass);
        this.raioVol = rv;
        this.estado = est;
    }
    
     /** Construtor com um Voluntario*/
    public Voluntario(Voluntario v){
        super(v);
        this.raioVol = v.getRaioVol();
        this.estado = v.getEstado();
    }
    
     /** 
      * Metodo que retorna o raio de ação de um voluntário
     * 
     * @return Raio de ação de um voluntário
     */
    public double getRaioVol(){
        return this.raioVol;
    }
    
    /** 
      * Metodo que retorna o estado de um voluntário
     * 
     * @return Estado de um voluntário
     */
    public String getEstado(){
        return this.estado;
    }
    
     /**
     * Metodo que altera o raio de ação de um voluntário
     * 
     * @param rv valor no qual o raio de ação de um voluntário vai ser alterado
     */
    public void setRaioVol(double rv){
        this.raioVol = rv;
    }
    
     /**
     * Metodo que altera o estado de um voluntário
     * 
     * @param est valor no qual o estado de um voluntário vai ser alterado
     */
    public void setEstado(String est){
        this.estado = est;
    }
    
    /** 
     * Metodo retorna uma copia do Voluntario
     * 
     * @return Uma copia do Voluntario
     */
    public Voluntario clone(){
        return new Voluntario(this);
    }
    
     /** 
     * Metodo que "transforma" um Voluntario numa String
     * 
     * @return Uma String do Voluntario
     */
    public String toString(){
        String s = "Código: " + getCod() + 
        "\nNome: " + getNome() + 
        "\nCoordenadas: " + getX() + ", " + getY() + 
        "\nEmail: " + getEmail() +
        "\nPass: " + getPassword() +
        "\nRaio do Voluntário: " + raioVol +
        "\nEstado: " + estado;
        
        return s;
    }
    
     /** 
     * Metodo que verifica se um Objeto é igual a um Voluntario
     * 
     * @param o Objeto que irá ser comparado com o voluntário
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
        
        Voluntario v = (Voluntario) o;
        
        if(super.equals(v) && this.raioVol == v.getRaioVol() && estado.equals(v)) 
        return true;
        
        return b;
    }
}
