public abstract class Entidade
{
    /**Código de identificaçao da entidade*/
    private String cod;
    /**Nome da entidade*/
    private String nome;
    /**Localização por GPS da entidade em latitude*/
    private double gpsx;
    /**Localização por GPS da entidade em longitude*/
    private double gpsy;
    /**Email da entidade*/
    private String email;
    /**Password da entidade*/
    private String password;
    
     /** Construtor vazio de Entidade*/
    public Entidade(){
        this.cod = "";
        this.nome = "";
        this.gpsx = 0.0;
        this.gpsy = 0.0;
        this.email = "";
        this.password = "";
    }
    
     /** Construtor com todas as variaveis de instancia da Entidade*/
    public Entidade(String codigo, String nome, double gpsx, double gpsy, String email, String password){
        this.cod = codigo;
        this.nome = nome;
        this.gpsx = gpsx;
        this.gpsy = gpsy;
        this.email = email;
        this.password = password;
    }
    
     /** Construtor com uma Entidade*/
    public Entidade(Entidade e){
        this.cod = e.getCod();
        this.nome = e.getNome();
        this.gpsx = e.getX();
        this.gpsy = e.getY();
        this.email = e.getEmail();
        this.password = e.getPassword();
    }
    
     /** 
      * Metodo que retorna o codigo de uma entidade
     * 
     * @return Codigo de uma entidade
     */
    public String getCod(){
        return this.cod;
    }

     /**
     * Metodo que altera o código de uma Entidade
     * 
     * @param codigo valor no qual o código de uma Entidade vai ser alterado
     */
    public void setCod(String codigo){
        this.cod = codigo;
    }
    
     /** 
      * Metodo que retorna o nome de uma entidade
     * 
     * @return Nome de uma entidade
     */
    public String getNome(){
        return this.nome;
    }

     /**
     * Metodo que altera o nome de uma entidade
     * 
     * @param nome valor no qual o nome de uma entidade vai ser alterado
     */
    public void setNome(String nome){
        this.nome = nome;
    }
    
     public double getX(){
        return this.gpsx;
    }

    public double getY(){
        return this.gpsy;
    }
    
    public void setX(double x){
        this.gpsx = x;
    }

    public void setY(double y){
        this.gpsy = y;
    }

    public void setLocation(double x, double y){
        setX(x);
        setY(y);
    }
    
    /**
     * Metodo que retorna o email de uma Entidade
     * 
     * @return email da entidade
     */
    public String getEmail(){
        return email;
    }
    
    /**
     * Metodo que altera o email de uma Entidade
     * 
     * @param email valor no qual o email vai ser alterado
     */
    public void setEmail(String email){
        this.email = email;
    }
    
    /**
     * Metodo que retorna a password de uma Entidade
     * 
     * @return Password da entidade
     */
    public String getPassword(){
        return password;
    }
    
    /**
     * Metodo que altera a password de uma Entidade
     * 
     * @param password valor no qual a password vai ser alterada
     */
    public void setPassword(String password){
        this.password = password;
    }
    
    /** 
     * Metodo que "transforma" uma Entidade numa String
     * 
     * @return Uma String da Entidade
     */
    public String toString(){
        String s = "Código: " + cod + 
        "\nNome: " + nome + 
        "\nCoordenadas: " + gpsx + ", " + gpsy +
        "\nEmail: " + email +
        "\nPassword: " + password;
        
        return s;
    }
    
    /** 
     * Metodo que verifica se um Objeto é igual a uma Entidade
     * 
     * @param o Objeto que irá ser comparado com a entidade
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
        
        Entidade e = (Entidade) o;
        
        if(this.cod.equals(e.getCod()) && this.nome.equals(e.getNome()) && this.gpsx == e.getX() && this.gpsy == e.getY()
        && this.email.equals(e.getEmail()) && this.password.equals(e.getPassword())) 
        return true;
        
        return b;
    }
    /**
     * Método que verifica se uma entidade acertou a password e dá-lhe acesso
     * 
     * @param password Password que a entidade introduziu
     * 
     * @return true caso comparaçao acerte a password, false caso contrário
     */
    public boolean login(String password){
        return this.password.equals(password);
    }
}
