import java.time.LocalDate;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
/**
 * Write a description of class voluntario here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Voluntarios extends Dados implements Serializable{
    private String codVoluntario;
    private String email;
    private String password;
    private String nome;
    private double gpsx;
    private double gpsy;
    private int nif;
    private double raio;
    private List <Entregas> entregas;
    private List <Encomenda> encomendas;
    
    public Voluntarios(){
        this.codVoluntario = new String();
        this.nome = new String();
        this.email = new String();
        this.password = new String();
        this.gpsx = 0;
        this.gpsy = 0;
        this.nif = 0;
        this.raio= 0;
        this.entregas= new ArrayList<>();
        this.encomendas = new ArrayList<>();
    }
    
    public Voluntarios(String cod, String em, String nm, String pw, double x, double y,int nif,double r, ArrayList<Entregas> en, ArrayList<Encomenda> e ){
        this.setCod(cod);
        this.setEmail(em);
        this.setNome(nm);
        this.setPassword(pw);
        this.setGPSx(x);
        this.setGPSy(y);
        this.setNif(nif);
        this.setRaio(r);
        this.setEntregas(en);
        this.setEncomendas(e);
    }
    
    public Voluntarios(Voluntarios v){
        this.codVoluntario = v.getCod();
        this.email = v.getEmail();
        this.nome = v.getNome();
        this.password = v.getPassword();
        this.gpsx = v.getGPSx();
        this.gpsy = v.getGPSy();
        this.nif = v.getNif();
        this.raio = v.getRaio();
        this.entregas = v.getEntregas();
        this.encomendas = v.getEncomendas();
    }
    
    public String getCod(){ return this.codVoluntario;}
    public String getEmail(){return this.email;}
    public String getNome(){return this.nome;}
    public String getPassword(){return this.password;}
    public double getGPSx(){ return this.gpsx;}
    public double getGPSy(){return this.gpsy;}
    public int getNif(){return this.nif;}
    public double getRaio(){return this.raio;}
    public List<Entregas> getEntregas(){
        List<Entregas> aux = new ArrayList<>();
        for(Entregas a : this.entregas)
            aux.add(a.clone());
        return aux;
    }
    public List<Encomenda> getEncomendas(){
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda a : this.encomendas)
            aux.add(a.clone());
        return aux;
    }
    
    public Voluntarios clone(){
       return new Voluntarios(this);
    }
    
    public void setCod(String c){this.codVoluntario = c;}
    public void setEmail(String em){this.email = em;}
    public void setNome(String n){this.nome= n;}
    public void setPassword(String pd){this.password = pd;}
    public void setGPSx(double x){this.gpsx = x;}
    public void setGPSy(double y){this.gpsy = y;}
    public void setNif(int n){this.nif = n;}
    public void setRaio(double r){this.raio = r;}
    public void setEntregas(List<Entregas> en){
        this.entregas = new ArrayList<>();
        for(Entregas a :en)
            this.entregas.add(a.clone());
    }
    public void setEncomendas(List<Encomenda> en){
        this.encomendas = new ArrayList<>();
        for(Encomenda a :en)
            this.encomendas.add(a.clone());
    }
    
    /**
     * Metodo que altera a classificacao
     
    public void alteraClassificacaoVol(double cla){
        double classi = this.classificacao * this.encomendas.size();
        classi = (classi + cla)/(this.encomendas.size() + 1);
        setClassificacaoVol(classi);
    }
    */
    
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o == null || o.getClass() != this.getClass())
            return false;
        else{
            Voluntarios v = (Voluntarios) o;
            return v.getCod().equals(this.getCod()) && 
                   v.getEmail().equals(this.getEmail()) && 
                   v.getNome().equals(this.getNome()) && 
                   v.getPassword().equals(this.getPassword()) && 
                   v.getNif() == this.getNif() && 
                   v.getGPSx() == this.getGPSx() && 
                   v.getGPSy() == this.getGPSy() &&
                   v.getRaio() == this.getRaio() &&
                   v.getEntregas().equals(this.getEntregas()) && 
                   v.getEncomendas().equals(this.getEncomendas());
                   
        }
    }
    public String toString(){
        String aux = "Codigo do Voluntario: " + this.codVoluntario + ";\n" 
                     + "Email: " + this.email + ";\n" 
                     + "Nome: " + this.nome + ";\n"
                     + "Password: " + this.password + ";\n"
                     + "Nif: " + this.nif + ";\n"
                     + "Coordenada X: " + this.gpsx + ";\n"
                     + "Coordenada Y: " + this.gpsy + ";\n"
                     + "Raio:" + this.raio + ";\n" 
                     + "Entregas: \n";
        for(Entregas a : this.entregas)
            aux += a.toString() + "\n";
        aux += "Encomendas: \n";
        for(Encomenda a : this.encomendas)
            aux += a.toString() + "\n";
        return aux;
    }
    
    /**
     * Metodo que verifica se o email e a password dados sao os do voluntario
     * 
     * @param  ema   o email a comparar
     * @param  pass   a password a comparar
     * 
     * @return  o resultado da comparacao
     */
    public boolean dadosValidosVoluntario(String ema, String pass){
        return(this.email.equals(ema) && this.password.equals(pass));
    }
     
    public boolean aceitoTransporteMedicamentos(){
        for(Encomenda e : this.encomendas){
            if(e.getTipo() == "m")
                return true;
        }
        return false;
    }
    
    public boolean dispostoFazerEntrega(){
        for (Entregas e : this.entregas){
            if(e.getEstadoEntrega() == 2)
                return true;
        }
        return false;
    }

    public void entregasEfetuadas(Entregas e, ArrayList<Entregas> list){
        e.setEstadoEntrega(1);
        list.add(e);
    }
    
    public Encomenda escolhaEncomenda(){
        Encomenda b = null;
        for(Encomenda e : this.encomendas){
            if(e.getEstado() == 0  && e.getTipo() != "m") 
                b = e;
        }        
        return b;
    }
    
    public Encomenda escolhaEncomenda3(){
        Encomenda b = null;
        for(Encomenda e : this.encomendas){
            if(e.getEstado() == 0  && e.getTipo() == "m") 
                b = e;
        }        
        return b;
    }
    
    /**
     * Metodo que devolve o tempo resultante do deslocamento entre dois pontos
     * 
     * @param  x a coordenada x atual
     * @param  y a coordenada x atual
     * @param  xDestino a coordenada x do destino
     * @param  yDestino a coordenada y do destino
     * @param  velocidade do voluntario
     * 
     * @return tempo do deslocamento entre dois pontos
     */
    public double tempoEntrega (double x, double y, double xDestino, double yDestino){
        double distancia = this.dist(x, y, xDestino, yDestino);
        return distancia/(5/3600);
    }
    
    /**
     * Metodo que devolve a distancia entre dois pontos 
     * 
     * @param  x1 a coordenada x atual
     * @param  y1 a coordenada y atual
     * @param  x2 a coordenada x destino
     * @param  y2 a coordenada y destino
     * 
     * @return dist percorrida de um ponto ao outro
     */
    public double dist(double x1, double y1, double x2, double y2) {       
        return Math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1));
    }  
}
