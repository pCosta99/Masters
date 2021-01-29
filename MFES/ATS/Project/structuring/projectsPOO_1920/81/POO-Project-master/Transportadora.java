
import java.time.LocalDate;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
/**
 * Write a description of class empresa here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Transportadora extends Dados implements Serializable{
    private String codEmpresa;
    private String email;
    private String password;
    private String nome;
    private double gpsx;
    private double gpsy;
    private int nif;
    private double raio;
    private double classificacao;
    private double totalFaturado;
    private double precoKm;
    private double precoAdicional;
    private List <Entregas> entregas;
    private List <Encomenda> encomendas;
    
    public Transportadora(){
        this.codEmpresa = new String();
        this.nome = new String();
        this.email = new String();
        this.password = new String();
        this.gpsx = 0;
        this.gpsy = 0;
        this.nif = 0;
        this.raio= 0;
        this.classificacao = 0;
        this.totalFaturado = 0;
        this.precoKm = 0;
        this.precoAdicional = 0;
        this.entregas = new ArrayList<>();
        this.encomendas = new ArrayList<>();
    }
    
    public Transportadora(String cod, String em, String nm, String pw, double x, double y,
                            int nif,double r ,double cla,double tf,double p, double pa, ArrayList<Entregas> en, ArrayList<Encomenda> e){
        this.setCod(cod);
        this.setEmail(em);
        this.setNome(nm);
        this.setPassword(pw);
        this.setGPSx(x);
        this.setGPSy(y);
        this.setNif(nif);
        this.setRaio(r);
        this.setClassificacaoTrans(cla);
        this.setTotalFaturado(tf);
        this.setPrecoKm(p);
        this.setPrecoAdicional(pa);
        this.setEntregas(en);
        this.setEncomendas(e);
    }
    
    public Transportadora(Transportadora t){
        this.codEmpresa = t.getCod();
        this.email = t.getEmail();
        this.nome = t.getNome();
        this.password = t.getPassword();
        this.gpsx = t.getGPSx();
        this.gpsy = t.getGPSy();
        this.nif = t.getNif();
        this.raio = t.getRaio();
        this.classificacao = t.getClassificacaoTrans();
        this.totalFaturado = t.getTotalFaturado();
        this.precoKm = t.getPrecoKm();
        this.precoAdicional = t.getPrecoAdicional();
        this.entregas = t.getEntregas();
        this.encomendas = t.getEncomendas();
    }
    
    public String getCod(){ return this.codEmpresa;}
    public String getEmail(){return this.email;}
    public String getNome(){return this.nome;}
    public String getPassword(){return this.password;}
    public double getGPSx(){ return this.gpsx;}
    public double getGPSy(){return this.gpsy;}
    public int getNif(){return this.nif;}
    public double getRaio(){return this.raio;}
    public double getClassificacaoTrans(){return this.classificacao;}
    public double getTotalFaturado(){return this.totalFaturado;}
    public double getPrecoKm(){return this.precoKm;}
    public double getPrecoAdicional(){return this.precoAdicional;}
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
   
    public void setCod(String c){this.codEmpresa = c;}
    public void setEmail(String em){this.email = em;}
    public void setNome(String n){this.nome= n;}
    public void setPassword(String pd){this.password = pd;}
    public void setGPSx(double x){this.gpsx = x;}
    public void setGPSy(double y){this.gpsy = y;}
    public void setNif(int n){this.nif = n;}
    public void setRaio(double r){this.raio = r;}
    public void setClassificacaoTrans(double cla){this.classificacao = cla;}
    public void setTotalFaturado(double tf){this.totalFaturado = tf;}
    public void setPrecoKm(double p ){this.precoKm = p;}
    public void setPrecoAdicional(double pa ){this.precoAdicional = pa;}
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
     */
    public void alteraClassificacaoTrans(double cla){
        double classi = this.classificacao * this.encomendas.size();
        classi = (classi + cla)/(this.encomendas.size() + 1);
        setClassificacaoTrans(classi);
    }
    
    public Transportadora clone(){
        return new Transportadora(this);
    }
    
     public boolean equals(Object o){
        if(o == this)
            return true;
        if(o == null || o.getClass() != this.getClass())
            return false;
        else{
            Transportadora t = (Transportadora) o;
            return(t.getCod().equals(this.getCod())
                   && t.getEmail().equals(this.getEmail())
                   && t.getNome().equals(this.getNome())
                   && t.getPassword().equals(this.getPassword())
                   && t.getGPSx() == this.getGPSx()
                   && t.getGPSy() == this.getGPSy()
                   && t.getNif() == this.getNif())
                   && t.getRaio() == this.getRaio()
                   && t.getTotalFaturado() == this.getTotalFaturado()
                   && t.getClassificacaoTrans() == this.getClassificacaoTrans()  
                   && t.getPrecoKm() == this.getPrecoKm()
                   && t.getEntregas().equals(this.getEntregas())
                   && t.getEncomendas().equals(this.getEncomendas());
        }
    }
    
    public String toString(){
        String aux = "Codigo da Empresa: " + this.codEmpresa + ";\n" 
                     + "Email da Empresa:" + this.email + ";\n"
                     + "Nome da Empresa: " + this.nome + ";\n"
                     + "Password:" + this.password + ";\n"
                     + "Destino X " + this.gpsx + ";\n"
                     + "Destino Y: " + this.gpsy + ";\n"
                     + "NIF: " +  this.nif + ";\n"
                     + "Raio: " + this.raio + ";\n"
                     + "Faturação Total da Entrega: " + this.totalFaturado + ";\n"
                     + "Classificação da Transportadora: " + this.classificacao + ";\n"
                     + "Custo por Km: " + this.precoKm + ";\n"
                     + "Entregas: \n";
        for(Entregas a : this.entregas)
            aux += a.toString() + "\n";
        aux += "Encomendas: \n";
        for(Encomenda a : this.encomendas)
            aux += a.toString() + "\n";
        return aux;
    }
   
    public String toString_top10_distancia(){
        double km = 0.0;
        for(Entregas e : this.entregas){
            km += e.getKm();
        }
        String aux = "A Empresa transportadora " + this.codEmpresa + " percorreu " + km + " km\n";
        return aux;
    
    }
    
    public int hashCode(){
        int hash = 7; 
        hash = 31 * hash + codEmpresa.hashCode();
        hash = 31 * hash + nome.hashCode();
        long aux = Double.doubleToLongBits(gpsx);
        hash = 31 * hash + (int)(aux ^ (aux >>> 32));
        long aux1 = Double.doubleToLongBits(gpsy);
        hash = 31 * hash + (int)(aux1 ^ (aux1 >>> 32));
        long aux2 = Double.doubleToLongBits(nif);
        hash = 31 * hash + (int)(aux2 ^ (aux2 >>> 32));
        long aux3 = Double.doubleToLongBits(raio);
        hash = 31 * hash + (int)(aux3 ^ (aux3 >>> 32));
        long aux4 = Double.doubleToLongBits(totalFaturado);
        hash = 31 * hash + (int)(aux4 ^ (aux4 >>> 32));
        long aux5 = Double.doubleToLongBits(classificacao);
        hash = 31 * hash + (int)(aux5 ^ (aux5 >>> 32));
        long aux6 = Double.doubleToLongBits(precoKm);
        hash = 31 * hash + (int)(aux6 ^ (aux6 >>> 32));
        for(Entregas a : this.entregas)
            hash = 31 * hash + a.hashCode();
        return hash;
    }
    
    public boolean dadosValidosEmpresa(String cod, String pass){
        return(this.codEmpresa.equals(cod) && this.password.equals(pass));
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
            if(e.getEstadoEntrega() == 1)
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
    public double tempoViagem(double x, double y, double xDestino, double yDestino){
        double distancia = this.dist(x, y, xDestino, yDestino);
        return distancia/(80/3600);
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
    
    public double custoViagem(double x, double y, double xD, double yD){
        if(dist(x,y,xD,yD) > 60) return this.precoKm * dist(x,y,xD,yD) + this.precoAdicional * (dist(x,y,xD,yD)-60);
        else return this.precoKm * dist(x,y,xD,yD);
    }
}
