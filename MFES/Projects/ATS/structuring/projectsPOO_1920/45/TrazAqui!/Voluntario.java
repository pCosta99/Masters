import java.io.Serializable;
import java.util.*;

public class Voluntario implements Serializable {
    /*variáveis de instância */
    private String codVol;
    private String nome;
    private double gpsX;
    private double gpsY;
    private double raio;
    private String email;
    private String password;
    private int disp; // 0 - Indisponivel; 1 - Disponivel; 2 - Disponivel para encomendas medicas;
    private List <Encomenda> encomendas;
    
    public Voluntario(){
        codVol=null;
        nome=null;
        gpsX=0.0;
        gpsY=0.0;
        raio=0.0;
        email = null;
        password = null;
        disp = 1;
        encomendas = new ArrayList<>();
    }

    public Voluntario(String codVol, String nome, double gpsX, double gpsY, Double raio, String email, String password, int disp, List<Encomenda> encomendas){
        this.codVol=codVol;
        this.nome=nome;
        this.gpsX=gpsX;
        this.gpsY=gpsY;
        this.raio=raio;
        this.email = email;
        this.password = password;
        this.disp = disp;
        this.encomendas = encomendas;
    }

    public Voluntario(Voluntario v){
        this.codVol = v.getCodVol();
        this.nome = v.getNome();
        this.gpsX = v.getGPSX();
        this.gpsY = v.getGPSY();
        this.raio = v.getRaio();
        this.email = v.getEmail();
        this.password = v.getPassword();
        this.disp = v.getDisp();
        List <Encomenda> encomendasClone = new ArrayList<>();
        for(Encomenda e : v.encomendas){
            encomendasClone.add(e.clone());
        }
        this.encomendas = encomendasClone;
    }

    public String getCodVol() {
        return codVol;
    }

    public String getNome() {
        return nome;
    }

    public double getGPSX() {
        return gpsX;
    }

    public double getGPSY() {
        return gpsY;
    }

    public double getRaio() {
        return raio;
    }
    
    public String getEmail(){
        return email;
    }
    
    public String getPassword(){
        return password;
    }
    
    public int getDisp(){
        return disp;
    }
    
    public List<Encomenda> getEncomendasEstado(int s){
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda a : this.encomendas)
            if(a.getEstado() == s)
                aux.add(a.clone());
        return aux;
    }
    
    public List<Encomenda> getEncomendas(){
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda a : this.encomendas)
            aux.add(a.clone());
        return aux;
    }

    public void setCodVol(String codVol) {
        this.codVol = codVol;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setGPS(double gpsX, double gpsY) {
        this.gpsX = gpsX;
        this.gpsY = gpsY;
    }

    public void setGPSX(double gpsX) {
        this.gpsX = gpsX;
    }

    public void setGPSY(double gpsY) {
        this.gpsY = gpsY;
    }

    public void setRaio(Double raio) {
        this.raio = raio;
    }
    
    public void setEmail(String email) {
        this.email = email;
    }

    public void setPassword(String password) {
        this.password = password;
    }
    
    public void setDisp(int disp){
        this.disp = disp;
    }
    
    public void setEncomendas(List<Encomenda> al){
        this.encomendas = new ArrayList<>();
        for(Encomenda a : al)
            this.encomendas.add(a.clone());
    }
    
    public void addEncomenda(Encomenda e){
        this.encomendas.add(e.clone());
    }
    
    public Voluntario clone(){
        return new Voluntario(this);
    }

    public String toString(){
        String aux = "Email: " + this.email + ";\n"
                   + "Nome: " + this.nome + ";\n"
                   + "Password: " + this.password + ";\n"
                   + "Código: " + this.codVol +";\n"
                   + "Coordenadas: " + this.gpsX + "," + this.gpsY + ";\n"
                   + "Raio: " + this.raio + ";\n";
        if(this.disp == 0) aux = aux + "Indisponivel;\n";
        if(this.disp == 1) aux = aux + "Disponivel;\n";
        if(this.disp == 2) aux = aux + "Disponivel para encomendas medicas;\n";
        aux += "Encomendas: \n";
        for(Encomenda a : this.encomendas)
            aux += "-" + a.getCodEnc().toString() + "\n";
        return aux;
    }

    public boolean equals (Object obj) {
        if (this == obj)
            return true;
        else if ((obj == null) || (this.getClass() != obj.getClass()))
            return false;
        else {
            Voluntario ft = (Voluntario) obj;
            return codVol == ft.getCodVol() 
            && nome == ft.getNome()
            && gpsX == ft.getGPSX()
            && gpsY == ft.getGPSY()
            && raio == ft.getRaio() 
            && email == ft.getEmail()
            && password == ft.getPassword()
            && disp == ft.getDisp()
            && encomendas== ft.getEncomendas();
        }
    }

    public int hashCode() {
        final int primo = 31;
        int result = 1;
        result = primo * result + ((codVol == null) ? 0 : codVol.hashCode());
        result = primo * result + ((nome == null) ? 0 : nome.hashCode());
        result = primo * result + (int )gpsX + (int)gpsY;
        long aux = Double.doubleToLongBits(raio);
        result = primo * result + (int)(aux ^ (aux >>> 32));
        result = primo * result + ((email == null) ? 0 : email.hashCode());
        result = primo * result + ((password == null) ? 0 : password.hashCode());
        result = primo * result + disp;
        for(Encomenda e : this.encomendas)
            result = primo * result + e.hashCode();
        return result;
    }

}







