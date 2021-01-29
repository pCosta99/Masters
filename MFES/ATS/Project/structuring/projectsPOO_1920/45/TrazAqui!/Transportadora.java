import java.util.*;
import java.io.Serializable;

public class Transportadora implements Serializable{
    /*Variaveis de instância*/
    private String codTran;
    private String nome;
    private double gpsX;
    private double gpsY;
    private long nif;
    private double raio;
    private double precoKM;
    private String email;
    private String password;
    private int disp; // 0 - Indisponivel; 1 - Disponivel; 2 - Disponivel para encomendas medicas;
    private List <Encomenda> encomendas;
    
    /*Construtores */
    public Transportadora(){
        codTran = null;
        nome = null;
        gpsX = 0.0;
        gpsY = 0.0;
        nif = 0;
        raio = 0.0;
        precoKM = 0.0;
        email = null;
        password = null;
        disp = 1;
        encomendas = new ArrayList<>();
    }

    public Transportadora(String codTran, String nome, double gpsX, double gpsY, long nif, double raio, double precoKM, String email, String password, int disp, List<Encomenda> encomendas){
        this.codTran = codTran;
        this.nome = nome;
        this.gpsX = gpsX;
        this.gpsY = gpsY;
        this.nif = nif;
        this.raio = raio;
        this.precoKM = precoKM;
        this.email = email;
        this.password = password;
        this.disp = disp;
        this.encomendas = encomendas;
    }

    public Transportadora(Transportadora t){
        this.codTran = t.getCodTran();
        this.nome = t.getNome();
        this.gpsX = t.getGPSX();
        this.gpsY = t.getGPSY();
        this.nif = t.getNIF();
        this.raio = t.getRaio();
        this.precoKM = t.getPrecoKM();
        this.raio=t.getRaio();
        this.email =t.getEmail();
        this.password = t.getPassword();
        this.disp = t.getDisp();
        List <Encomenda> encomendasClone = new ArrayList<>();
        for(Encomenda e : t.encomendas){
            encomendasClone.add(e.clone());
        }
        this.encomendas = encomendasClone;
    }

    /*Metodos de instância*/

    public String getCodTran() {
        return codTran;
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


    public long getNIF() {
        return nif;
    }
    
    public int getDisp() {
        return disp;
    }

    public double getRaio() {
        return raio;
    }

    public double getPrecoKM() {
        return precoKM;
    }
    
    public String getEmail(){
        return email;
    }
    
    public String getPassword(){
        return password;
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

    public void setCodTran(String codTran) {
        this.codTran = codTran;
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

    public void setNIF(long nif) {
        this.nif = nif;
    }

    public void setDisp(int disp) {
        this.disp = disp;
    }
    
    public void setRaio(double raio) {
        this.raio = raio;
    }

    public void setPrecoKM(double precoKM) {
        this.precoKM = precoKM;
    }
    
    public void setEmail(String email) {
        this.email = email;
    }

    public void setPassword(String password) {
        this.password = password;
    }
    
    public void setEncomendas(List<Encomenda> al){
        this.encomendas = new ArrayList<>();
        for(Encomenda a : al)
            this.encomendas.add(a.clone());
    }

    public Transportadora clone(){
        return new Transportadora(this);
    }

    public String toString(){
        StringBuilder aux = new StringBuilder("Email: " + this.email + ";\n"
                   + "Nome: " + this.nome + ";\n"
                   + "Password: " + this.password + ";\n"
                   + "Codigo: " + this.codTran +";\n"
                   + "Nif: " + this.nif +";\n"
                   + "Coordenadas: " + this.gpsX + "," + this.gpsY + ";\n"
                   + "Raio: " + this.raio +";\n"
                   + "Preço/KM: " + this.precoKM +";\n");
        if(this.disp == 0) aux.append("Indisponivel;\n");
        if(this.disp == 1) aux.append("Disponivel;\n");
        if(this.disp == 2) aux.append("Disponivel para encomendas medicas;\n");
        
        aux.append("Encomendas: \n");
        for(Encomenda a : this.encomendas)
            switch(a.getEstado()){
                case 0: aux.append("-").append(a.getCodEnc()).append(": Terminada").append("\n");
                    break;
                case 1: aux.append("-").append(a.getCodEnc()).append(": Espera sinalizacao da loja").append("\n");
                    break;
                case 2: aux.append("-").append(a.getCodEnc()).append(": Espera em loja").append("\n");
                    break;
                case 3: aux.append("-").append(a.getCodEnc()).append(": Espera Resposta do Utilizador(Transportadora)").append("\n");
                    break;
            }
                                    
        return aux.toString();
    }

    public boolean equals(Object obj){
        if(this == obj)
            return true;
        else if((obj == null) || (this.getClass() != obj.getClass()))
            return false;
        else{
            Transportadora ft = (Transportadora) obj;
            return codTran == ft.getCodTran() && nome == ft.getNome() && gpsX == ft.getGPSX() && gpsY == ft.getGPSY() &&
                    nif == ft.getNIF() && raio == ft.getRaio() && precoKM == ft.getPrecoKM() &&
                    email == ft.getEmail() && password == ft.getPassword() && encomendas == ft.getEncomendas();
        }
    }
    
    public int hashCode() {
        final int primo = 31;
        int result = 1;
        result = primo * result + ((codTran == null) ? 0 : codTran.hashCode());
        result = primo * result + ((nome == null) ? 0 : nome.hashCode());
        result = primo * result + (int) gpsX + (int) gpsY;
        result = primo * result + (int)nif;
        long aux = Double.doubleToLongBits(raio);
        result = primo * result + (int)(aux ^ (aux >>> 32));
        long aux1 = Double.doubleToLongBits(precoKM);
        result = primo * result + (int)(aux1 ^ (aux1 >>> 32));
        result = primo * result + ((email == null) ? 0 : email.hashCode());
        result = primo * result + ((password == null) ? 0 : password.hashCode());
        for(Encomenda e : this.encomendas)
            result = primo * result + e.hashCode();
        return result;
    }

    public void addEncomenda(Encomenda e){
        this.encomendas.add(e.clone());
    }




}
