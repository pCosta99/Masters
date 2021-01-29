import java.util.ArrayList;

public class Cliente extends Utilizador {
    private String codigo;
    private int ndeEnc;
    private ArrayList<Encomendas> encomendas;
    private ArrayList<Encomendas> transporte;
    private ArrayList<String> classificaVoluntarios;

    public Cliente(String nome, GPS localizacao, String email, String password, String codigo, int ndeEnc, ArrayList<Encomendas> encomendas, ArrayList<Encomendas> p, ArrayList<String> v) {
        super(nome, localizacao, email, password);
        this.codigo = codigo;
        this.ndeEnc = ndeEnc;
        this.encomendas = encomendas;
        this.transporte = p;
        this.classificaVoluntarios = v;
    }

    public Cliente(){
        super();
        this.codigo = "n/a";
        this.ndeEnc = 0;
        this.encomendas = new ArrayList<>();
        this.transporte = new ArrayList<>();
        this.classificaVoluntarios = new ArrayList<>();
    }

    public Cliente(Cliente c) {
        super(c);
        this.codigo = c.getCodigo();
        this.ndeEnc = c.getNdeEnc();
        this.encomendas = c.getEncomendas();
        this.transporte = c.getTransporte();
        this.classificaVoluntarios = c.getClassificaVoluntarios();
    }

    public String getCodigo() {
        return this.codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public int getNdeEnc() {
        return this.ndeEnc;
    }

    public int setNdeEnc(int ndeEnc) {
        this.ndeEnc = ndeEnc;
        return ndeEnc;
    }

    public ArrayList<Encomendas> getEncomendas(){
        return this.encomendas;
    }

    public void setEncomendas(ArrayList<Encomendas> e){
        this.encomendas = e;
    }

    public void addEncomenda(Encomendas e){
        this.encomendas.add(e);
    }

    public ArrayList<Encomendas> getTransporte(){
        return this.transporte;
    }

    public void setTransporte(ArrayList<Encomendas> p){
        this.transporte = p;
    }

    public void addTransporte(Encomendas e){
        this.transporte.add(e);
    }

    public ArrayList<String> getClassificaVoluntarios(){
        return this.classificaVoluntarios;
    }

    public void setClassificaVoluntarios(ArrayList<String> v){
        this.classificaVoluntarios = v;
    }

    public void addCVol(String cod){
        this.classificaVoluntarios.add(cod);
    }

    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        Utilizador u = (Utilizador) obj;
        return false;
    }

    public int numerodeEnc(){
        return this.setNdeEnc(getNdeEnc() + 1);
    }

    public Cliente clone() {
        return new Cliente (this);
    }
    
    public String toString(){
        return this.getNome() + " com " + this.ndeEnc + " encomendas";
    }
}
