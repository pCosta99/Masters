import java.util.*;
import java.util.stream.Collectors;
import static java.util.stream.Collectors.toList;
import java.util.InputMismatchException;
import java.io.*;


public class GerirTrazAqui implements Serializable{
    private Map<String, Cliente> cliente;
    private Map<String, Lojas> lojas;
    private Map<String, Voluntario> voluntario;
    private Map<String, Transportadora> empresas;
    private Comparator<Cliente> comparadores;
    private Comparator<Transportadora> comparadoresT;

    public GerirTrazAqui(Map<String, Cliente> cliente, Map<String, Lojas> lojas, Map<String, Voluntario> voluntario, Map<String, Transportadora> empresas) {
        this.cliente = cliente;
        this.lojas = lojas;
        this.voluntario = voluntario;
        this.empresas = empresas;
        this.comparadores = new Comparador();
        this.comparadoresT = new ComparadorT();
    }

    public GerirTrazAqui() {
        this.cliente = new HashMap<>();
        this.lojas = new HashMap<>();
        this.voluntario = new HashMap<>();
        this.empresas = new HashMap<>();
        this.comparadores = new Comparador();
        this.comparadoresT = new ComparadorT();
    }

    public GerirTrazAqui(GerirTrazAqui g) {
        this.cliente = g.getCliente();
        this.lojas = g.getLojas();
        this.voluntario = g.getVoluntario();
        this.empresas = g.getEmpresas();
        this.comparadores = new Comparador();
        this.comparadoresT = new ComparadorT();
    }

    public Map<String, Cliente> getCliente() {
        return this.cliente;
    }

    public void setCliente(Map<String, Cliente> cliente) {
        this.cliente = cliente;
    }

    public Map<String, Lojas> getLojas() {
        return this.lojas;
    }

    public void setLojas(Map<String, Lojas> lojas) {
        this.lojas = lojas;
    }

    public Map<String, Voluntario> getVoluntario() {
        return this.voluntario;
    }

    public void setVolutario(Map<String, Voluntario> voluntario) {
        this.voluntario = voluntario;
    }

    public Map<String, Transportadora> getEmpresas() {
        return this.empresas;
    }

    public void setEmpresas(Map<String, Transportadora> empresas) {
        this.empresas = empresas;
    }

    public Comparator<Cliente> getComparadores() {
        return this.comparadores;
    }

    public void setComparadores(Comparator<Cliente> comparadores) {
        this.comparadores = comparadores;
    }

    public Comparator<Transportadora> getComparadoresT() {
        return this.comparadoresT;
    }

    public void setComparadoresT(Comparator<Transportadora> comparadoresT) {
        this.comparadoresT = comparadoresT;
    }

    public Lojas getLoja(String nome){
    	for (Lojas l: this.lojas.values()){
    		if (l.getNome().equals(nome)){
    			return l;
    		}
    	}
    	System.out.println("Loja inexistente");
    	return null;
    }

    public Lojas getL(String cod){
    	for (Lojas l: this.lojas.values()){
    		if (l.getCodigo().equals(cod)){
    			return l;
    		}
    	}
    	return null;
    }
    
    public Cliente getUtil(String cod){
    	for (Cliente c: this.cliente.values()){
    		if (c.getCodigo().equals(cod)){
    			return c;
    		}
    	}
    	return null;
    }

    public Transportadora getEmp(String cod){
    	for (Transportadora t: this.empresas.values()){
    		if (t.getCodigoT().equals(cod)){
    			return t;
    		}
    	}
    	return null;
    }

    public Utilizador getUtilizador(String cod, String email) {
        switch (cod) {
            case "Utilizador":
                return this.cliente.get(email);
            case "Loja":
                return this.lojas.get(email);
            case "Voluntário":
                return this.voluntario.get(email);
            case "Empresa":
                return this.empresas.get(email);
        }
        return null;
    }

    public void addCliente(String email, String password, String nome, GPS gps) {
        Cliente c = new Cliente(nome, gps, email, password, 'u' + Integer.toString(this.cliente.size() + 1), 0, new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
        this.cliente.put(email, c);
    }

    public void addVoluntario(String email, String password, String nome, GPS gps, double raio, boolean meds, int vel) {
        Voluntario v = new Voluntario(nome, gps, email, password, raio, 'v' + Integer.toString(this.voluntario.size() + 1), true, meds, new ArrayList<>(), vel, null);
        this.voluntario.put(email, v);
    }

    public void addLoja(String email, String password, String nome, GPS gps, double espera) {
        Lojas l = new Lojas(nome, gps, email, password, 'l' + Integer.toString(this.lojas.size() + 1), espera, new ArrayList<>(), 0, new ArrayList<>());
        this.lojas.put(email, l);
    }

    public void addEmpresa(String email, String password, String nome, GPS gps, double raio, boolean meds, int aceite, float p, double t, int vel) {
        Transportadora e = new Transportadora(nome, gps, email, password, 0, raio, 'e' + Integer.toString(this.empresas.size() + 1), meds, aceite, p, t, vel, new ArrayList<>());
        this.empresas.put(email, e);
    }

    public boolean existeUtilizador(String u, String email) {
        switch (u) {
            case "Utilizador":
                return this.cliente.containsKey(email);
            case "Loja":
                return this.lojas.containsKey(email);
            case "Voluntário":
                return this.voluntario.containsKey(email);
            case "Empresa":
                return this.empresas.containsKey(email);
        }
        return false;
    }
    
    public void classificarVoluntario(String cod, int classificacao){
        if (this.voluntario.containsKey(cod))
            this.voluntario.get(cod).addClassificacao(classificacao);
    }

    public void aceitaEntT(Transportadora t, Encomendas e) {
        if (t.getAceite() > 0) {
            e.setCodBusca(t.getCodigoT());
            e.setDisponivel(false);
            t.setAceite(t.getAceite() - 1);
        }
    }

    public float kmsPercorridos(Lojas j, Cliente c, Transportadora t) {
        float r = (float) (t.distEntre(j.getLocalizacao()) + j.distEntre(c.getLocalizacao()));
        t.setKms(t.getKms() + r);
        return r;
    }

    public double estimativaV(Lojas j, Voluntario v) {
        return (j.getAtendimentohabitual() * j.getFila()) + j.getLocalizacao().distEntre(v) / v.getVelocidadeEst();
    }

    public double estimativaT(Lojas j, Transportadora t) {
        return (j.getAtendimentohabitual() * j.getFila()) + j.getLocalizacao().distEntre(t) / t.getVelocidadeEst();
    }

    public List<Cliente> ordenarCliente(Comparator<Cliente> c){
        return this.cliente.values().stream().map(Cliente::clone).sorted(c).collect(Collectors.toList());
    }

    public List<Transportadora> ordenarTransportadora(Comparator<Transportadora> c){
        return this.empresas.values().stream().map(Transportadora::clone).sorted(c).collect(Collectors.toList());
    }

    public void entregaV(Voluntario v){
        v.setEncomenda(null);
        v.setDisponivel(true);
    }

    public void entregaT(Encomendas e, Transportadora t){
        t.removeEncomenda(e);
        t.setAceite(t.getAceite() + 1);
    }
    
    public void gravarObj(String nFich) throws IOException{
        ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(nFich));

        oos.writeObject(this);

        oos.flush();
        oos.close();
    }
    
    public static GerirTrazAqui lerObj(String nFich) throws IOException, ClassNotFoundException {
        ObjectInputStream ois = new ObjectInputStream(new FileInputStream(nFich));

        GerirTrazAqui g = (GerirTrazAqui) ois.readObject();

        ois.close();
        return g;
    }
}