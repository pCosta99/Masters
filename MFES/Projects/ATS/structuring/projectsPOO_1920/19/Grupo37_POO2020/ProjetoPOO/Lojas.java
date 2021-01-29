import java.util.ArrayList;
import java.util.stream.Collectors;
import static java.util.stream.Collectors.toList;
import java.util.*;

public class Lojas extends Utilizador {
    private String codigo;
    private double atendimentohabitual;
    private ArrayList<Produto> catalogo;
    private int fila;
    private ArrayList<Encomendas> encomenda;

    public Lojas() {
        super();
        this.codigo = "n/a";
        this.atendimentohabitual = 0.0;
        this.catalogo = new ArrayList<Produto>();
        this.fila = 0;
        this.encomenda = new ArrayList<Encomendas>(); //sou burro
    }

    public Lojas (Lojas j){
        super(j);
        this.codigo = j.getCodigo();
        this.atendimentohabitual = j.getAtendimentohabitual();
        this.catalogo = j.getCatalogo();
        this.fila = j.getFila();
        this.encomenda = j.getEncomenda();
    }

    public Lojas (String nome, GPS localizacao, String email, String password, String codigo, double atendimentohabitual, ArrayList<Produto> catalogo, int fila, ArrayList<Encomendas> encomenda){
        super(nome,localizacao,email ,password);
        this.codigo = codigo;
        this.atendimentohabitual = atendimentohabitual;
        this.catalogo = catalogo;
        this.fila = fila;
        this.encomenda = encomenda;
    }

    public String getCodigo() {
        return this.codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public double getAtendimentohabitual() {
        return this.atendimentohabitual;
    }

    public void setAtendimentohabitual(double atendimento) {
        this.atendimentohabitual = atendimento;
    }

    public ArrayList<Produto> getCatalogo(){
        return this.catalogo;
    }

    public void setCatalogo(ArrayList<Produto> enc){
        this.catalogo = enc;
    }

    public void addCatalogo(Produto p){
    	this.catalogo.add(p);
    }

    public void removeItem(String nome, int quant){
    	for (Produto p: this.catalogo){
    		if (p.getDescricao().equals(nome)){
    			p.setQuantidade(p.getQuantidade() - quant);
    			if (p.getQuantidade() == 0){
    				this.removeCat(p);
    			}
    			break;
    		}
    	}
    }

    public void removeCat(Produto p){
    	this.catalogo.remove(p);
    }

    public Produto getProduto(String nome){
    	for (Produto p: this.catalogo){
    		if (p.getDescricao().equals(nome)){
    			return p;
    		}
    	}
    	System.out.println("Produto inexistente");
    	return null;
    }

    public int getFila() {
        return this.fila;
    }

    public void setFila(int fila) {
        this.fila = fila;
    }

    public ArrayList<Encomendas> getEncomenda(){
    	return this.encomenda;
    }

    public void setEncomenda(ArrayList<Encomendas> enc){
    	this.encomenda = enc;
    }

    public void addEncomenda(Encomendas enc){
        this.encomenda.add(enc);
    }

    public void removeEncomenda(Encomendas enc){
        this.encomenda.remove(enc);
    }

    public boolean equals(Object l){
        if(this == l){
            return true;
        }
        if(l == null || this.getClass() != l.getClass()){
            return false;
        }
        Lojas loja = (Lojas) l;
        return (super.equals(l) && this.getCodigo().equals(loja.getCodigo()) && this.getAtendimentohabitual() == loja.getAtendimentohabitual() && this.getCatalogo() == loja.getCatalogo());//completar
    }

    public Lojas clone(){
        return new Lojas(this);
    }

    public void incFila(){
        setFila(this.fila + 1);
    }

    public void decFila(){
        setFila(this.fila - 1);
    }

    public String toString(){
    	return "Nome: " + this.getNome() + "\n" + "Catálogo: " + this.getCatalogo().toString();
    }

    public void getEnc(String cod){
    	for (Encomendas e: this.encomenda){
    		if (e.getCodEnc().equals(cod)){
    			e.setDisponivel(true);
    			break;
    		}
    	}
    }

    public List<Encomendas> getNaoAceite() {
        return this.encomenda.stream().filter(Encomendas::isDisponivel).collect(toList());
    }
}