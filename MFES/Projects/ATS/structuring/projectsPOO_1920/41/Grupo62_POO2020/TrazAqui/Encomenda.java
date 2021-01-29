import java.util.stream.Collectors;
import java.util.*;
import java.io.*;
import java.time.LocalDate;

public class Encomenda implements Serializable
{
    /** Código da Encomenda */
    private String codEnc;
    /** Código do Utilizador que fez a encomenda */
    private String codUser;
    /** Código da Loja que enviou a encomenda */
    private String codLoja;
    /** Código de quem entregou a encomenda */
    private String codEnt;
    /** Peso da Encomenda */
    private double peso;
    /** Linha de Encomenda */
    private List<LinhaEncomenda> lEnc;
    /** Nº de produtos na Linha de Encomenda */
    private int nProd;
    /** Data da Encomenda */
    private LocalDate data;
    /** Estado da Encomenda */
    private String estado;
    
    public Encomenda(){
        this.codEnc = "";
        this.codUser = "";
        this.codLoja = "";
        this.codEnt = "";
        this.peso = 0.0;
        this.lEnc = new ArrayList<>();
        this.nProd = 0;
        this.data = LocalDate.now();
        this.estado = "";
    }
    
    public Encomenda(String ce, String cu, String cl, String ct, double peso, LocalDate data, List<LinhaEncomenda> lEnc, int nProd, String estado){
        this.codEnc = ce;
        this.codUser = cu;
        this.codLoja = cl;
        this.codEnc = ct;
        this.peso = peso;
        this.data = data;
        setLEnc(lEnc);
        this.nProd = nProd;
        this.estado = estado;
    }
    
    public Encomenda(Encomenda e){
        this.codEnc = e.getCodEnc();
        this.codUser = e.getCodUser();
        this.codLoja = e.getCodLoja();
        this.codEnt = e.getCodEnt();
        this.peso = e.getPeso();
        this.lEnc = e.getLEnc();
        this.data = e.getData();
        this.nProd = e.getNProd();
        this.estado = e.getEstado();
    }
    
    public String getCodEnc(){
        return this.codEnc;
    }

    public void setCodEnc(String ce){
        this.codEnc = ce;
    }
    
    public String getCodUser(){
        return this.codUser;
    }

    public void setCodUser(String cu){
        this.codUser = cu;
    }
    
    public String getCodLoja(){
        return this.codLoja;
    }

    public void setCodLoja(String cl){
        this.codLoja = cl;
    }
    
    public String getCodEnt(){
        return this.codEnt;
    }

    public void setCodEnt(String ct){
        this.codEnt = ct;
    }
    
    public double getPeso(){
        return this.peso;
    }

    public void setPeso(double peso){
        this.peso = peso;
    }
    
    public List<LinhaEncomenda> getLEnc(){
      return this.lEnc.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());        
    }

    public void setLEnc(List<LinhaEncomenda> linhasEnc){
        this.lEnc = new ArrayList<>();
        for(LinhaEncomenda le : linhasEnc){
            this.lEnc.add(le.clone());
        }
    }
    
    public int getNProd(){
        return this.nProd;
    }

    public void setNProd(int nProd){
        this.nProd = nProd;
    }
    
    public LocalDate getData(){
        return this.data;
    }
    
    public void setData(LocalDate d){
        this.data = d;
    }
    
    public String getEstado(){
        return this.estado;
    }

    public void setEstado(String est){
        this.estado = est;
    }
    
    public Encomenda clone(){
        return new Encomenda(this);
    }
    
    public String toString(){
        String s = "Código da Encomenda: " + codEnc + 
        "\nCódigo do Utilizador: " + codUser + 
        "\nCódigo da Loja: " + codLoja +
        "\nCódigo do Voluntário/Empresa: " + codEnt + 
        "\nPeso da Encomenda: " + peso +
        "\nLinha de Encomenda" + lEnc +
        "\nEstado" + estado;
        
        return s;
    }
    
    public boolean equals(Object o){
        
        boolean b = false;
        if(this == o){
            return true;
        }
        
        if(o == null || this.getClass() != o.getClass()){
            return false;
        }
        
        Encomenda e = (Encomenda) o;
        
        if(this.codEnc.equals(e.getCodEnc()) && this.codUser.equals(e.getCodUser()) && this.codLoja.equals(e.getCodLoja()) &&
        this.codEnt.equals(e.getCodEnt()) && this.peso == e.getPeso() && this.lEnc.equals(e.getPeso()) 
        && this.lEnc.equals(e.getData()) && this.nProd == e.getNProd() && this.estado.equals(e.getEstado()))
        return true;
        
        return b;
    }
    
    /** 
     * Metodo que insere uma Linha de Encomenda numa lista
     * 
     * @param le LinhaEncomenda que vai ser inserida
     */
    public void insereLinhaEncomenda(LinhaEncomenda le){
        lEnc.add(le);
    }
}
