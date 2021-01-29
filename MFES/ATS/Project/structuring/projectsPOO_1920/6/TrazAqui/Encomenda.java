    
/**
 * Classe das Encomendas
 * 
 * @author (Jo„o Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */

import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
public class Encomenda implements Serializable
{
    // vari·veis de inst‚ncia da classe Encomenda
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private List<LinhaEncomenda> le;
    private double classificacao;
    private double custo;
    
    //construtor por omiss„o
    public Encomenda(){
        this.codEncomenda = "n/a";
        this.codUtilizador = "n/a";
        this.codLoja = "n/a";
        this.peso = 0.0;
        this.le = new ArrayList<>();
        this.classificacao = 0.0;
        this.custo = 0.0;
    }
    
    //construtor parametrizado
    public Encomenda(String codE, String codU, String codL, double peso, List<LinhaEncomenda> le){
        this.codEncomenda = codE;
        this.codUtilizador = codU;
        this.codLoja = codL;
        this.peso = peso;
        this.setLe(le);
    }
    
    //construtor de copia
    public Encomenda(Encomenda e){
        this.codEncomenda = e.getCodEnc();
        this.codUtilizador = e.getCodU();
        this.codLoja = e.getCodL();
        this.peso = e.getPeso();
        this.le = e.getLe();
    }
    
    //metodo que devolve o codigo de uma encomenda
    public String getCodEnc(){
        return this.codEncomenda;
    }
    
    // metodo que devolve o codigo de um utilizador
    public String getCodU(){
        return this.codUtilizador;
    }
    
    //metodo que devolve o codigo de uma loja
    public String getCodL(){
        return this.codLoja;
    }
    
    //metodo que devolve o peso de uma encomenda
    public double getPeso(){
        return this.peso;
    }
    
    // metodo que devolve uma lista de linhas de encomenda
    public List<LinhaEncomenda> getLe(){
        List<LinhaEncomenda> res = new ArrayList<>();
        for (LinhaEncomenda l: this.le){
            res.add(l.clone());
        }
        return res;
    }

    public double getCusto(){
        return this.custo;
    }
    
    public double getClassificacao(){
        return this.classificacao;
    }
    
    // metodo para definir o codigo de uma empresa transportadora
    public void setCodEnc(String codEnc){
        this.codEncomenda = codEnc;
    }
    
    // metodo para definir o codigo de um utilizador
    public void setCodU(String codU){
        this.codUtilizador = codU;
    }
    
    // metodo para definir o codigo de uma loja
    public void setCodL(String codL){
        this.codLoja = codL;
    }
    
    // metodo para definir o peso de uma encomenda
    public void setPeso(double peso){
        this.peso = peso;
    }
    
    // metodo para definir a lista de linhas de encomenda
    public void setLe(List<LinhaEncomenda> les){
        this.le = new ArrayList<>();
        for (LinhaEncomenda l : les){
            this.le.add(l.clone());
        }
    }
    
    public void setClassificacao(double classificacao){
        this.classificacao = classificacao;
    }

    public void setCusto(double custo){
        this.custo = custo;
    }
    
    // metodo para adicionar uma linha de encomenda a uma lista de linhas de encomenda
    public void addLinhaEncomenda(LinhaEncomenda l){
        this.le.add(l.clone());
    }
    
    // metodo que coloca toda a informa√ß√£o sobre uma Encomenda numa string
    public String toString(){
        StringBuffer sb = new StringBuffer();
        sb.append("Encomenda:"+this.codEncomenda+","+this.codUtilizador+","+this.codLoja+","+this.peso+",");
        for (LinhaEncomenda l: le){
            sb.append(l.clone()+",");
        }
        sb.deleteCharAt(sb.lastIndexOf(","));
        return sb.toString();
    }
    
    // metodo de copia de uma encomenda
    public Encomenda clone(){
        return new Encomenda(this);
    }
    
    // metodo que compara se duas Encomenda sao iguais
    public boolean equals(Object o){
        if (o==this) return true;
        if ((o.getClass()!=this.getClass())||o==null) return false;
        Encomenda e = (Encomenda) o;
        return this.codEncomenda.equals(e.getCodEnc()) &&
               this.codUtilizador.equals(e.getCodU()) &&
               this.codLoja.equals(e.getCodL()) &&
               this.peso == e.getPeso() &&
               this.le.equals(e.getLe());
    }
}
