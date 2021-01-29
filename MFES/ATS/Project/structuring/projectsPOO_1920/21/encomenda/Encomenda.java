import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.Iterator;

public class Encomenda {
    private String codcliente;
    private String codenc;
    private String codloja;
    private Double peso;
    private List<LinhaEncomenda> linhas;
    
    public Encomenda( String codenc,String codcliente,String codloja, double peso, List<LinhaEncomenda> linhasEnc) {
        this.codcliente = codcliente;
        this.codenc = codenc;
        this.codloja= codloja;
        this.peso = peso;
        setEncomendas(linhasEnc);
    }
    
    public Encomenda() {
        this.linhas = new ArrayList<>();
        this.codcliente = "n/a";
        this.codenc = "n/a";
        this.codloja="n/a";
        this.peso = 0.0;
    }
    
    public Encomenda(Encomenda enc) {
        this.linhas = enc.getLinhas();
        this.codcliente = enc.getcodcliente();
        this.codenc = enc.getcodenc();
        this.codloja=enc.getcodloja();
        this.peso = enc.getpeso();
        //this.data = LocalDate.now();
    }
    
    public String getcodcliente() {
        return this.codcliente;
    }
    
    public String getcodloja() {
        return this.codloja;
    }
    
    public String getcodenc() {
        return this.codenc;
    }
    
    public double getpeso() {
        return this.peso;
    }    
    
    public void setEncomendas(List<LinhaEncomenda> linhasEnc) {
        this.linhas = new ArrayList<>();
        for(LinhaEncomenda le : linhasEnc) {
            this.linhas.add(le.clone());
        }
    }
    
    
    public List<LinhaEncomenda> getLinhas() {
      return this.linhas.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());        
    }
    
    
    public Encomenda clone() {
        return new Encomenda(this);
    }
    
    public boolean equals(Object o) {
        if(o==this) return true;
        if(o==null || o.getClass() != this.getClass()) return false;
        Encomenda e = (Encomenda)o;
        return codcliente.equals(e.getcodcliente()) && codloja.equals(e.getcodloja()) &&
               codenc.equals(e.getcodenc()) && peso == e.getpeso() &&
               this.linhas.equals(e.getLinhas()); 
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("# ENCOMENDA #");
        sb.append("peso: ").append(this.peso);
        sb.append("codcliente: ").append(this.codcliente);
        sb.append("codloja: ").append(this.codloja);
        sb.append("codenc: ").append(this.codenc);
        for (LinhaEncomenda le: this.linhas){
            System.out.println("----//----//----");
            sb.append(le.toString());
        }
        return sb.toString();
    }
    
    
    /**
     * B)
     */
    public double calculaValorTotal() {
        double r = 0;
        for(LinhaEncomenda le : this.linhas) {
            r += le.calculaValorLinhaEnc();
        }
        return r;
    }
    
    
    

    
    /**
     * D)
     */
    public double numeroTotalProdutos() {
        double r = 0;
        for(LinhaEncomenda le : this.linhas) {
            r += le.getQuantidade();
        }
        return r;
    }

    /**
     * E)
     */
    public boolean existeNaEncomenda(String codProd) {
      return this.linhas.stream().anyMatch(e -> (e.getReferencia()).equals(codProd));
    }
    
    
    /**
     * F)
     */
    public void adicionaLinha(LinhaEncomenda linha) {
        this.linhas.add(linha.clone());
    }

    /**
     * G)
     */
    
    public void removeProduto(String codProd){
      for(Iterator<LinhaEncomenda> it = this.linhas.iterator() ; it.hasNext();){
          LinhaEncomenda le = it.next();
          if(le.getReferencia().equals(codProd)){
              it.remove();
          }
      }
    }

    //calcula o número de produtos encomendados nesta encomenda
    public double numProdutos() {
     
      return this.linhas.stream().mapToDouble(LinhaEncomenda::getQuantidade).sum();
    }    
    
    
    
    
    
    //calcula a lista dos produtos de uma encomenda
    public List<String> getListaProdutos() {
      return this.linhas.stream().map(LinhaEncomenda::getReferencia).distinct().collect(Collectors.toList());    
    }
}
