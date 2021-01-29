import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

public class Loja extends Utilizador implements Serializable {
    
    private String codLoja;
    private Map<String,LinhaEncomenda> produtos;
    private List<Encomenda> listaEspera;
    private List<Encomenda> listaAceite;

    public Loja() {
        super();
        this.codLoja = new String();
        this.produtos = new HashMap<>();
        this.listaEspera = new ArrayList<>();
        this.listaAceite = new ArrayList<>();
    }

    public Loja(String codLoja, String nome, String email, String password, double posX, double posY, Map<String,LinhaEncomenda> listaProdutos,
                    List<Encomenda> listaEspera, List<Encomenda> encomendasAceites) {
        super(nome, email, password, posX, posY);
        this.codLoja = codLoja;
        this.setProdutos(listaProdutos);
        this.setListaEspera(listaEspera);
        this.setListaAceite(encomendasAceites);
    }

    public Loja(Loja l) {
        super(l);
        this.codLoja = l.getCodLoja();
        this.setProdutos(l.getProdutos());
        this.setListaEspera(l.getListaEspera());
        this.setListaAceite(l.getListaAceite());
    }

    public String getCodLoja() {
        return this.codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }
    
    public Map<String,LinhaEncomenda> getProdutos(){
        Map<String,LinhaEncomenda> res = new HashMap<>();
        for(Map.Entry<String,LinhaEncomenda> entry : this.produtos.entrySet()){
            res.put(entry.getKey(), entry.getValue().clone());
        }
        return res;
    }
    
    public void setProdutos(Map<String,LinhaEncomenda> produtos){
        this.produtos = new HashMap<>();
        for(Map.Entry<String,LinhaEncomenda> entry : produtos.entrySet()){
            this.produtos.put(entry.getKey(), entry.getValue());
        }
    }
    
    public List<Encomenda> getListaEspera() {
        List<Encomenda> aux = new ArrayList<>(this.listaEspera.size());
        for (Encomenda s : this.listaEspera) {
            aux.add(s.clone());
        }
        return aux;
    }

    public void setListaEspera(List<Encomenda> enc) {
        this.listaEspera = new ArrayList<>(enc.size());
        enc.forEach(e -> this.listaEspera.add(e));
    }

    public List<Encomenda> getListaAceite() {
        List<Encomenda> aux = new ArrayList<>(this.listaAceite.size());
        for (Encomenda s : this.listaAceite) {
            aux.add(s);
        }
        return aux;
    }

    public void setListaAceite(List<Encomenda> enc) {
        this.listaAceite = new ArrayList<>(enc.size());
        enc.forEach(e -> this.listaAceite.add(e));
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append(super.toString());
        s.append("\tCodigo Loja: ");
        s.append(this.codLoja);
        s.append("\n\tLista de produtos:");
        s.append(this.produtos);
        s.append("\n\tLista de Espera: ");
        s.append(this.listaEspera.toString());
        s.append("\n\tLista de encomendas aceites: ");
        s.append(this.listaAceite.toString());
        return s.toString();
    }

    public boolean equals(Object obj) {

        if (this == obj)
            return true;
        if ((obj == null) || (this.getClass() != obj.getClass()))
            return false;
        Loja loja = (Loja) obj;
        return super.equals(loja) && 
                this.codLoja.equals(loja.getCodLoja()) &&
                this.produtos.equals(loja.getProdutos()) &&
                this.listaEspera.equals(loja.getListaEspera()) &&
                this.listaAceite.equals(loja.getListaAceite());
    }

    public Loja clone() {
        return new Loja(this);
    }

    
    public Encomenda validarEncomenda() throws NaoExisteException {
        if(this.listaEspera != null && !this.listaEspera.isEmpty()) {
            Encomenda encomenda = this.listaEspera.get(0);
            this.listaAceite.add(encomenda.clone());
            this.listaEspera.remove(0);
            
            return encomenda;
        }
        else {
            throw new NaoExisteException(Constantes.NAO_EXISTEM_ENCOMENDAS);
        }
    }

    public int quantidadeListaEspera() {
        return this.listaEspera.size();
    }

    public int quantidadeListaAceite() {
        return this.listaAceite.size();
    }
    
    public int getNumTotalEncomendas() {
        return this.quantidadeListaAceite() + this.quantidadeListaEspera();
    }

    public void adicionaListaEspera(Encomenda encomenda) throws QuantidadeExcedidaException, NaoExisteException {
        List<LinhaEncomenda> encomendas = encomenda.getLinhasEncomendas();
        Map<String,LinhaEncomenda> novosProdutos = this.getProdutos();
        for(LinhaEncomenda le : encomendas){
            String codProduto = le.getCodProduto();
            if(novosProdutos.containsKey(codProduto)){
                LinhaEncomenda produtoLoja = novosProdutos.get(codProduto);
                double quantidadeT = produtoLoja.getQuantidade() - le.getQuantidade();
                if(quantidadeT < 0){
                    throw new QuantidadeExcedidaException();
                }
                produtoLoja.setQuantidade(quantidadeT);
            }
            else{ 
                throw new NaoExisteException(Constantes.PRODUTO_NAO_EXISTE + codProduto);
            }
        }
        this.setProdutos(novosProdutos);
        this.listaEspera.add(encomenda.clone());
    }

    public void registarProduto(LinhaEncomenda le){ // caso o produto exista altera para os novos dados, se nao existir acrescenta
        this.produtos.put(le.getCodProduto(), le.clone());
    }

    public LinhaEncomenda getProduto(String codProduto){
        LinhaEncomenda le = null;
        if(this.produtos.containsKey(codProduto)){
            le = this.produtos.get(codProduto).clone();
        }
        return le;
    }
    
    public boolean containsProduto(String codProduto){
        return this.produtos.containsKey(codProduto);
    }
}
