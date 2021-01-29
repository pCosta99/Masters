import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class Encomenda implements Serializable{
    private String codigo;
    private final String codigo_user;
    private final String codigo_loja;
    private final double peso;
    private final String comprador;
    private final String vendedor;
    private final LocalDateTime data;
    private boolean preparada;
    private boolean entregue;
    private boolean levantada;
    private Map<String, LinhaEncomenda> produtos;
    private boolean encomendaMedica;


    public Encomenda(){
        this.codigo = " ";
        this.codigo_user = " ";
        this.codigo_loja = " ";
        this.peso = 0.0;
        this.comprador = " ";
        this.vendedor = " ";
        this.entregue = false;
        this.levantada = false;
        this.preparada = false;
        this.produtos = new HashMap<>();
        this.data = LocalDateTime.now();
        this.encomendaMedica = true;
    }

    public  Encomenda(String codigo, String codigo_user, String codigo_loja, double peso, String comprador, String vendedor, Map<String, LinhaEncomenda> produtos,
                      boolean encomendaMedica, LocalDateTime data, boolean entregue, boolean levantada, boolean preparada){
        this.codigo = codigo;
        this.codigo_user = codigo_user;
        this.codigo_loja = codigo_loja;
        this.peso = peso;
        this.comprador = comprador;
        this.vendedor = vendedor;
        this.data = data;
        this.entregue = entregue;
        this.levantada = levantada;
        this.preparada = preparada;
        this.setProdutos(produtos);
        this.encomendaMedica = encomendaMedica;
    }

    public  Encomenda(Encomenda e){
        this.codigo = e.getCodigo();
        this.codigo_user = e.getCodigo_user();
        this.codigo_loja = e.getCodigo_loja();
        this.peso = e.getPeso();
        this.comprador = e.getComprador();
        this.vendedor =  e.getVendedor();
        this.setProdutos(e.getProdutos());
        this.data = e.getData();
        this.encomendaMedica = e.isEncomendaMedica();
        this.levantada = e.isLevantada();
        this.entregue = e.isEntregue();
        this.preparada = e.isPreparada();
    }

    public LocalDateTime getData() {
        return data;
    }

    public boolean isEntregue() {
        return entregue;
    }

    public boolean isLevantada() {
        return levantada;
    }

    public boolean isPreparada() {
        return preparada;
    }

    public String getCodigo(){
      return this.codigo;
    }

    public String getCodigo_user(){
      return this.codigo_user;
    }

    public String getCodigo_loja(){
      return this.codigo_loja;
    }

    public double getPeso() {
        return this.peso;
    }

    public String getComprador() {
        return this.comprador;
    }

    public String getVendedor() {
        return this.vendedor;
    }

    public void setPreparada(boolean preparada) {
        this.preparada = preparada;
    }

    public Map<String, LinhaEncomenda> getProdutos(){
        return this.produtos.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().clone()));
    }

    public void setLevantada(boolean levantada) {
        this.levantada = levantada;
    }

    public void setEntregue(boolean entregue) {
        this.entregue = entregue;
    }

    public boolean isEncomendaMedica() {
        return this.encomendaMedica;
    }

    public void setCodigo(String codigo){
      this.codigo = codigo;
    }

    public void setProdutos(Map<String, LinhaEncomenda> produtos) {
        this.produtos = new HashMap<>();
        produtos.forEach((key, value) -> this.produtos.put(key, value.clone()));
    }

    public void setEncomendaMedica(boolean encomendaMedica) {
        this.encomendaMedica = encomendaMedica;
    }

    public Encomenda clone(){
        return new Encomenda(this);
    }

    public boolean equals(Object obj){
        if(obj == this) return true;
        if(obj == null || obj.getClass() != this.getClass()) return false;
        Encomenda e = (Encomenda) obj;
        return  this.codigo.equals(e.getCodigo()) &&
                this.codigo_user.equals(e.getCodigo_user()) &&
                this.codigo_loja.equals(e.getCodigo_loja()) &&
                e.getPeso() == this.getPeso() &&
                this.comprador.equals(e.getComprador()) &&
                this.vendedor.equals(e.getVendedor()) &&
                this.produtos.equals(e.getProdutos()) &&
                this.encomendaMedica == e.isEncomendaMedica();
    }

    public String toString(){
        return "\n" + "Encomenda: " + "\n" +
                "Código: " +
                this.codigo + "\n" +
                "Código do utilizador: " +
                this.codigo_user + "\n" +
                "Código da loja: " +
                this.codigo_loja + "\n" +
                "Peso: " +
                this.peso + "\n" +
                "Comprador: " +
                this.comprador + "\n" +
                "Vendedor: " +
                this.vendedor + "\n" +
                "Data de emissão da encomenda: " +
                this.data + "\n" +
                "Produtos: " + "\n" +
                this.produtos + "\n";
    }



}
