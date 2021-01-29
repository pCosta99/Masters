import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Classe que implementa um Encomenda para efeitos de log in
 *
 * @author monteiro06
 * @version 20200406
 */
public class Encomenda  implements Serializable {
    private String codigo;
    private String destinatario;
    private String estabelecimento;
    private String estafeta;
    private double valor;
    private double peso;
    private List<Produto> produtos;
    private boolean encomendaMedica;
    private int estado; //0 - Pedido | 1 - Preparar | 2 - Pronto | 3 - Transporte | 4 - Entregue
    private GPS localizacao;
    private int classificacao;
    private LocalDateTime pedida;
    private LocalDateTime despachada;
    private LocalDateTime entregue;

    public Encomenda() {
        this.codigo = "n/a";
        this.destinatario = "n/a";
        this.estabelecimento = "n/a";
        this.estafeta = "n/a";
        this.valor = 0;
        this.peso = 0;
        this.produtos = new ArrayList<>();
        this.encomendaMedica = false;
        this.estado = 0;
        this.localizacao = new GPS();
        this.classificacao = 0;
        this.pedida = LocalDateTime.now();
        this.despachada = LocalDateTime.now();
        this.entregue = LocalDateTime.now();
    }

    public Encomenda(String umCodigo, String umDestinatario, String umEstabelecimento, String umEstafeta, double umValor, double umPeso, List<Produto> unsProdutos, boolean umaEncomendaMedica, int umEstado, int umaClassificacao, LocalDateTime umaDataPedida, LocalDateTime umaDataDespachada, LocalDateTime umaDataEntrega) {
        this.codigo = umCodigo;
        this.destinatario = umDestinatario;
        this.estabelecimento = umEstabelecimento;
        this.estafeta = umEstafeta;
        this.valor = umValor;
        this.peso = umPeso;
        this.produtos = unsProdutos;
        this.encomendaMedica = umaEncomendaMedica;
        this.estado = umEstado;
        this.classificacao = umaClassificacao;
        this.pedida = umaDataPedida;
        this.despachada = umaDataDespachada;
        this.entregue = umaDataEntrega;
    }

    /**
     * Método de criação de Emcomenda para os Logs fornecidos
     * @param umCodigo
     * @param umDestinatario
     * @param umaLoja
     * @param umPeso
     * @param unsProdutos
     */
    public Encomenda(String umCodigo, String umDestinatario, String umaLoja, double umPeso, List<Produto> unsProdutos){
        this.codigo = umCodigo;
        this.destinatario = umDestinatario;
        this.estabelecimento = umaLoja;
        this.estafeta = "n/a";
        this.valor = 0;
        this.peso = umPeso;
        this.produtos = unsProdutos;
        this.encomendaMedica = false;
        this.estado = 0;
        this.localizacao = new GPS();
        this.classificacao = 0;
        this.pedida = LocalDateTime.now();
        this.despachada = LocalDateTime.now();
        this.entregue = LocalDateTime.now();
    }

    public Encomenda(Encomenda novaEncomenda) {
        this.codigo = novaEncomenda.getCodigo();
        this.destinatario = novaEncomenda.getDestinatario();
        this.estabelecimento = novaEncomenda.getEstabelecimento();
        this.estafeta = novaEncomenda.getEstafeta();
        this.valor = novaEncomenda.getValor();
        this.peso = novaEncomenda.getPeso();
        this.produtos = novaEncomenda.getProdutos();
        this.encomendaMedica = novaEncomenda.isEncomendaMedica();
        this.estado = novaEncomenda.getEstado();
        this.localizacao = novaEncomenda.getLocalizacao();
        this.classificacao = novaEncomenda.getClassificacao();
        this.pedida = novaEncomenda.getPedida();
        this.despachada = novaEncomenda.getDespachada();
        this.entregue = novaEncomenda.getEntregue();
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }

    public String getCodigo() {
        return this.codigo;
    }

    public String getDestinatario() {
        return this.destinatario;
    }

    public String getEstabelecimento() {
        return this.estabelecimento;
    }

    public String getEstafeta(){
        return this.estafeta;
    }

    public double getValor(){
        return this.valor;
    }

    public double getPeso() {
        return this.peso;
    }

    public List<Produto> getProdutos() {
        return this.produtos;
    }

    public boolean isEncomendaMedica(){
        return  this.encomendaMedica;
    }

    public int getEstado(){
        return this.estado;
    }

    public GPS getLocalizacao(){
        return this.localizacao;
    }

    public int getClassificacao() {
        return this.classificacao;
    }

    public LocalDateTime getPedida() {
        return this.pedida;
    }

    public LocalDateTime getDespachada() {
        return this.despachada;
    }

    public LocalDateTime getEntregue() {
        return this.entregue;
    }

    /**
    public GPS getLocalizacao(){
        switch (this.estado){
            case 0:
                this.estabelecimento.getLocalizacao();
                break;
            case 1:
                this.estabelecimento.getLocalizacao();
                break;
            case 2:
                this.estabelecimento.getLocalizacao();
                break;
            case 3:
                this.estafeta.getLocalizacao();
                break;
            case 4:
                this.destinatario.getLocalizacao();
                break;
        }

        return this.localizacao;
    }
**/
    public void setCodigo(String novoCodigo) {
        this.codigo = novoCodigo;
    }

    public void setDestinatario(String novoDestinatario) {
        this.destinatario = novoDestinatario;
    }

    public void setEstabelecimento(String novoEstabelecimento) {
        this.estabelecimento = novoEstabelecimento;
    }

    public void setEstafeta(String novoEstafeta){
        this.estafeta = novoEstafeta;
    }

    public void setValor(double novoValor){
        this.valor = novoValor;
    }

    public void setPeso(double novoPeso) {
        this.peso = novoPeso;
    }

    public void setProdutos(List<Produto> novosProdutos) {
        this.produtos = novosProdutos;
    }

    public void setEncomendaMedica(boolean novaEncomendaMedica){
        this.encomendaMedica = novaEncomendaMedica;
    }

    public void setEstado(int novoEstado){
        this.estado = novoEstado;
    }

    public void setLocalizacao(GPS novaLocalizacao){
        this.localizacao = novaLocalizacao;
    }

    public void setClassificacao(int novaClassificacao) {
        this.classificacao = novaClassificacao;
    }

    public void setPedida(LocalDateTime novaDataPedida) {
        this.pedida = novaDataPedida;
    }

    public void setDespachada(LocalDateTime novaDataDespachada) {
        this.despachada = novaDataDespachada;
    }

    public void setEntregue(LocalDateTime novaDataEntregue) {
        this.entregue = novaDataEntregue;
    }


    /* ------------ Outros Métodos ------------ */

    public double calculaPeso() {
        double peso = 0;
        for (Produto p : produtos) {
            peso += p.getPeso() * p.getQuantidade();
        }
        return peso;
    }

    public boolean existeProduto(Produto umProduto) {
        for(Produto p : this.produtos) {
            if (p.getCodigo().equals(umProduto.getCodigo()))
                return true;
        }
        return false;
    }

    public void adicionaProduto(Produto umProduto){
        if(this.existeProduto(umProduto)) {
            Produto p = this.produtos.get(produtos.indexOf(umProduto));
            p.aumentaQuantidade(umProduto.getQuantidade());
            this.setPeso(this.peso + p.getPeso());
        }
        else{
            this.produtos.add(umProduto.clone());
            this.setPeso(umProduto.getPeso() * umProduto.getQuantidade());
            if(umProduto.isMedicamento()) this.setEncomendaMedica(true);
        }
    }

    public void removeProduto(Produto umProduto) throws ProdutoInexistenteException {
        if (!this.produtos.contains(umProduto))
            throw new ProdutoInexistenteException("Não existe um produto com o código " + umProduto.getCodigo() + " na encomenda!");
        else {
            this.produtos.remove(umProduto);
            if (umProduto.isMedicamento())
                if (!temProdutosMedicos())
                    this.setEncomendaMedica(false);
        }
    }

    public void alteraEstado(){
        switch (this.estado){
            case 0:
                this.estado++;
                break;
            case 1:
                this.estado++;
                break;
            case 2:
                this.estado++;
                break;
            case 3:
                this.estado++;
                break;
            case 4:
                break;
        }
    }

    public boolean temProdutosMedicos(){
        for(Produto p : this.produtos)
            if(p.isMedicamento())
                return true;
        return false;
    }


    public boolean equals(Object o){
        if(this == o)
            return true;
        if(o == null || this.getClass() != o.getClass())
            return false;

        Encomenda e = (Encomenda) o;
        return this.codigo.equals(e.getCodigo()) &&
                this.destinatario.equals(e.getDestinatario()) &&
                this.estabelecimento.equals((e.getEstabelecimento())) &&
                this.estafeta.equals(e.getEstafeta()) &&
                this.valor == e.getValor() &&
                this.peso == e.getPeso() &&
                this.produtos.equals(e.getProdutos()) &&
                this.encomendaMedica == e.isEncomendaMedica() &&
                this.estado == e.getEstado();
    }


    public String toString(){
        return "Encomenda:{" +
                "\n\t Código de Controlo = " + this.codigo +
                "\n\t Destinatário = " + this.destinatario +
                "\n\t Estabelecimento = " + this.estabelecimento +
                "\n\t Estafeta = " + this.estafeta +
                "\n\t Valor = " + this.valor +
                "\n\t Peso = " + this.peso +
                "\n\t Produtos = " + this.produtos +
                "\n\t Encomenda Médica = " + this.encomendaMedica +
                "\n\t Estado da Encomenda = " + this.estado +
                '\n' + '}';
    }

    public String toCSV(){
        return  this.codigo + ',' +
                this.destinatario + ',' +
                this.estabelecimento + ',' +
                this.peso + '[' + toLog(produtos) + ']';
    }

    public String toLog(){
        return this.codigo + ',' +
                this.destinatario + ',' +
                this.estabelecimento + ',' +
                this.peso + toLog(produtos);
    }

    public String toLog(List<Produto> unsProdutos){
        StringBuilder res = new StringBuilder();
        for(Produto p : unsProdutos)
            res.append(',').append(p.toLog());

        return res.toString();
    }
}
