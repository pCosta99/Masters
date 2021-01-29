import java.io.Serializable;

/**
 * Classe que implementa um Usuário para efeitos de log in
 *
 * @author monteiro06
 * @version 20200407
 */
public class Produto  implements Serializable {

    //variáveis de instância
    private String codigo;
    private String descricao;
    private float quantidade;
    private double valor;
    private double peso;
    private boolean medicamento;

    /**
     * Construtores da classe Produto
     * Declaração dos construtores por omissão (vazio), parametrizado e de cópia.
     */
    public Produto(){
        this.codigo = "n/a";
        this.descricao = "n/a";
        this.quantidade = 0;
        this.valor = 0;
        this.peso = 0;
        this.medicamento = false;
    }

    public Produto(String umCodigo, String umDescricao, int umaQuantidade, double umValor, double umPeso, boolean isMedicamento){
        this.codigo = umCodigo;
        this.descricao = umDescricao;
        this.quantidade = umaQuantidade;
        this.valor = umValor;
        this.peso = umPeso;
        this.medicamento = isMedicamento;
    }



    /**
     * Método de criação de Produto para os Logs fornecidos
     * @param umCodigo
     * @param umaDescricao
     * @param umaQuantidade
     * @param umValor
     */
    public Produto(String umCodigo, String umaDescricao, float umaQuantidade, double umValor){
        this.codigo = umCodigo;
        this.descricao = umaDescricao;
        this.quantidade = umaQuantidade;
        this.valor = umValor;
        this.peso = 0;
        this.medicamento = false;
    }

    public Produto(Produto umProduto){
        this.codigo = umProduto.getCodigo();
        this.descricao = umProduto.getDescricao();
        this.quantidade = umProduto.getQuantidade();
        this.valor = umProduto.getValor();
        this.peso = umProduto.getPeso();
        this.medicamento = umProduto.isMedicamento();
    }

    public Produto clone(){
        return new Produto(this);
    }


    /**
     * Métodos de instância
     */

    public String getCodigo(){
        return this.codigo;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public float getQuantidade() {
        return this.quantidade;
    }

    public double getValor() {
        return this.valor;
    }

    public double getPeso() {
        return this.peso;
    }

    public boolean isMedicamento(){
        return this.medicamento;
    }

    public void setCodigo(String novoCodigo){
        this.codigo = novoCodigo;
    }

    public void setDescricao(String novaDescricao) {
        this.descricao = novaDescricao;
    }

    public void setQuantidade(float novaQuantidade) {
        this.quantidade = novaQuantidade;
    }

    public void setValor(double novoValor) {
        this.valor = novoValor;
    }

    public void setPeso(double novoPeso) {
        this.peso = novoPeso;
    }

    public void setMedicamento(boolean isMedicamento){
        this.medicamento = isMedicamento;
    }

    public boolean equals(Object o){
        if(this == o)
            return true;
        if(o == null || this.getClass() != o.getClass())
            return false;

        Produto p = (Produto) o;
        return this.codigo.equals(p.getCodigo()) &&
                this.descricao.equals(p.getDescricao()) &&
                this.quantidade == p.getQuantidade() &&
                this.valor == p.getValor() &&
                this.peso == p.getPeso() &&
                this.medicamento == p.isMedicamento();
    }

    public String toString() {
        return "Produto:{" +
                "\n\t Código de Controlo = " + this.codigo +
                "\n\t Descricao = " + this.descricao +
                "\n\t Quantidade = " + this.quantidade +
                "\n\t Preço = " + this.valor +
                "\n\t Peso = " + this.peso +
                "\n\t Medicamento = " + this.medicamento +
                '\n' + '}';
    }

    public String toCSV(){
        return this.codigo + ',' +
                this.descricao + ',' +
                this.quantidade + ',' +
                this.valor + ',' +
                this.peso + ',' +
                this.medicamento;
    }

    public String toLog(){
        return this.codigo + ',' + this.descricao + ',' + this.quantidade + ',' + this.valor;
    }


    public void aumentaQuantidade(float umaQuantidade){
        this.quantidade += umaQuantidade;
    }

}
