package Model;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

public class Encomenda implements Serializable {

    private String codigo;
    private String codigoUser;
    private String codigoLoja;
    private String codigoTrans;
    private double peso;
    private boolean medical;
    private boolean aceitePorLoja;
    private boolean entregue;
    private LocalDate data;
    private double tempoEntrega; //dado em minutos
    private List<LinhaEncomenda> lista;

    /* Class Constructors*/
    public Encomenda(){
        this.codigo = "";
        this.codigoUser = "";
        this.codigoLoja = "";
        this.codigoTrans = "";
        this.peso = 0.0;
        this.medical = false;
        this.aceitePorLoja = false;
        this.entregue = false;
        data = null;
        this.tempoEntrega = 0.0;
        lista = new ArrayList<>();
    }
    public Encomenda(String c, String cu, String cl, String ct, double p, boolean m, boolean apl, boolean e, LocalDate data, double tde, List<LinhaEncomenda> lista){
        setCodigo(c);
        setCodigoUser(cu);
        setCodigoLoja(cl);
        setCodigoTrans(ct);
        setPeso(p);
        setMedical(m);
        setAceitePorLoja(apl);
        setEntregue(e);
        this.data = data;
        setTempoEntrega(tde);
        setLista(lista);
    }
    public Encomenda(Encomenda e){
        this.codigo = e.getCodigo();
        this.codigoUser = e.getCodigoUser();
        this.codigoLoja = e.getCodigoLoja();
        this.codigoTrans = e.getCodigoTrans();
        this.peso = e.getPeso();
        this.medical = e.isMedical();
        this.aceitePorLoja = e.isAceitePorLoja();
        this.entregue = e.isEntregue();
        this.data = e.getData();
        this.tempoEntrega = e.getTempoEntrega();
        this.lista = e.getLista();
    }

    /* Getters and Setters*/
    public String getCodigoTrans() {
        return codigoTrans;
    }
    public void setCodigoTrans(String codigoTrans) {
        this.codigoTrans = codigoTrans;
    }

    public String getCodigo() {
        return codigo;
    }
    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public String getCodigoUser() {
        return codigoUser;
    }
    public void setCodigoUser(String codigoUser) {
        this.codigoUser = codigoUser;
    }

    public String getCodigoLoja() {
        return codigoLoja;
    }
    public void setCodigoLoja(String codigoLoja) {
        this.codigoLoja = codigoLoja;
    }

    public double getPeso() {
        return peso;
    }
    public void setPeso(double peso) {
        this.peso = peso;
    }

    public boolean isMedical() {
        return medical;
    }
    public void setMedical(boolean medical) {
        this.medical = medical;
    }

    public boolean isAceitePorLoja() {
        return aceitePorLoja;
    }
    public void setAceitePorLoja(boolean aceitePorLoja) {
        this.aceitePorLoja = aceitePorLoja;
    }

    public boolean isEntregue() {
        return entregue;
    }
    public void setEntregue(boolean entregue) {
        this.entregue = entregue;
    }

    public LocalDate getData()
    {
        return data;
    }
    public void setData(LocalDate data)
    {
        this.data = data;
    }
    public void setData()
    {
        this.data = LocalDate.now();
    }

    public double getTempoEntrega() {
        return tempoEntrega;
    }
    public void setTempoEntrega(double tempoEntrega) {
        this.tempoEntrega = tempoEntrega;
    }

    public List<LinhaEncomenda> getLista(){
        List<LinhaEncomenda> res = new ArrayList<>(this.lista.size());
        for(LinhaEncomenda linha: lista){
            res.add(linha.clone());
        }
        
        return res;
    }
    public void setLista(List<LinhaEncomenda> lista) {
        this.lista = new ArrayList<>(lista.size());
        for(LinhaEncomenda linha: lista){
            this.lista.add(linha.clone());
        }
    }

    /** Adds a package line to the package
     *
     * @param linha - new package line
     */
    public void adicionaLinha(LinhaEncomenda linha)
    {
        this.lista.add(linha);
    }
    public void removeProduto(String codProd){
        lista.removeIf(p -> p.getReferencia().equals(codProd));
    }
    public boolean isEmpty(){
        return this.lista.size() == 0;
    }

    /** Checks if a product exists on a Package.
     *
     * @param refProduto - product
     * @return - true/false depending on the existence of the product.
     */
    public boolean existeProdutoEncomenda(String refProduto){
        return this.lista.stream().anyMatch(p -> p.getReferencia().equals(refProduto));
    }

    @Override
    public Encomenda clone(){
        return new Encomenda(this);
    }
    @Override
    public String toString() {
        return "Encomenda{" +
                "código = " + this.codigo + " | " +
                "codigoUser = " + this.codigoUser + " | " +
                "codigoLoja = " + this.codigoLoja + " | " +
                "codigoTrans = " + this.codigoTrans + " | " +
                "peso = " + this.peso + " | " +
                "medical = " + this.medical + " | " +
                "aceitePorLoja = " + this.aceitePorLoja + " | " +
                "entregue = " + this.entregue + " | " +
                "data = " + this.data + " | " +
                "lista = {" + this.lista +
                '}';
    }

    /** Calculates the price of the package as a whole by adding the price of every line of the package.
     *
     * @return - final price
     */
    public double calculaValorTotal(){
        int i;
        double valor = 0;
        for(i=0;i < lista.size();i++){
            valor += lista.get(i).calculaValorLinhaEnc();
        }
        return valor;
    }

    /** Measures the amount of products on a certain package.
     *
     * @return - amount of products.
     */
    public int numeroTotalProdutos() {
        int i;
        int quantidade = 0;
        for(i=0;i<lista.size();i++)
        {
            quantidade += lista.get(i).getQuantidade();
        }
        return quantidade;
    }

    /** Calculates the total weight of the package by adding the weight of all products.
     *
     * @return the weight of the package.
     */
    public double somaPeso(){
        double res = 0;
        for(LinhaEncomenda le : this.lista){
            res += le.getQuantidade();
        }
        return res;
    }

    /** Given a package, checks if it's code follows the package's code rules.
     *
     * @param e - package that we're validating.
     * @return - true it the code is valid, false otherwise
     */
    public static boolean validate(Encomenda e){
        String code = e.getCodigo();
        boolean valid = true;
        for(int i = 1; i < code.length() && valid; i++){
            if(!(Character.isDigit(code.charAt(i)))){
                valid = false;
            }
        }
        return (code.charAt(0) == 'e' && valid);
    }

    public Encomenda addEncomenda(Encomenda enc){
        Encomenda newEnc = new Encomenda(this);
        //Adicionar as linhas de encomenda
        int i = 0;
        List<LinhaEncomenda> newList = new ArrayList<>(enc.getLista().size() + this.getLista().size());
        for(LinhaEncomenda le : this.getLista()){
            newList.add(i,le);
            i++;
        }
        for(LinhaEncomenda le: enc.getLista()){
            newList.add(i,le);
            i++;
        }
        //Adicionar o peso de todas
        newEnc.somaPeso();
        return newEnc;
    }

    /** Changes the stat of the package from unaccepted to accepted by the store.
     *
     */
    public void lojaAceita(){
        setAceitePorLoja(true);
    }
}
