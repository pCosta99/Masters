import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.util.*;

public class Encomenda implements Serializable{
    /*Variaveis de instância*/
    private String codEncomenda;
    private String codUser;
    private String codLoja;
    private double peso;
    private int estado; // 0 - Terminada;
                        // 1 - Espera sinalizacao da loja;
                        // 2 - Espera em Loja;
                        // 3 - Espera Resposta do Utilizador(Transportadora);

    private List<LinhaEncomenda> produtos;
    private String data;
    private double precoEntrega;
    private boolean med;

    /*Construtores */
    public Encomenda(){
        codEncomenda = null;
        codUser = null;
        codLoja = null;
        peso = 0.0;
        estado = 1;
        produtos = new ArrayList<>();
        data = null;
        precoEntrega = 0.0;
        med = false;
    }

    public Encomenda(String codEncomenda, String codUser, String codLoja, double peso, int estado, ArrayList<LinhaEncomenda> produtos, String data, double precoEntrega, boolean med){
        this.codEncomenda = codEncomenda;
        this.codUser = codUser;
        this.codLoja = codLoja;
        this.peso = peso;
        this.estado = estado;
        this.produtos = produtos;
        this.data = data;
        this.precoEntrega = precoEntrega;
        this.med = med;
    }

    public Encomenda(Encomenda t){
        this.codEncomenda = t.getCodEnc();
        this.codUser = t.getCodUser();
        this.codLoja = t.getCodLoja();
        this.peso = t.getPeso();
        this.estado = t.getEstado();
        List<LinhaEncomenda> produtosClone = new ArrayList<>();
        for(LinhaEncomenda e : t.produtos){
            produtosClone.add(e.clone());
        }
        this.produtos = produtosClone;
        this.data = t.getData();
        this.precoEntrega = t.getPrecoEntrega();
        this.med = t.isMed();
    }

    /*Metodos de instância*/

    public String getCodEnc() {
        return codEncomenda;
    }

    public String getCodUser() {
        return codUser;
    }

    public String getCodLoja() {
        return codLoja;
    }

    public double getPeso() {
        return peso;
    }
    
    public int getEstado() {
        return estado;
    }

    public List<LinhaEncomenda> getProdutos() {
        return produtos;
    }

    public LinhaEncomenda getIndProdutos(int i){
        return produtos.get(i);
    }

    public String getData() {
        return data;
    }

    public String getDataPrint() {
        LocalDateTime ldt = LocalDateTime.parse(data);
        DateTimeFormatter formatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.SHORT, FormatStyle.SHORT);
        return ldt.format(formatter);
    }

    public double getPrecoEntrega() {
        return precoEntrega;
    }

    public boolean isMed(){
        return med;
    }

    public void setCodEnc(String codEncomenda){
        this.codEncomenda = codEncomenda;
    }

    public void setCodUser(String codUser){
        this.codUser = codUser;
    }

    public void setCodLoja(String codLoja){
        this.codLoja = codLoja;
    }

    public void setPeso(double peso){
        this.peso = peso;
    }
    
    public void setEstado(int estado){
        this.estado = estado;
    }

    public void setProdutos(ArrayList<LinhaEncomenda> ps){
        this.produtos = new ArrayList<>();
        for(LinhaEncomenda le : ps)
            this.produtos.add(le.clone());
    }

    public void setData(String data){
        this.data = data;
    }

    public void setPrecoEntrega(double precoEntrega){
        this.precoEntrega = precoEntrega;
    }

    public void setIndProdutos(int i, LinhaEncomenda l){
        produtos.set(i, l);
    }

    public void setMed(Boolean b){
        this.med = b;
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }

    public String toString() {
        StringBuilder s = new StringBuilder();
        if(med) s.append("Encomenda medica:");
        else s.append("Encomenda:");
        s.append(codEncomenda);
        s.append(",");
        s.append(codUser);
        s.append(",");
        s.append(codLoja);
        s.append(",");
        s.append(peso);
        s.append(",");
        if(estado == 0) s.append("Terminada");
        if(estado == 1) s.append("Espera confirmacao da loja");
        if(estado == 2) s.append("Espera na loja");
        if(estado == 3) s.append("Espera Resposta do Utilizador(Transportadora);");
        s.append(data);
        s.append(",");
        s.append(precoEntrega);
        s.append(",");
        for(LinhaEncomenda produto : produtos) {
            s.append(",");
            s.append(produto.toString());
        }
        return s.toString();
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        else if ((obj == null) || (this.getClass() != obj.getClass()))
            return false;
        else {
            Encomenda ft = (Encomenda) obj;
            return codEncomenda.equals(ft.getCodEnc())
            && codUser.equals(ft.getCodUser())
            && codLoja.equals(ft.getCodLoja())
            && peso == ft.getPeso()
            && data.equals(ft.getData())
            && precoEntrega == ft.getPrecoEntrega()
            && estado == ft.getEstado()
            && med == isMed();
        }
    }
    
    public int hashCode() {
        final int primo = 31;
        int result = 1;
        if(isMed()) result = 2;
        result = primo * result + ((codEncomenda == null) ? 0 : codEncomenda.hashCode());
        result = primo * result + ((codUser == null) ? 0 : codUser.hashCode());
        result = primo * result + ((codLoja == null) ? 0 : codLoja.hashCode());
        result = primo * result + ((data == null) ? 0 : data.hashCode());
        long aux = Double.doubleToLongBits(peso);
        result = primo * result + (int)(aux ^ (aux >>> 32));
        long aux1 = Double.doubleToLongBits(precoEntrega);
        result = primo * result + (int)(aux1 ^ (aux1 >>> 32));
        result = primo * result + estado;
        for(LinhaEncomenda le : this.produtos)
            result = primo * result + le.hashCode();
        return result;
    }
}
