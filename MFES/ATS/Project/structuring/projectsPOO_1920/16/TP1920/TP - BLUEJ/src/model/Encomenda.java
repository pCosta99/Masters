package src.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Classe que cria as Encomendas
 */

public class Encomenda implements Comparable<Encomenda>, Serializable {

    //Variáveis de classe    
    private static String codEncGlobal = "e0";


    //Variáveis de Instância
    private List<LinhaEncomenda> linhas;
    private String codEnc;
    private String codDest;
    private String codLoja;
    private double peso;
    private double custo;
    boolean medicamentos;

    /**
     * Métodos de Classe
     */

    /**
     * Devolve o código de encomenda gerado para a próxima encomenda a ser criada
     * @return String com o novo código de encomenda
     */
    private static String getCodEncGlobal(){
        String cod = Encomenda.codEncGlobal;
        String inc = cod.substring(1);
        int i = Integer.parseInt(inc);
        i = i+1;
        Encomenda.setCodEncGlobal("e"+i);
        return cod;
    }

    /**
     * Atualiza o código de encomenda gerado para a próxima encomenda a ser criada
     * @param cod String com o novo código de encomenda que substituirá o atual
     */
    private static void setCodEncGlobal(String cod){
        Encomenda.codEncGlobal = cod;
    }

    /**
     * Atualiza o código de encomenda a ser atribuido para a encomenda seguinte baseado no ultimo codigo atribuído através de logs;
     * @param cod String com o ultimo código de encomenda usado nos logs
     */
    public static void updateCodGlobal(String cod){
        String atual = Encomenda.getCodEncGlobal();
        int numberAtual = Integer.parseInt(atual.substring(1));
        int numberCod = Integer.parseInt(cod.substring(1));
        if (numberCod > numberAtual) {
            Encomenda.setCodEncGlobal("e"+(numberCod+1));
        }
    }

    /**
     * Construtores da classe model.Encomenda.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.LinhaEncomenda.
     */
    public Encomenda(){
        this.codEnc = Encomenda.getCodEncGlobal();
        this.linhas = new ArrayList<>();
        this.codDest = "";
        this.codLoja = "";
        this.peso = 0;
        this.custo = 0;
        this.medicamentos = false;
    }

    /**
     * Construtor parametrizado de model.Encomenda.
     * Aceita como parâmetros os valores para cada variável de instância.
     */

    public Encomenda(List<LinhaEncomenda> linhas, String codDest, String codLoja){
        this.codEnc = Encomenda.getCodEncGlobal();
        this.setLinhas(linhas);
        this.codDest = codDest;
        this.codLoja = codLoja;
        double p = 0;
        double c = 0;
        for(LinhaEncomenda e : linhas){
            p += e.getPeso();
            c += e.getPreco();
        }
        this.peso = p;
        this.custo = c;
        this.medicamentos = this.tem_medicamentos(linhas);
    }

    /**
     * Construtor de cópia de model.Encomenda.
     * Aceita como parâmetro outra model.LinhaEncomenda e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public Encomenda(Encomenda e){
        this.codEnc = e.getCodEnc();
        this.linhas = e.getLinhas();
        this.codDest = e.getCodDest();
        this.codLoja = e.getCodLoja();
        this.peso = e.getPeso();
        this.custo = e.getCusto();
        this.medicamentos = e.is_medicamento();
    }

    /**
     * Construtor parametrizado de model.Encomenda para criação de instância a partir de logs.
     * Aceita como parâmetros os valores para cada variável de instância.
     */

    public Encomenda(String cod, double peso, List<LinhaEncomenda> linhas, String codDest, String codLoja){
        this.codEnc = cod;
        Encomenda.updateCodGlobal(cod);
        this.setLinhas(linhas);
        this.codDest = codDest;
        this.codLoja = codLoja;
        double c = 0;
        for(LinhaEncomenda e : linhas){
            c += e.getPreco();
        }
        this.peso = peso;
        this.custo = c;
        this.medicamentos = this.tem_medicamentos(linhas);
    }


    /**
     * Métodos de Instância
     */

    /**
     * Devolve o peso da model.Encomenda
     * @return double peso da model.Encomenda
     */

    public double getPeso() {
        return this.peso;
    }

    /**
     * Devolve o custo da model.Encomenda
     * @return double custo da model.Encomenda
     */

    public double getCusto() {
        return this.custo;
    }

    /**
     * Devolve o código do Destinatário
     * @return String código do Destinatário
     */

    public String getCodDest() {
        return this.codDest;
    }

    /**
     * Devolve o código da model.Encomenda
     * @return String código da model.Encomenda
     */

    public String getCodEnc() {
        return this.codEnc;
    }

    /**
     * Devolve o código da model.Loja da qual a model.Encomenda provém
     * @return String código da model.Loja
     */

    public String getCodLoja() {
        return this.codLoja;
    }

    /**
     * Devolve as linhas de encomenda que constituem a model.Encomenda
     * @return ArrayList cópia das Linhas de model.Encomenda
     */

    public List<LinhaEncomenda> getLinhas() {
        ArrayList<LinhaEncomenda> res = new ArrayList<>();
        for(LinhaEncomenda e : this.linhas){
            res.add(e.clone());
        }

        return  res;
    }

    /**
     * Determina se uma encomenda contém medicamentos
     * @return true caso a encomenda contenha medicamentos, false caso contrário
     */

    public boolean is_medicamento(){
        return this.medicamentos;
    }

    /**
     * Atualiza o peso da model.Encomenda
     * @param peso novo peso da model.Encomenda
     */

    public void setPeso(double peso) {
        this.peso = peso;
    }

    /**
     * Atualiza o custo da model.Encomenda
     * @param custo novo custo da model.Encomenda
     */

    public void setCusto(double custo) {
        this.custo = custo;
    }

    /**
     * Atualiza o código do Destinatário da model.Encomenda
     * @param codDest novo código do Destinatário
     */

    public void setCodDest(String codDest) {
        this.codDest = codDest;
    }

    /**
     * Atualiza o código da model.Encomenda
     * @param codEnc novo código da model.Encomenda
     */

    public void setCodEnc(String codEnc) {
        this.codEnc = codEnc;
    }

    /**
     * Atualiza o código da model.Loja da qual a model.Encomenda provém
     * @param codLoja novo código da model.Loja
     */

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    /**
     * Atualiza as linhas de encomenda que constituem a model.Encomenda
     * @param linhas novo registo das Linhas de model.Encomenda
     */

    public void setLinhas(List<LinhaEncomenda> linhas) {
        ArrayList<LinhaEncomenda> l = new ArrayList<>();
        for(LinhaEncomenda e : linhas){
            l.add(e.clone());
        }
        this.linhas = l;
    }

    /**
     * Atualiza a v.i. que determina se a encomenda tem medicamentos ou não
     * @param med novo estado interno da encomenda
     */

    public void setMedicamentos(boolean med){
        this.medicamentos = med;
    }

    /**
     * Método que corre as linhas da model.Encomenda para determinar se esta contém medicamentos
     * @param linhas linhas de encomenda que o método irá correr
     * @return true se a encomenda possuir medicamento
     */

    private boolean tem_medicamentos(List<LinhaEncomenda> linhas){

        for( LinhaEncomenda l : linhas){
            if( l.getProduto().getMedicamento()) return true;
        }

        return false;
    }

    /**
     * Método que adiciona uma model.LinhaEncomenda à model.Encomenda
     * @param l nova linha para adicionar à model.Encomenda
     */

    public void addLinha(LinhaEncomenda l){
        if(l != null){
            boolean var = false;

            for(LinhaEncomenda le : this.linhas){
                if(le.getProduto() == l.getProduto()){
                    le.setQuantidade(le.getQuantidade() + l.getQuantidade());
                    le.setPreco(le.getPreco() + l.getPreco());
                    le.setPeso(le.getPeso() + l.getPeso());
                    var = true;
                    break;
                }
            }

            if(var){
                this.linhas.add(l.clone());
            }
        }
    }



    /**
     * Método que transforma uma model.Encomenda numa String
     * @return String com toda a informação presente no objeto model.Encomenda
     */
    public String toString() {
        final StringBuilder sb = new StringBuilder("Encomenda{");
        sb.append(this.codEnc).append('\n');
        sb.append("codDest= ").append(this.codDest).append('\n');
        sb.append("/ codLoja= ").append(this.codLoja).append('\n');
        sb.append("/ peso= ").append(this.peso).append('\n');
        sb.append("/ preço= ").append(this.custo).append('\n');
        sb.append("/ linhas= {\n").append(this.linhas.toString()).append(" }\n");
        sb.append("/ medicamento= ").append(this.medicamentos).append('\n');
        sb.append('}');
        return sb.toString();
    }


    /**
     * Método que retorna um objeto que é uma cópia exata daquele
     * a qual o clone foi enviado
     * @return Cópia do objeto model.Encomenda
     */

    public Encomenda clone(){
        return new Encomenda(this);
    }

    /**
     * Método que determina se o Objeto model.Encomenda é igual
     * a um dado Objeto
     * @param o Object a ser comparado
     * @return true, caso os objetos sejam iguais, false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Encomenda e = (Encomenda) o;
        return e.getPeso() == this.peso &&
                e.getCusto() == this.custo &&
                e.getCodEnc().equals(this.codEnc) &&
                e.getLinhas().equals(this.linhas) &&
                e.getCodDest().equals(this.codDest) &&
                e.getCodLoja().equals(this.codLoja) &&
                e.is_medicamento() == this.is_medicamento();
    }

    /**
     * Método que compara o this model.Encomenda com outro model.Encomenda a partir da ordem lexicográfica de seus códigos
     * @param e2 model.Encomenda com o qual o this será comparado
     * @return inteiro correspondente à comparação da ordem lexicográfica associada
     */    

    public int compareTo(Encomenda e2){
        return this.codEnc.compareTo(e2.getCodEnc());
    }

}
