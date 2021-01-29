package src.model;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Classe que cria Utilizadores
 */

public class Utilizador extends User {

    //Variáveis globais
    private static String codUtilizadorGlobal = "u0";

    //Variáveis de Instância
    private Map<String,Encomenda> pedidosEncomenda;
    private List<LinhaEncomenda> novasLinhas;
    private List<Entrega> historicoEntregas;


    /**
     * Métodos de Classe
     */

    /**
     * Devolve o username de model.Utilizador gerado para o próximo model.Utilizador a ser criada
     * @return String com o novo username de model.Utilizador
     */
    private static String getCodUtilizadorGlobal(){
        String cod = Utilizador.codUtilizadorGlobal;
        String inc = cod.substring(1);
        int i = Integer.parseInt(inc);
        i = i+1;
        Utilizador.setCodUtilizadorGlobal("u"+i);
        return cod;
    }

    /**
     * Atualiza o o username de model.Utilizador gerado para o próximo model.Utilizador a ser criada
     * @param cod String com o novo username de model.Utilizador que substituirá o atual
     */
    private static void setCodUtilizadorGlobal(String cod){
        Utilizador.codUtilizadorGlobal = cod;
    }

    /**
     * Atualiza o username de Utilizador a ser atribuido para o utilizador seguinte, baseado no ultimo codigo atribuído através de logs;
     * @param cod String com o ultimo username de Utilizador usado nos logs
     */
    public static void updateCodGlobal(String cod){
        String atual = Utilizador.getCodUtilizadorGlobal();
        int numberAtual = Integer.parseInt(atual.substring(1));
        int numberCod = Integer.parseInt(cod.substring(1));
        if (numberCod > numberAtual) {
            Utilizador.setCodUtilizadorGlobal("u"+(numberCod+1));
        }
    }

    /**
     * Construtores da classe model.Utilizador.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.Utilizador.
     */

    public Utilizador(){
        super();
        this.setUsername(Utilizador.getCodUtilizadorGlobal());
        this.historicoEntregas = new ArrayList<>();
        this.novasLinhas = new ArrayList<>();
        this.pedidosEncomenda = new HashMap<>();
    }

    /**
     * Construtor parametrizado de model.Utilizador.
     * Aceita como parâmetros os valores para cada variável de instância e chama o construtor de super.
     */

    public Utilizador( String password, String nome, String email, Ponto p, List<Entrega> h){
        super(Utilizador.getCodUtilizadorGlobal(),password, nome,email,p);
        this.novasLinhas = new ArrayList<>();
        this.pedidosEncomenda = new HashMap<>();
        this.setHistoricoEntregas(h);
    }

    /**
     * Construtor de cópia de model.Voluntario.
     * Aceita como parâmetro outro model.Voluntario e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public Utilizador(Utilizador u){
        super(u.getUsername(),u.getPassword(),u.getNome(),u.getEmail(),u.getLocalizacao());
        this.historicoEntregas = u.getHistoricoEntregas();
        this.pedidosEncomenda = u.getPedidosEncomenda();
        this.setNovasLinhas(u.getNovasLinhas());
    }



    /**
     * Construtor parametrizado de model.Utilizador para construção a partir dos logs.
     * Aceita como parâmetros os valores para cada variável de instância e chama o construtor de super.
     */
    public Utilizador(String cod, String password, String nome, String email, Ponto p, List<Entrega> h){
        super(cod,password, nome,email,p);
        Utilizador.updateCodGlobal(cod);
        this.novasLinhas = new ArrayList<>();
        this.pedidosEncomenda = new HashMap<>();
        this.setHistoricoEntregas(h);
    }


    /**
     * Métodos de Instância
     */

    /**
     * Devolve o registo interno de Entregas recebidas pelo model.Utilizador
     * @return List<model.Entrega> com todas as Entregas recebidas pelo model.Utilizador
     */

    public List<Entrega> getHistoricoEntregas() {
        ArrayList<Entrega> res = new ArrayList<>();

        for(Entrega e : this.historicoEntregas){
            res.add(e.clone());
        }
        return res;
    }

    /**
     * Devolve o registo das novas linhas a serem adicionadas em uma encomenda criadas pelo model.utilizador
     * @return List<model.LinhaEncomenda> lista das linhas de encomenda criadas
     */
    public List<LinhaEncomenda> getNovasLinhas() {
        List<LinhaEncomenda> res = new ArrayList<>();

        for(LinhaEncomenda l : this.novasLinhas){
            res.add(l.clone());
        }

        return res;
    }

    /**
     * Devolve o registo dos pedidos de encomenda criados pelo model.utilizador
     * @return Map<String,model.Encomenda> Map com as encomendas pedidas associadas aos seus códigos
     */
    public Map<String,Encomenda> getPedidosEncomenda() {
        Map<String,Encomenda> res = new HashMap<>();

        for(Map.Entry<String,Encomenda> e : this.pedidosEncomenda.entrySet()){
            res.put(e.getKey(),e.getValue().clone());
        }

        return res;
    }


    /**
     * Atualiza o registo das novas linhas a serem adicionadas em uma encomenda criadas pelo model.utilizador
     * @param novasLinhas lista das linhas de encomenda a substituirem as atuais
     */
    public void setNovasLinhas(List<LinhaEncomenda> novasLinhas) {
        List<LinhaEncomenda> nova = new ArrayList<>();

        for(LinhaEncomenda l : novasLinhas){
            nova.add(l.clone());
        }

        this.novasLinhas = nova;
    }

    /**
     * Atualiza o registo interno de Entregas recebidas pelo model.Utilizador
     * @param historicoEntregas novo registo de Entregas
     */

    public void setHistoricoEntregas(List<Entrega> historicoEntregas) {
        ArrayList<Entrega> res = new ArrayList<>();
        for(Entrega e : historicoEntregas){
            res.add(e.clone());
        }

        this.historicoEntregas = res;
    }

    /**
     * Atualiza o registo dos pedidos de encomenda criados pelo model.utilizador
     * @param pedidos Map com as encomendas pedidas associadas aos seus códigos para substiruir o map atual
     */
    public void setPedidosEncomenda(Map<String,Encomenda> pedidos) {
        Map<String,Encomenda> res = new HashMap<>();
        for(Map.Entry<String,Encomenda> e : pedidos.entrySet()){
            res.put(e.getKey(),e.getValue().clone());
        }
        this.pedidosEncomenda = res;
    }

    /**
     * Adiciona uma encomenda ao registo dos pedidos de encomenda criados pelo model.utilizador
     * @param e pedido de encomenda a ser adicionado aos pedidos
    */
    public void addPedidoEncomenda(Encomenda e){
        this.pedidosEncomenda.put(e.getCodEnc(),e.clone());
    }


    /**
     * Remove uma encomenda ao registo dos pedidos de encomenda criados pelo model.utilizador
     * @param codEnc código da encomenda a ser removida dos pedidos
    */
    public Encomenda removePedidoEncomenda(String codEnc){
        return this.pedidosEncomenda.remove(codEnc);
    }

    /**
     * Adiciona uma entrega ao registo interno de Entregas recebidas pelo model.Utilizador
     * @param e Entrega a ser adicionada no histórico do utilizador
     */
    public void addEntregaHistorico(Entrega e){
        this.historicoEntregas.add(e.clone());
    }

    /**
     * Adiciona uma Linha de encomenda às linhas de encomenda que o utilizador queira incluir em uma futura encomenda
     * @param p produto a ser adicionado
     * @param quantidade quantidade do produto a ser adicionado
     */
    public void addLinhaEncomenda(Produto p, double quantidade){
        LinhaEncomenda nova = new LinhaEncomenda(p,quantidade);
        this.novasLinhas.add(nova);
    }


    /**
     * Método que transforma um model.Utilizador numa String
     * @return String com toda a informação presente no model.Utilizador
     */

    public String toString() {
        String user = super.toString();
        final StringBuilder sb = new StringBuilder("model.Utilizador{");
        sb.append(", historicoEntregas=").append(this.historicoEntregas.toString());
        sb.append(", pedidosDeEncomenda=").append(this.pedidosEncomenda.toString());
        sb.append(", novasLinhas=").append(this.novasLinhas.toString());
        sb.append('}');
        return user + sb.toString();
    }

    /**
     * Método que determina se um model.Utilizador e um Objeto são iguais
     * @param o Objeto qualquer
     * @return true caso o model.Utilizador e o Objeto sejam iguais, e vice versa
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Utilizador u = (Utilizador) o;
        return this.historicoEntregas.equals(u.getHistoricoEntregas()) &&
                this.pedidosEncomenda.equals(u.getPedidosEncomenda()) &&
                this.novasLinhas.equals(u.getNovasLinhas());
    }

    /**
     * Método que clona um model.Utilizador, para tal é usado o construtor de cópia
     * @return Objeto model.Utilizador que é uma cópia do model.Utilizador
     */

    public Utilizador clone(){
        return new Utilizador(this);
    }


}
