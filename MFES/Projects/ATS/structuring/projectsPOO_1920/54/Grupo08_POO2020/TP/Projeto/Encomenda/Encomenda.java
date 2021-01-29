package Projeto.Encomenda;
import Projeto.Interfaces.IEncomenda;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que implementa uma Encomenda. 
 * Uma Encomenda e' um conjunto de linhas de encomenda, e mais algumas informaçoes sobre a encomenda.
 */
public class Encomenda implements IEncomenda, Comparable<IEncomenda>, Serializable {
    private String id;
    private String idLoja;
    private String idTransportador;
    private String idUser;
    private float peso;
    private Collection<LinhaDeEncomenda> linhaEncomendas;
    private LocalDate tempRecolha;
    private LocalDate tempEntrega;
    private float precoEntrega;

    /**
     * Variavel de classe
     */
    private static int lastnumber;
    
    /*
     * Construtores da Classe Encomenda.
     * Declaracao dos construtores por omissao, parametrizado e de copia.
     */
    /**
     * Construtor por omissao de Encomenda.
     */
    public Encomenda() {
        this.id = "";
        this.idLoja = "";
        this.idTransportador = "";
        this.idUser = "";
        this.peso = 0;
        this.linhaEncomendas = new ArrayList<>();
        this.precoEntrega = 0;
        this.tempEntrega = LocalDate.now();
        this.tempRecolha = LocalDate.now();
    }
    /**
     * Construtor parametrizado de Encomenda.
     * Aceita como parametros quatro Strings uma para o id da encomenda, outra para o id da loja,
     * outra para o id da empresa e o ultimo para o id do Utilizador.
     * Recebe tambem uma lista de LinhaDeEncomenda, um float para o peso da encomenda e um boolean para identificar se foi aceite ou nao.
     */
    public Encomenda(String i, String loja, String idEmpresa, String user, float p, Collection<LinhaDeEncomenda> l) {
        this.id = i;
        this.idLoja = loja;
        this.idTransportador = idEmpresa;
        this.idUser = user;
        this.peso = p;
        setLinhas(l);
    }

    /**
     * Construtor por copia de Encomenda.
     * Aceita como parametro outra Encomenda e utiliza os metodos de acesso aos valores das variaveis de instancia.
     */
    public Encomenda(Encomenda e) {
        this.id = e.getID();
        this.idLoja = e.getLojaID();
        this.idTransportador = e.getIdTransportador();
        this.idUser = e.getUserID();
        this.peso = e.getPeso();
        this.linhaEncomendas = e.getLinhas();
        this.tempEntrega = e.getTempEntrega();
        this.tempRecolha = e.getTempRecolha();
        this.precoEntrega = e.getPrecoEntrega();
    }
    
    /*
     * Getters e Setters
     */
    /**
     * Devolve o codigo da Encomenda.
     */
    public String getID() {
        return this.id;
    }

    /**
     * Devolve o codigo da loja onde a encomenda esta para ser recolhida.
     */
    public String getLojaID() {
        return this.idLoja;
    }

    /**
     * Devolve o codigo da empresa que recolher a encomenda.
     */
    public String getIdTransportador() {
        return idTransportador;
    }

    /**
     * Devolve o codigo do destinatario da Encomenda.
     */
    public String getUserID() {
        return this.idUser;
    }

    /**
    * Devolve o peso da Encomenda.
    */
    public float getPeso() {
        return this.peso;
    }

    /**
     * Devolve o tempo de recolha
     * @return tempo de recolha
     */
    public LocalDate getTempRecolha() {
        return tempRecolha;
    }

    /**
     * Devolve o tempo de entrega
     * @return tempo de entrega
     */
    public LocalDate getTempEntrega() {
        return tempEntrega;
    }

    /**
     * Devolve o preco de entrega
     * @return preco de entrega
     */
    public float getPrecoEntrega() {
        return precoEntrega;
    }

    /**
     * Devolve as Linhas de Encomenda desta Encomenda.
     */
    public Collection<LinhaDeEncomenda> getLinhas() {
        return this.linhaEncomendas.stream()
                .map(LinhaDeEncomenda::clone)
                .collect(Collectors.toList());
    }

    /**
     * Metodo de classe
     */
    public static int getLastnumber() {
        return Encomenda.lastnumber;
    }

    /**
     * Atualiza o codigo da Encomenda.
     * @param novoID - String com o codigo para atualizar na Encomenda.
     */
    public void setID(String novoID) {
        this.id = novoID;
    }

    /**
     * Atualiza o codigo da Loja de onde vai "sair" esta encomenda.
     * @param novoIDLoja - String com o codigo da Loja para atualizar.
     */
    public void setLojaID(String novoIDLoja) {
        this.idLoja = novoIDLoja;
    }

    /**
     * Atualiza o id da Empresa
     * @param idTransportador - id da Empresa
     */
    public void setIdTransportador(String idTransportador) {
        this.idTransportador = idTransportador;
    }

    /**
     * Atualiza o codigo do Utilizador que vai receber a loja.
     * @param novoUserID - String com o codigo do Utilizador a atualizar.
     */
    public void setUserID(String novoUserID) {
        this.idUser = novoUserID;
    }

    /**
    * Atualiza o peso da Encomenda.
    * @param novoPeso - float com o peso que se vai atualizar na Encomenda.
    */
    public void setPeso(float novoPeso) {
        this.peso = novoPeso;
    }

    /**
     * Atualiza as Linhas de Encomenda na Encomenda.
     * @param novasLinhas - List<LinhaDeEncomenda> lista atualizada.
     */
    public void setLinhas(Collection<LinhaDeEncomenda> novasLinhas) {
        this.linhaEncomendas = novasLinhas.stream()
                .map(LinhaDeEncomenda::clone)
                .collect(Collectors.toList());
    }

    /**
     * Atualiza o tempo de recolha
     * @param tempo tempo de recolha
     */
    public void setTempRecolha(LocalDate tempo) {
        this.tempRecolha = tempo;
    }

    /**
     * Atualiza o tempo de entrega
     * @param tempo tempo de entrega
     */
    public void setTempEntrega(LocalDate tempo) {
        this.tempEntrega = tempo;
    }

    /**
     * Atualiza o preco de entrega
     * @param precoEntrega preco de entrega
     */
    public void setPrecoEntrega(float precoEntrega) {
        this.precoEntrega = precoEntrega;
    }

    /**
     * Metodo de classe que atualiza a variavel de classe.
     */
    public static void setLastnumber(int lastnumber){
        Encomenda.lastnumber = lastnumber;
    }

    /*
     * Restantes Metodos de Instancia
     */
    /**
     * Metodo de classe que incrementa a variavel de classe.
     */
    public static void incLastnumber() {
        Encomenda.lastnumber++;
    }

    /**
     * Metodo que verifica se existe alguma linha de encomenda medicinal na Encomenda.
     * @return boolean Verdadeiro caso exista.
     */
    public boolean containsMed() {
        for (LinhaDeEncomenda l : this.linhaEncomendas)
            if (l.isMedicinal())
                return true;
        return false;
    }

    /**
     * Metodo que insere uma linha de encomenda numa Encomenda.
     * @param l - linha de enconenda a inserir.
     */
    public void insereLinhaEnc(LinhaDeEncomenda l) {
        this.linhaEncomendas.add(l.clone());
    }

    /**
     * Metodo que calcula o preço total da Encomenda.
     * @return float - preço total da Encomenda.
     */
    public float calculaPrecoTotal() {
        float total = 0;
        for(LinhaDeEncomenda l : this.linhaEncomendas) {
            total += l.getPreco() * l.getQuantidade();
        }
        return total;
    }
    
    /**
     * Compara 2 encomendas atraves do seu ID.
     * Vamos precisar disto apra ordenar as encomendas num TreeSet.
     */
    public int compareTo(IEncomenda e) {
        return this.id.compareTo(e.getID());
    }

    /**
     * Verifica se uma determinada empresa transportou a encomenda
     * @param idEmp id da Empresa
     * @return true caso e empresa tenha transportado a encomenda, false em caso contrário
     */
    private boolean empresaNaoTransportouEnc(String idEmp) {
        return !idEmp.equals(this.idTransportador);
    }

    /**
     * Verifica se a encomenda transportou uma Encomenda. Caso tenha transportado, verifica se
     * o tempo de entrega está dentro do intervalo fornecido
     * @param idEmp id da Empreas
     * @param tInicial um array para o tempo inicial, contendo o ano, mes e dia, respetivamente
     * @param tFinal um array para o tempo final, contendo o ano, mes e dia, respetivamente
     * @return true caso se encontre dentro do intervalo, false em caso contrário
     */
    public boolean empTransportouTempo(String idEmp, int[] tInicial, int[] tFinal) {
        String[] campos = this.tempEntrega.toString().split("-");
        int anoEnc = Integer.parseInt(campos[0]);
        int mesEnc = Integer.parseInt(campos[1]);
        int diaEnc = Integer.parseInt(campos[2]);
        if (this.empresaNaoTransportouEnc(idEmp)) return false;
        return tInicial[0] <= anoEnc && tInicial[1] <= mesEnc && tInicial[2] <= diaEnc &&
                tFinal[0] >= anoEnc && tFinal[1] >= mesEnc && tFinal[2] >= diaEnc;
    }

    /**
     * Verifica se a empresa transportou uma Encomenda. Caso tenha transportado, verifica se
     * o tempo de entrega corresponde ao tempo fornecido.
     * @param idEmp id da Empresa
     * @param tempo um array que contém o ano, mes e dia, respetivamernte
     * @return true caso o tempo fornecido seja igual ao tempo de entrega, false em caso contrário
     */
    public boolean empTransportouTempo(String idEmp, int[] tempo) {
        String[] campos = this.tempEntrega.toString().split("-");
        int anoEnc = Integer.parseInt(campos[0]);
        int mesEnc = Integer.parseInt(campos[1]);
        int diaEnc = Integer.parseInt(campos[2]);
        if (!this.empresaNaoTransportouEnc(idEmp)) return false;
        return tempo[0] <= anoEnc && tempo[1] <= mesEnc && tempo[2] <= diaEnc;
    }
    /**
     * Metodo que faz uma copia do objeto receptor da mensagem.
     * @return objeto clone do objeto que recebe a mensagem.
     */
    public Encomenda clone() {
        return new Encomenda(this);
    }

    /**
     * Metodo que devolve a representaçao em String da Encomenda.
     * @return String com as variaveis desta instancia.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\nID da Encomenda:").append(this.id)
                .append("\nID da Loja:").append(this.idLoja)
                .append("\nID do Utilizador:").append(this.idUser)
                .append("\nPeso da encomenda:").append(this.peso);
        for(LinhaDeEncomenda l : this.linhaEncomendas){
            sb.append("\n\t").append(l.toString());
        }
        sb.append("\n");
        return sb.toString();
    }
    /**
     * Metodo que determina se duas Encomendas sao iguais.
     * @return boolean verdadeiro caso o id de duas Encomendas seja igual.
     */
    public boolean equals(Object o) {
        if(o == this) return true;
        if(o == null || o.getClass() != this.getClass()) return false;
        Encomenda e = (Encomenda) o;
        return this.id.equals(e.getID());
    }

    /**
     * Método hashCode
     */
    @Override
    public int hashCode() {
        return Objects.hash(id, idLoja, idTransportador, idUser, peso, linhaEncomendas, tempRecolha, tempEntrega, precoEntrega);
    }
}