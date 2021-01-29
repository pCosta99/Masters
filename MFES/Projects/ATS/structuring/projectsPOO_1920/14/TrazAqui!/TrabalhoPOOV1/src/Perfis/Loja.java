package Perfis;

import Auxiliares.GPS;
import Encomendas.Encomenda;
import Encomendas.EncomendaMedica;
import Encomendas.EncomendaNormal;
import Encomendas.LinhaEncomenda;

import java.io.Serializable;
import java.lang.String;
import java.util.ArrayList;
import java.util.HashMap;


public class Loja extends Perfil implements Serializable {

    /*
    Variaveis
     */
    private ArrayList<Encomenda> encomendasEmFila;
    private HashMap<String, LinhaEncomenda> produtos;


    /*
    Construtores
     */

    public Loja(String codigoPerfil, GPS coordenadas, String nome, String email, String pass, ArrayList<Encomenda> encomendasEmFila, HashMap<String, LinhaEncomenda> produtos) {
        super(codigoPerfil,coordenadas, nome, email, pass);
        this.produtos = produtos;
    }

    public Loja() {
        super();
        this.produtos = new HashMap<>();
    }

    public Loja(Loja other) {
        super(other);
        this.produtos = other.getProdutos();
    }


    /*
    Getters e Setters
     */


    public ArrayList<Encomenda> getEncomendasEmFila() {

        if(this.encomendasEmFila == null)
        {
            System.out.println("Esta loja não tem encomendas em fila!");
            return new ArrayList<Encomenda>();
        }
        else {

            ArrayList<Encomenda> temp = new ArrayList<>();
            for (Encomenda encomenda : this.encomendasEmFila) temp.add(encomenda.clone());
            return temp;
        }


    }

    public void setEncomendasEmFila(ArrayList<Encomenda> emFila) {
        this.encomendasEmFila = new ArrayList<>();
        for (Encomenda encomenda : emFila) encomendasEmFila.add(encomenda.clone());
    }

    public HashMap<String, LinhaEncomenda> getProdutos() {
        HashMap<String, LinhaEncomenda> res = new HashMap<>();
        for (HashMap.Entry<String, LinhaEncomenda> p : this.produtos.entrySet()) {
            res.put(p.getKey(), p.getValue().clone());
        }
        return res;
    }

    public void setProdutos(HashMap<String, LinhaEncomenda> l) {
        this.produtos = new HashMap<>();
        l.forEach((key, value) -> this.produtos.put(key, value.clone()));
    }



    /*
    Metodos
     */


    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Loja: ").append(this.getCodigoPerfil()).append("\n")
                .append(" - Nome: ").append(this.getNome()).append("\n")
                .append(" - Localização: ").append(this.getCoordenadas()).append("\n")
                .append(" - Produtos Dísponiveis: ");
        for (LinhaEncomenda p : this.produtos.values()) {
            sb.append(p.getNome()).append("\n");
        }
        return sb.toString();

    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Loja loja = (Loja) o;
        return super.equals(loja) &&
                this.getCodigoPerfil().equals(loja.getCodigoPerfil()) &&
                this.encomendasEmFila.equals(loja.getEncomendasEmFila()) &&
                this.produtos.equals(loja.getProdutos());

    }

    public Loja clone() {
        return new Loja(this);
    }

    // Se tiver encomendas = True,  else = False

    public boolean sinalizarEncomendas() {

        if(this.encomendasEmFila == null) {
            return false;
        }
        else {
            return encomendasEmFila.size() != 0;
        }
    }



    // toString de todos os nomes dos produtos
    public String toStringProdutos(){
        StringBuilder sb = new StringBuilder();
        for (LinhaEncomenda l : this.getProdutos().values()) {
            sb.append(" - ").append(l.getNome()).append("\n");
        }
        return sb.toString();
    }


    // toString de todos os codigos de encomenda da loja
    public String toStringEncomendas(Transportadores t){
        StringBuilder sb = new StringBuilder();
        StringBuilder medicas = new StringBuilder();
        StringBuilder normais = new StringBuilder();


        for (Encomenda l : this.encomendasEmFila) {

            if (l instanceof EncomendaMedica) {
                medicas.append(" - ").append(l.getCodEncomenda()).append("\n");
            }
            if(l instanceof  EncomendaNormal){
                normais.append(" - ").append(l.getCodEncomenda()).append("\n");
            }
        }
        if(!normais.toString().isEmpty() && !medicas.toString().isEmpty()){ // Existem encomendas normais e medicas
            sb.append("A Loja  ").append(this.getNome()).append(" encontra-se a ").append((t.getCoordenadas().distanciaEntre(this.getCoordenadas()))).append("kms de si, e tem estas encomendas disponíveis:\n")
                    .append("----Encomendas Normais---- \n").append(normais.toString()).append("\n")
                    .append("----Encomendas Medicas---- \n").append(medicas.toString()).append("\n");
        }
        else {
            if (!normais.toString().isEmpty() && medicas.toString().isEmpty()) { // So existe encomendas normais
                sb.append("A Loja  ").append(this.getNome()).append(" encontra-se a ").append((t.getCoordenadas().distanciaEntre(this.getCoordenadas()))).append("kms de si, e tem estas encomendas disponíveis:\n")
                        .append("----Encomendas Normais---- \n").append(normais.toString()).append("\n");
            }
            if(normais.toString().isEmpty() && !medicas.toString().isEmpty()) { // So existe encomendas medicas
                sb.append("A Loja  ").append(this.getNome()).append(" encontra-se a ").append((t.getCoordenadas().distanciaEntre(this.getCoordenadas()))).append("kms de si, e tem estas encomendas disponíveis:\n")
                        .append("----Encomendas Medicas---- \n").append(medicas.toString()).append("\n");
            }
        }

        return sb.toString();
    }

    // Quando o utilizador escreve o nome do produto mas escolhe mal, para ver se esse produto esta na loja
    public String getCodFromNome(String nomeProd){
        for(LinhaEncomenda l : produtos.values()){
            if(l.getNome().equals(nomeProd)){return l.getCodProduto();}
        }
        return "";
    }

    // Quando o utilizador acaba de fazer o pedido de encomenda, adicionamos esta as encomendas em fila
    // (que estao a espera de um transportador)
    public void adicionaEncomendasEmFila(Encomenda e){
        if(this.encomendasEmFila == null)
        {
            this.setEncomendasEmFila(new ArrayList<Encomenda>());
            this.encomendasEmFila.add(e);
            this.setEncomendasEmFila(encomendasEmFila);   //Aparece

        }
        else {
            this.encomendasEmFila.add(e);
            this.setEncomendasEmFila(encomendasEmFila);  //Aparece
        }
    }

    // Quando o utilizador escolhe uma empresa ou e lhe atribuido um voluntario, removemos a encomenda do ArrayList encomendasEmFila
    public void removeEncomendasEmFila(Encomenda e){
        ArrayList<Encomenda> novo = new ArrayList<>(this.encomendasEmFila);
        novo.removeIf(enc -> enc.equals(e));
        this.setEncomendasEmFila(novo);
    }

    public void addProduto(LinhaEncomenda p){

        HashMap<String,LinhaEncomenda> novo = new HashMap<> (this.produtos);
        novo.put(p.getCodProduto(),p);
        this.setProdutos(novo);
    }

    public String toStringGrava(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getCodigoPerfil()).append(",")
                .append(this.getNome()).append(",")
                .append(this.getCoordenadas().toStringGrava());

        return sb.toString();
    }

    public String verificaProduto(String nomeProduto){
        for(LinhaEncomenda p: produtos.values()){
            if(p.getNome().equals(nomeProduto)){return p.getCodProduto();}
        }
        return "";
    }

    // Aqui ja existe o codigo no hashMap
    public void removeProduto(String codigo){
        HashMap<String,LinhaEncomenda> novo = new HashMap<>(this.produtos);
        novo.remove(codigo);
        this.setProdutos(novo);
    }

    public void atualizaProdutos(String codProduto, double quant){

        this.produtos.get(codProduto).setQuantidade(quant);
    }
}
