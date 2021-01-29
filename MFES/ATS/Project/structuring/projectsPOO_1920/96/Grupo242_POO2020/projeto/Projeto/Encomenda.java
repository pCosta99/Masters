package Projeto;

import java.util.ArrayList;

public class Encomenda {
    private String code;
    private String user;
    private String loja;
    private String peso;
    private ArrayList<LinhaEncomendas> LinhaEncomendas;

    public Encomenda(String code, String user, String loja, String peso, ArrayList<LinhaEncomendas> LinhaEncomendas){
        this.code = code;
        this.peso = peso;
        this.user = user;
        this.loja = loja;
        this.LinhaEncomendas = LinhaEncomendas;
    }

    public void setPeso(String peso){
        this.peso = peso;
    }

    public String getCode() {
        return code;
    }

    public String getUser() {
        return user;
    }

    public String getPeso() {
        return peso;
    }

    public ArrayList<Projeto.LinhaEncomendas> getLinhaEncomendas() {
        return LinhaEncomendas;
    }

    public String getLoja(){
        return this.loja;
    }

    public void addEncomendas(LinhaEncomendas enc){
        this.LinhaEncomendas.add(enc);
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    @Override
    public String toString() {
        return "Encomenda{" +
                "code='" + code + '\'' +
                ", user='" + user + '\'' +
                ", loja='" + loja + '\'' +
                ", peso=" + peso +
                ", LinhaEncomendas=" + LinhaEncomendas +
                '}';
    }
}
