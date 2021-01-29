package TrazAqui;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Utilizador implements Entrada, Serializable {
    /**
     * Variaveis de instancia
     */
    private String nome;
    private String cod;
    private GPS localizacao;
    private Map<String,Encomenda> encomendasConcluidas;

    /**
     * Construtor vazio
     */
    public Utilizador() {
        this.nome = "";
        this.cod = "";
        this.localizacao = new GPS();
        this.encomendasConcluidas = new HashMap<>();
    }

    /**
     * Construtor parametrizado
     * @param n String nome 
     * @param c String codigo
     * @param pos GPS localizacao
     * @param l Map<String,Encomenda> map de codigos de encomenda para encomenda
     */
    public Utilizador(String n, String c, GPS pos, Map<String,Encomenda> l) {
        this.nome = n;
        this.localizacao = pos;
        this.cod = c;
        this.setEncomendasConcluidas(l);
    }

    /**
     * Construtor por copia
     * @param u Utilizador 
     */
    public Utilizador(Utilizador u) {
        this.nome = u.getNome();
        this.cod = u.getCod();
        this.localizacao = u.getLocalizacao();
        this.encomendasConcluidas = u.getEncomendasConcluidas();
    }

    /**
     * Retorna uma copia da class que a chama
     * @return Utilizador 
     */
    public Utilizador clone() {
        return new Utilizador(this);
    }


    /**
     * Retorna os dados o voluntario em formato string
     * @return String
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Utilizador{");
        sb.append("nome='").append(nome).append('\'');
        sb.append(", cod='").append(cod).append('\'');
        sb.append(", localizacao=").append(localizacao);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Compara o objeto recebido com o que chama
     * @param o Object
     * @return boolean
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o==null || this.getClass().equals(o.getClass())) return true;
        Utilizador u = (Utilizador) o;

        return this.localizacao.equals(u.getLocalizacao()) &&
                this.cod.equals(u.getCod()) &&
                this.nome.equals(u.getNome());
    }

    /**
     * Retorna as encomendas concluidas
     * @return Map<String, Encomenda> map de codigo de encomenda para encomenda
     */
    public Map<String, Encomenda> getEncomendasConcluidas() {
        Map<String,Encomenda> ret = new HashMap<>();
        for(Map.Entry<String,Encomenda> a:this.encomendasConcluidas.entrySet())
            ret.put(a.getKey(),a.getValue().clone());
        return ret;
    }

    /**
     * Define as encomendas concluidas
     * @param encomendasConcluidas Map<String, Encomenda> map de codigo de encomenda para encomenda 
     */
    public void setEncomendasConcluidas(Map<String, Encomenda> encomendasConcluidas) {
        this.encomendasConcluidas = new HashMap<>();
        for(Map.Entry<String,Encomenda> e: encomendasConcluidas.entrySet())
            this.encomendasConcluidas.put(e.getKey(),e.getValue().clone());
    }

    /**
     * Recorde o seu nome
     * @return String
     */
    public String getNome() {
        return nome;
    }

    /**
     * Define o seu nome
     * @param nome String
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Retorna o seu codigo
     * @return String 
     */
    public String getCod() {
        return cod;
    }

    /**
     * Define o seu codigo
     * @param cod String
     */
    public void setCod(String cod) {
        this.cod = cod;
    }

    /**
     * Retorna a sua localizacao
     * @return GPS
     */
    public GPS getLocalizacao() {
        return localizacao;
    }

    /**
     * Define a sua localizacao
     * @param localizacao GPS
     */
    public void setLocalizacao(GPS localizacao) {
        this.localizacao = localizacao;
    }

    /**
     * Adiciona uma encomenda as encomendas concluidas
     * @param e Encomenda
     */
    public void addEncomenda(Encomenda e) {
        this.encomendasConcluidas.put(e.getCod(),e.clone());
    }

    /**
     * Retorna o nome
     * @return String
     */
    public String toStringNome() {
        return "Utilizador";
    }

    /**
     * Procura pela lista de encomendas feitas em determinado intervalo de tempo 
     * @param inicio LocalDateTime data inicial 
     * @param fim LocalDateTime data final 
     * @return List<Encomenda> lista de encomendas 
     */
    public List<Encomenda> procuraPor(LocalDateTime inicio, LocalDateTime fim) {
        List<Encomenda> aux=new ArrayList<>();
        for(Map.Entry<String,Encomenda> map : this.getEncomendasConcluidas().entrySet()){
            if(map.getValue().getData().isAfter(inicio) && map.getValue().getData().isBefore(fim)) aux.add(map.getValue());
        }
        return aux;
    }
}
