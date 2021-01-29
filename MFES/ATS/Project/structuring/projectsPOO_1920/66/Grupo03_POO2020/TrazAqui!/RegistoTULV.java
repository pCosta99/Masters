import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Super-classe das classes Utilizador, Loja, Transportadora e Voluntario
 */
public class RegistoTULV implements Serializable{
    private String codigo;
    private String mail;
    private String nome;
    private Ponto gps;
    private String password;
    private TreeMap<String,EncDistr> encomendas;

    /**
     * Construtor vazio
     */
    public RegistoTULV() {
        this.codigo = "";
        this.nome = "";
        this.gps = new Ponto();
        this.password = "";
        this.encomendas = new TreeMap<>();
        this.mail = "";
    }

    /**
     * Construtor com argumentos
     * @param c Código
     * @param n Nome
     * @param p GPS
     * @param pass Password
     * @param e Encomendas
     * @param m Email
     */
    public RegistoTULV(String c, String n, Ponto p, String pass,TreeMap<String, EncDistr> e, String m) {
        this.codigo = c;
        this.nome = n;
        this.gps = p;
        this.password = pass;
        TreeMap<String, EncDistr> copy = new TreeMap<>();
        for (Map.Entry<String, EncDistr> entry : e.entrySet()) {
            copy.put(entry.getKey(), entry.getValue().clone());
        }
        this.encomendas = copy;
        this.mail = m;
    }

    /**
     * Construtor que recebe um objeto RegistoTULV
     * @param i RegistoTULV
     */
    public RegistoTULV(RegistoTULV i) {
        this.codigo = i.codigo;
        this.nome = i.nome;
        this.gps = i.gps.clone();
        this.password = i.password;
        TreeMap<String, EncDistr> copy = new TreeMap<>();
        for (Map.Entry<String, EncDistr> entry : i.encomendas.entrySet()) {
           copy.put(entry.getKey(), entry.getValue().clone());
        }
        this.encomendas = copy;
        this.mail = i.mail;
    }

    /**
     * Devolve código
     * @return Código
     */
    public String getCodigo () {return this.codigo;}

    /**
     * Devolve nome
     * @return Nome
     */
    public String getNome () {return this.nome;}

    /**
     * Devolve GPS
     * @return GPS
     */
    public Ponto getGps () {return this.gps.clone();}

    /**
     * Devolve password
     * @return Password
     */
    public String getPassWord () { return this.password; }

    /**
     * Devolve map das encomendas
     * @return Encomendas
     */
    public TreeMap<String, EncDistr> getEncomendas() {
        TreeMap<String, EncDistr> copy = new TreeMap<>();
        for (Map.Entry<String, EncDistr> entry : this.encomendas.entrySet()) {
           copy.put(entry.getKey(), entry.getValue().clone());
        }
        return copy;
    }

    /**
     * Devolve email
     * @return Email
     */
    public String getMail () { return this.mail; }

    /**
     * Introduz Código
     * @param c Código
     */
    public void setCodigo (String c) {this.codigo = c;}

    /**
     * Introduz nome
     * @param c Nome
     */
    public void setNome (String c) {this.nome = c;}

    /**
     * Introduz GPS
     * @param c GPS
     */
    public void setGps (Ponto c) {this.gps = c;}

    /**
     * Introduz password
     * @param c Password
     */
    public void setPassword (String c) { this.password = c; }

    /**
     * Introduz encomendas
     * @param encomendas Encomendas
     */
    public void setEncomendas(TreeMap<String, EncDistr> encomendas) {
        TreeMap<String, EncDistr> copy = new TreeMap<>();
        for (Map.Entry<String, EncDistr> entry : encomendas.entrySet()) {
           copy.put(entry.getKey(), entry.getValue().clone());
        }
        this.encomendas = copy;
    }

    /**
     * Introduz email
     * @param c Email
     */
    public void setMail (String c) { this.mail = c; }

    /**
     * Método equals
     * @param o RegistoTULV
     * @return true se os objetos forem iguais ou false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RegistoTULV that = (RegistoTULV) o;
        return Objects.equals(codigo, that.codigo) &&
                Objects.equals(nome, that.nome) &&
                Objects.equals(gps, that.gps) &&
                Objects.equals(password, that.password) &&
                Objects.equals(encomendas, that.encomendas) &&
                Objects.equals(mail,that.mail);
    }

    /**
     * Método clone
     * @return Cópia de um objeto da classe RegistoTULV
     */
    public RegistoTULV clone () {
        return new RegistoTULV(this);
    }

    /**
     * Método toString
     * @return String com informações de um objeto RegistoTULV
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Password: ").append(this.password).append("\n")
                .append("Email: ").append(this.mail).append("\n")
                .append("Código: ").append(this.codigo).append("\n")
                .append("Nome: ").append(this.nome).append("\n")
                .append("Ponto: ").append(this.gps).append("\n")
                .append("Encomendas: ").append(this.encomendas);
        return sb.toString();
    }

    /**
     * Introduz um objeto EncDistr no map de encomendas
     * @param e EncDistr
     */
    public void inserirEncomenda (EncDistr e) {
        this.encomendas.put(e.getEncomenda().getCodEncomenda(),e);
    }

    /**
     * Devolve o número de encomendas
     * @return Número de encomendas
     */
    public int getEncsSize() {
        return this.encomendas.size();
    }

    /**
     * Devolve lista de encomendas em estado "Pendente"
     * @return Lista de encomendas em estado "Pendente"
     */
    public List<Encomenda> getEncPendentes () {
        return this.encomendas.values().stream().filter(e -> e.getEncomenda().getEstado().equals("Pendente"))
                .sorted(new ReverseComparatorData()).map(EncDistr::getEncomenda).collect(Collectors.toList());
    }

    /**
     * Altera o estado de uma encomenda no map de encomendas
     * @param e Encomenda com estado alterado
     */
    public void alteraEstadoEncomenda (Encomenda e) {
        EncDistr n = this.encomendas.get(e.getCodEncomenda());
        n.setEncomenda(e);
        this.encomendas.put(e.getCodEncomenda(), n);
    }
}
