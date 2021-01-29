import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que faz o tratamento de dados de todos os registos efetuados na aplicação
 */
public class Registo implements Serializable {
    private static int codigoU = 0;
    private static int codigoV = 0;
    private static int codigoT = 0;
    private static int codigoL = 0;
    private TreeMap<String,RegistoTULV> registos;

    /**
     * Construtor vazio
     */
    public Registo() {
        this.registos = new TreeMap<>();
    }

    /**
     * Construtor com argumentos
     * @param l TreeMap de registos TULV
     */
    public Registo(TreeMap<String, RegistoTULV> l) {
        this.registos = new TreeMap<>();
        for(Map.Entry<String,RegistoTULV> u: l.entrySet()){
            this.registos.put(u.getKey(),u.getValue().clone());
        }
    }

    /**
     * Construtor com um único parâmetro
     * @param r Registo
     */
    public Registo(Registo r) {
        this.registos = new TreeMap<>();
        for(Map.Entry<String,RegistoTULV> u: r.registos.entrySet()){
            this.registos.put(u.getKey(),u.getValue().clone());
        }
    }

    /**
     * Método que retorna as lojas dos registos, como string
     * @return String de lojas
     */
    public String getLojas () {
        StringBuilder sb = new StringBuilder();
        sb.append("\n");
        ArrayList<RegistoTULV>  aux = (ArrayList<RegistoTULV>) this.registos.values().stream().filter(x -> x instanceof Loja).map(RegistoTULV::clone).collect(Collectors.toList());
        for(RegistoTULV r: aux) {
            sb.append(r.getCodigo()).append(" - ").append(r.getNome()).append("\n");
        }
        return sb.toString();
    }

    /**
     * Método que retorna uma cópia dos registos
     * @return clone de Registos
     */
    public TreeMap<String,RegistoTULV> getRegistos() {
        TreeMap<String, RegistoTULV> res = new TreeMap<>();
        for(Map.Entry<String,RegistoTULV> u: this.registos.entrySet()){
            res.put(u.getKey(),u.getValue().clone());
        }
        return res;
    }

    /**
     * Método que redefine os registos
     * @param r TreeMap que corresponde aos registos que pretendemos definir
     */
    public void setRegistos(TreeMap<String,RegistoTULV> r) {
        this.registos = new TreeMap<>();
        for(Map.Entry<String,RegistoTULV> u: r.entrySet()){
            this.registos.put(u.getKey(),u.getValue().clone());
        }
    }

    /**
     * Método equals
     * @param o Objeto que neste caso corresponde a um Registo
     * @return boolean que indica true ou false
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Registo registo = (Registo) o;
        return Objects.equals(getRegistos(), registo.getRegistos());
    }

    /**
     * Método que, dado um determinado registoTULV e um char que indica o seu tipo, adiciona o registo resultante ao conjunto de todos os registos
     * @param u Registo TULV
     * @param c Char com o "tipo" de RegistoTULV
     */
    public void adicionaRegisto (RegistoTULV u, char c){
        if (c =='u') {
            while(this.registos.containsKey("u"+codigoU)) codigoU++;
            u.setCodigo("u"+codigoU);
        }
        else if (c =='v') {
            while(this.registos.containsKey("v"+codigoV)) codigoV++;
            u.setCodigo("v"+codigoV);
        }
        else if (c =='t') {
            while(this.registos.containsKey("t"+codigoT)) codigoT++;
            u.setCodigo("t"+codigoT);
        }
        else if (c == 'l') {
            while(this.registos.containsKey("l"+codigoL)) codigoL++;
            u.setCodigo("l"+codigoL);
        }
        this.registos.put(u.getCodigo(),u.clone());
    }

    /**
     * Méotodo toString
     * @return String que corresponde à classe numa versão "legível"
     */
    public String toString() {
        return "Registo{" +
                "registos = " + registos +
                '}';
    }

    /**
     * Método clone
     * @return cópia do Registo
     */
    public Registo clone(){
       return new Registo(this);
    }

    /**
     * Método que guarda o estado atual da aplicação
     * @param fich que corresponde ao ficheiro onde se guarda a informação
     * @throws IOException exceção
     */
    public void guardaEstado(String fich) throws IOException {
        FileOutputStream fos = new FileOutputStream(fich);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }

    /**
     * Método que carrega do ficheiro o estado anteriormente gravado
     * @param fich ficheiro
     * @return Registo com a informação do estado que foi carregado
     * @throws IOException exceção
     * @throws ClassNotFoundException exceção
     */
    public static Registo carregaEstado(String fich) throws IOException, ClassNotFoundException {
        FileInputStream fis = new FileInputStream(fich);
        ObjectInputStream ois = new ObjectInputStream(fis);
        Registo r = (Registo) ois.readObject();
        ois.close();
        return r;
    }

    /**
     * Método que adiciona uma encomenda a um Utilizador e a uma Loja
     * @param e Encomenda que é para adicionar
     */
    public void adicionaEncomendaLU(Encomenda e){
        String codU = e.getCodUtilizador();
        Utilizador u = (Utilizador) this.registos.get(codU);
        u.inserirEncomenda(new EncDistr(e,""));
        this.registos.put(u.getCodigo(),u);
        String codL = e.getCodLoja();
        RegistoTULV l = this.registos.get(codL);
        l.inserirEncomenda(new EncDistr(e,""));
        this.registos.put(l.getCodigo(),l);
    }

    /**
     * Método que devolve a transportadora mais rápida de acordo com os estabelecimentos que se estão a considerar
     * @param u Utilizador
     * @param l Loja
     * @param fM flag Médica
     * @return Transportadora que efetuará a entrega mais rapidamente
     */
    public Transportadora transpFaster (Utilizador u, Loja l, int fM) {
        Transportadora t = null;
        double min = 999999999;
        ArrayList<RegistoTULV> aux = (ArrayList<RegistoTULV>) registos.values().stream().filter(x -> x instanceof Transportadora).collect(Collectors.toList());
        for(RegistoTULV r : aux) {
            Transportadora q = (Transportadora) r;
            if (fM == 2){
                if(q.inRaio(l) && q.aceitoTransporteMedicamentos()) {
                    double x = q.calculoTempo(l, u);
                    if (x < min) {
                        min = x;
                        t = q;
                    }
                }
            }
            else {
                if(q.inRaio(l)) {
                    double x = q.calculoTempo(l, u);
                    if (x < min) {
                        min = x;
                        t = q;
                    }
                }
            }
        }
        return t;
    }

    /**
     * Método que devolve o voluntário mais rápido de acordo com os estabelecimentos que se estão a considerar
     * @param u Utilizador
     * @param l Loja
     * @param fM flag Médica
     * @return Voluntário que efetuará a entrega mais rapidamente
     */
    public Voluntario volFaster (Utilizador u, Loja l, int fM) {
        Voluntario v = null;
        double min = 999999999;
        ArrayList<RegistoTULV> aux = (ArrayList<RegistoTULV>) registos.values().stream().filter(x -> x instanceof Voluntario).collect(Collectors.toList());
        for(RegistoTULV r : aux) {
            Voluntario q = (Voluntario) r;
            if (fM == 2){
                if(q.inRaio(l) && q.aceitoTransporteMedicamentos()) {
                    double x = q.calculoTempo(l, u);
                    if (x < min) {
                        min = x;
                        v = q;
                    }
                }
            }
            else {
                if(q.inRaio(l)) {
                    double x = q.calculoTempo(l, u);
                    if (x < min) {
                        min = x;
                        v = q;
                    }
                }
            }
        }
        return v;
    }

    /**
     * Método que gera um código (novo e que não exista ainda) para um determinado produto
     * @param codL referência para certos casos que complementa a estratégia
     * @return String código de produto gerado
     */
    public String geraCodProduto (int codL) {
        List<String> w = new ArrayList<>();
        int cod = 0;
        ArrayList<RegistoTULV> aux = (ArrayList<RegistoTULV>) this.registos.values().stream().filter(x -> x instanceof Utilizador).map(RegistoTULV::clone).collect(Collectors.toList());
        for (RegistoTULV ut : aux) {
            TreeMap<String, EncDistr> fg = ut.getEncomendas();
            for (EncDistr e : fg.values()) {
                Map<String, LinhaEncomenda> s = e.getEncomenda().getLinha();
                for (LinhaEncomenda li : s.values()) {
                    w.add(li.getCodProd());
                }
            }
        }
        if (!w.contains("p" + codL)) return "p" + codL;
        cod = codL;
        while (w.contains("p" + cod)) cod++;
        return "p" + cod;
    }

    /**
     * Método que gera um código (novo e que não exista ainda) para um determinado produto
     * @return String código de produto gerado
     */
    public String geraCodEncomenda () {
        List<String> w = new ArrayList<>();
        int cod = 0;
        ArrayList<RegistoTULV> aux = (ArrayList<RegistoTULV>) this.registos.values().stream().filter(x -> x instanceof Utilizador).map(RegistoTULV::clone).collect(Collectors.toList());
        for (RegistoTULV ut : aux) {
            Utilizador a = (Utilizador) ut;
            for (EncDistr e : a.getEncomendas().values()) {
                w.add(e.getEncomenda().getCodEncomenda());
            }
        }
        while (w.contains("e" + cod)) cod++;
        return "e" + cod;
    }

    /**
     * Métodd que, dados 3 registos TULV diferentes, atualiza os dados dos registos
     * @param v RegistoTULV
     * @param t RegistoTULV
     * @param l RegistoTULV
     */
    public void atualizaRegistos(RegistoTULV v, RegistoTULV t, RegistoTULV l) {
        this.registos.put(v.getCodigo(),v);
        this.registos.put(t.getCodigo(),t);
        this.registos.put(l.getCodigo(),l);
    }

    /**
     * Método que determina se um email já existe nos registos
     * @param email Email a verificar
     * @return boolean (true caso exista, false caso não exista)
     */
    public boolean verificaEmail(String email) {
        List<String> mails = this.registos.values().stream().map(RegistoTULV::getMail).collect(Collectors.toList());
        return mails.contains(email);
    }

    /**
     * Método que determina os top 10 utilizadores da app. Aqueles que mais encomendas realizaram
     * @return String com o top 10 dos utilizadores
     */
    public String top10Utilizadores() {
        int f = 0;
        List<RegistoTULV> aux;
        StringBuilder sb = new StringBuilder();
        aux = this.registos.values().stream().filter(x -> x instanceof Utilizador).sorted(new ComparatorQuantidadeEnc()).collect(Collectors.toList());
        Iterator<RegistoTULV> it = aux.iterator();
        while (it.hasNext() && f<10) {
            Utilizador u = (Utilizador) it.next();
            sb.append(u.getNome()).append(" (").append(u.getCodigo()).append(") ").append("-> ").append(u.getEncsSize()).append(" encomendas").append("\n");
            f++;
        }
        return sb.toString();
    }

    /**
     * Método que determina as top 10 transportadoras da app. Aqueles que mais kms realizaram
     * @return String com o top 10 das transportadoras
     */
    public String top10Transportadoras() {
        int f = 0;
        List<RegistoTULV> aux;
        StringBuilder sb = new StringBuilder();
        aux = this.registos.values().stream().filter(x -> x instanceof Transportadora).sorted(new ComparatorNumKm()).collect(Collectors.toList());
        Iterator<RegistoTULV> it = aux.iterator();
        while (it.hasNext() && f<10) {
            Transportadora u = (Transportadora) it.next();
            sb.append(u.getNome()).append(" (").append(u.getCodigo()).append(") ").append("-> ").append(u.getNumKm()).append(" km").append("\n");
            f++;
        }
        return sb.toString();
    }

    /**
     * Método que determina se um RegistoTULV já existe nos registos
     * @param cod com código do RegistoTULV
     * @return boolean (true caso exista, false caso não exista)
     */
    public boolean existeRegisto(String cod){
        RegistoTULV u = this.registos.get(cod);
        return u != null;
    }

    /**
     * Método que determina se uma password se encontra associada a um dado nome
     * @param nome nome
     * @param pass password
     * @return boolean
     */
    public boolean verificaPass(String nome, String pass){
        return this.registos.get(nome).getPassWord().equals(pass);
    }

    /**
     * Método que, para um determinado nome, devolve as suas encomendas
     * @param nome nome
     * @return Lista das encomendas associadas a um determinado nome
     */
    public List<EncDistr> devolveMyEncs(String nome){
        return new ArrayList<>(this.registos.get(nome).getEncomendas().values());
    }

    /**
     * Método que, dado um nome, devolve o registoTULV a ele associado
     * @param nome nome
     * @return RegistoTULV
     */
    public RegistoTULV getRegistoTULV (String nome){
        return this.registos.get(nome);
    }

    /**
     * Método que insere um RegistoTULV nos registos
     * @param r RegistoTULV
     */
    public void putRegistoTULV (RegistoTULV r){
        this.registos.put(r.getCodigo(),r);
    }

    /**
     * Método que aplica a mudança de estado nos registos
     * @param t código transportadora
     * @param e encomenda
     */
    public void alteraEstadoEncomenda (String t, Encomenda e) {
        RegistoTULV ut = this.registos.get(e.getCodUtilizador());
        ut.alteraEstadoEncomenda(e);
        this.registos.put(e.getCodUtilizador(),ut);
        RegistoTULV tt = this.registos.get(t);
        tt.alteraEstadoEncomenda(e);
        this.registos.put(t,tt);
        RegistoTULV l = this.registos.get(e.getCodLoja());
        l.alteraEstadoEncomenda(e);
        this.registos.put(e.getCodLoja(),l);
    }

    /**
     * Método que adiciona uma encomenda por avaliar a um determinado utilizador
     * @param e EncDistr
     */
    public void adicionaMyEncAv (EncDistr e) {
        Utilizador u = (Utilizador) this.registos.get(e.getEncomenda().getCodUtilizador());
        u.adicionaMyEncAv(e);
        this.registos.put(u.getCodigo(), u);
    }

    /**
     * Método que faz com que uma dada encomenda seja aceite
     * @param e string
     */
    public void aceitaEncomendaLU (String e) {
        for(RegistoTULV r: this.registos.values()) {
            if (r instanceof Loja || r instanceof Utilizador) {
                TreeMap<String,EncDistr> l = r.getEncomendas();
                if(l.containsKey(e)) {
                    Encomenda en = l.get(e).getEncomenda();
                    en.aceitaEncomenda();
                    r.alteraEstadoEncomenda(en);
                    this.registos.put(r.getCodigo(),r);
                }
            }
        }
    }
}
