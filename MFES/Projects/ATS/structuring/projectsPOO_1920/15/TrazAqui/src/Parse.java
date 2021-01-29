import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;


public class Parse implements Serializable{

    private Map<String, Utilizador> utilizadors = new HashMap<>();
    private Map<String, Voluntario> voluntarios = new HashMap<>();
    private Map<String, Loja> lojas = new HashMap<>();
    private Map<String, Encomenda> encomendas = new HashMap<>();
    private Map<String, Transportadora> transportadoras = new HashMap<>();
    private Map<String, Encomenda> aceite = new HashMap<>();
    private List<LinhaEncomenda> produtos = new ArrayList<>();
    private Map<String, List<String>> pendentes = new HashMap<>();


    public void parse(Parse p){
        List<String> linhas = lerFicheiro("src/info.txt"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":",3);
            // -- entra no switch conforme o valor de linhaPartida[0]
            switch(linhaPartida[0]){
                case "Utilizador":
                    // -- passa a restante linha para a função
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    utilizadors.put(u.getCodUtilizador(), u.clone());
                    break;
                case "Voluntario":
                    Voluntario a = parseVoluntario(linhaPartida[1]); // criar um Utilizador
                    voluntarios.put(a.getCodUtilizador(), a.clone());
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    lojas.put(l.getCodUtilizador(), l.clone());
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    encomendas.put(e.getCodEncomenda(), e.clone());
                    break;
                case "Transportadora":
                    Transportadora t = parseTransportador(linhaPartida[1]);
                    transportadoras.put(t.getCodUtilizador(), t.clone());
                    break;
                case "Aceite":
                    e = parseAceite(linhaPartida[1]);
                    aceite.put(e.getCodEncomenda(), e.clone());
                    break;
                //...
                default:
                    //System.out.println("Linha inválida: " + linhaPartida[1]);
                    break;
            }
        }
        System.out.println("done!");
    }

    public Transportadora parseTransportador(String input) {
        String[] campos = input.split(",");

        String codUtilizador = campos[0];
        String nomeEmpresa = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double precoKm = Double.parseDouble(campos[6]);

        return new Transportadora(codUtilizador, nomeEmpresa, gpsx, gpsy, nif, raio, precoKm);
    }

    public Encomenda parseEncomenda(String input) {
        String[] campos = input.split(",");
        int size = campos.length;

        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        float peso = Float.parseFloat(campos[3]);
        // -- lê os 4 primeiros valores da encomenda
        ArrayList<LinhaEncomenda> linhaEncomendas = new ArrayList<>();
        String codProduto;
        String descricao;
        float quantidade;
        float valorUnitario;
        int i = 4;
        // -- a cada 4 campos novos temos uma nova linha, assim sendo
        // -- podemos fazer a seguinte iteração, pois sabemos que de 4 em 4
        // -- campos temos de novo o campo codProduto e subconsequentemente
        // -- uma linha de encomenda nova
        while (i<size){
            codProduto = campos[i];
            i++;
            descricao = campos[i];
            i++;
            quantidade = Float.parseFloat(campos[i]);
            i++;
            valorUnitario = Float.parseFloat(campos[i]);
            i++;
            linhaEncomendas.add(new LinhaEncomenda(codProduto, descricao, quantidade, valorUnitario));
        }

        return new Encomenda(codEncomenda, codUtilizador, codLoja, peso, linhaEncomendas);
    }

    public Utilizador parseUtilizador(String input){

        String[] campos = input.split(",");
        String nome = campos[1];
        String codUtilizador = campos[0];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);

        return new Utilizador(codUtilizador, nome, gpsy, gpsx);

    }

    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String nome = campos[1];
        String codUtilizador = campos[0];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        float raio = Float.parseFloat(campos[4]);

        return new Voluntario(codUtilizador, nome, gpsx, gpsy, raio);
    }

    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Loja(codLoja, nomeLoja, gpsx, gpsy);
    }

    public void parseProdutos(Map<String, Encomenda> encomendas){
        for(Map.Entry<String, Encomenda> e : encomendas.entrySet()){
            List<LinhaEncomenda> linhaEncomendas = e.getValue().getLinhaEncomendas();
            for(LinhaEncomenda le : linhaEncomendas){
                if (semRepetidos(le)){
                    produtos.add(le.clone());
                }
            }
        }
        produtos.sort(Comparator.comparing(LinhaEncomenda::getDescricao));
        setProdutos(produtos);
    }

    public boolean semRepetidos(LinhaEncomenda produto){
        for(LinhaEncomenda e : this.produtos ){
            if(e.getCodProduto().equals(produto.getCodProduto())){
                return false;
            }
        }
        return true;
    }

    public Encomenda parseAceite(String input){
        String[] campos = input.split(",");
        Encomenda e = this.encomendas.get(campos[0]).clone();
        return e;
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }

    public Map<String, Utilizador> getUtilizadors() {
        Map<String, Utilizador> utilizadores = new HashMap<>();
        for(Map.Entry<String, Utilizador> u : this.utilizadors.entrySet()){
            utilizadores.put(u.getKey(), u.getValue().clone());
        }
        return utilizadores;
    }

    public Map<String, Voluntario> getVoluntarios() {
        Map<String, Voluntario> volun = new HashMap<>();
        for(Map.Entry<String, Voluntario> u : this.voluntarios.entrySet()){
            volun.put(u.getKey(), u.getValue().clone());
        }
        return volun;
    }

    public Map<String, Loja> getLojas() {
        Map<String, Loja> loj = new HashMap<>();
        for(Map.Entry<String, Loja> u : this.lojas.entrySet()){
            loj.put(u.getKey(), u.getValue().clone());
        }
        return loj;
    }

    public Map<String, Encomenda> getEncomendas() {
        Map<String, Encomenda> enco = new HashMap<>();
        for(Map.Entry<String, Encomenda> u : this.encomendas.entrySet()){
            enco.put(u.getKey(), u.getValue().clone());
        }
        return enco;
    }

    public Map<String, Transportadora> getTransportadoras() {
        Map<String, Transportadora> tr = new HashMap<>();
        for(Map.Entry<String, Transportadora> u : this.transportadoras.entrySet()){
            tr.put(u.getKey(), u.getValue().clone());
        }
        return tr;
    }

    public Map<String, Encomenda> getAceite() {
        Map<String, Encomenda> ac = new HashMap<>();
        for(Map.Entry<String, Encomenda> u : this.aceite.entrySet()){
            ac.put(u.getKey(), u.getValue().clone());
        }
        return ac;
    }

    public Map<String, List<String>> getPendentes() {
        Map<String, List<String>> ac = new HashMap<>();
        for(Map.Entry<String, List<String>> u : this.pendentes.entrySet()){
            ac.put(u.getKey(), u.getValue());
        }
        return ac;
    }

    public void setPendentes(Map<String, List<String>> pendentes) {
        this.pendentes = new HashMap<>();
        for(String s: pendentes.keySet()){
            this.pendentes.put(s,pendentes.get(s));
        }
    }


    public void setUtilizadors(Map<String, Utilizador> utilizadors) {
        this.utilizadors = new HashMap<>();
        for(String s: utilizadors.keySet()){
            this.utilizadors.put(s,utilizadors.get(s).clone());
        }
    }

    public void setVoluntarios(Map<String, Voluntario> voluntarios) {
        this.voluntarios = new HashMap<>();
        for (String s: voluntarios.keySet()){
            this.voluntarios.put(s,voluntarios.get(s).clone());
        }
    }

    public void setLojas(Map<String, Loja> lojas) {
        this.lojas = new HashMap<>();
        for (String s: lojas.keySet()){
            this.lojas.put(s,lojas.get(s).clone());
        }
    }

    public void setEncomendas(Map<String, Encomenda> encomendas) {
        this.encomendas = new HashMap<>();
        for (String s: encomendas.keySet()){
            this.encomendas.put(s,encomendas.get(s).clone());
        }
    }

    public void setTransportadoras(Map<String, Transportadora> transportadoras) {
        this.transportadoras = new HashMap<>();
        for (String s: transportadoras.keySet()){
            this.transportadoras.put(s,transportadoras.get(s).clone());
        }
    }

    public void setAceite(Map<String, Encomenda> aceite) {
        this.aceite = new HashMap<>();
        for (String s: aceite.keySet()){
            this.aceite.put(s,aceite.get(s).clone());
        }
    }

    public List<LinhaEncomenda> getProdutos() {
        return this.produtos.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());
    }

    public void setProdutos(List<LinhaEncomenda> produtos){
        this.produtos = produtos.stream().map(LinhaEncomenda:: clone).collect(Collectors.toList());
    }

    /**
     * User faz login na aplicação
     * @param p
     * @param idUser
     * @param password
     * @return
     */
    public Utilizador loginUtilizador(Parse p, String idUser, String password) {
        if (p.getUtilizadors().containsKey(idUser)) {
            if(p.getUtilizadors().get(idUser).getPassword().equals(password)){
                return new Utilizador(p.getUtilizadors().get(idUser).clone());
            }
        }
        if (p.getLojas().containsKey(idUser)) {
            if(p.getLojas().get(idUser).getPassword().equals(password)){
                return new Utilizador(p.getLojas().get(idUser).clone());
            }
        }
        if (p.getVoluntarios().containsKey(idUser)) {
            if(p.getVoluntarios().get(idUser).getPassword().equals(password)){
                return new Utilizador(p.getVoluntarios().get(idUser).clone());
            }
        }
        if (p.getTransportadoras().containsKey(idUser)) {
            if(p.getTransportadoras().get(idUser).getPassword().equals(password)){
                return new Utilizador(p.getTransportadoras().get(idUser).clone());
            }
        }
        return null;
    }

    /**
     * Adiciona um utilizador à aplicação
     *
     */
    public void addUser(Utilizador user) throws utilizadorJaExiste {
        if(user instanceof Utilizador){
            if(utilizadors.get(user.getCodUtilizador()) != null) throw new utilizadorJaExiste("Utilizador já existe!");
            utilizadors.put(user.getCodUtilizador(),user.clone());
        }
        if(user instanceof Voluntario){
            if(voluntarios.get(user.getCodUtilizador()) != null) throw new utilizadorJaExiste("Utilizador já existe!");
            voluntarios.put(user.getCodUtilizador(),(Voluntario) user.clone());
        }
        if(user instanceof Loja){
            if(lojas.get(user.getCodUtilizador()) != null) throw new utilizadorJaExiste("Utilizador já existe!");
            lojas.put(user.getCodUtilizador(),(Loja) user.clone());
        }
        if(user instanceof Transportadora){
            if(transportadoras.get(user.getCodUtilizador()) != null) throw new utilizadorJaExiste("Utilizador já existe!");
            transportadoras.put(user.getCodUtilizador(),(Transportadora) user.clone());
        }
    }

    public boolean contem(String codProduto){
        boolean contem = false;
        for(LinhaEncomenda e : this.getProdutos()){
            if(e.getCodProduto().equals(codProduto)){
                return true;
            }
        }
        return contem;
    }


    /**
     * verifica o codigo do produto do ficheiro se é igual ao codigo do produto que o user seleciona
     * @param codProd
     * @return
     */
    public LinhaEncomenda descProd(String codProd){
        for(LinhaEncomenda e : this.produtos){
            if(e.getCodProduto().equals(codProd)){
                return e;
            }
        }
        return null;
    }

    /**
     * Verifica as encomendas efetuadas por parte do user logado
     * @param user
     */
    public void encomendasEfetuadas(String user) {
        List<Encomenda> encomendas = new ArrayList<>();
        for(Encomenda e : this.encomendas.values()){
            if(e.getCodUtilizador().equals(user)){
                encomendas.add(e.clone());
            }
        }
        if(encomendas.size() == 0){
            System.out.println("Não tem encomendas efetuadas!");
        }else{
            System.out.println(encomendas.toString());
        }
    }

    /**
     * Altera a disponibilidade do voluntario para recolher uma encomenda
     * @param user
     */
    public void alteraDisponibilidade_V(Voluntario user){
        user.setDisponibilidade(!user.getDisponibilidade());
    }

    /**
     * Altera a disponibilidade da transportadora para recolher uma encomenda
     * @param user
     */
    public void alteraDisponibilidade_T(Transportadora user){
        user.setDisponibilidade(!user.getDisponibilidade());
    }

    public void aceitoTransporteMedicamentos_T(Transportadora user) {
        user.setAceitoTransporteMedicamentos(!user.getAceitoTransporteMedicamentos());
    }

    public void aceitoTransporteMedicamentos_V(Voluntario user){
        user.setAceitaMedicamentos(!user.getAceitaMedicamentos());
    }

    /**
     * Voluntários ou empresa tranpostadora recolhem um encomenda no seu raio!
     * @param user
     */
    public void recolherEncomenda(Utilizador user){
        List<Encomenda> encomendasPorRecolher = new ArrayList<>();
        for(Map.Entry<String, Encomenda> e : this.encomendas.entrySet()){
            if(!aceite.containsKey(e.getKey())){
                encomendasPorRecolher.add(e.getValue().clone());
            }
            for(Encomenda f : encomendasPorRecolher) {
                Utilizador l = this.lojas.get(f.getCodLoja()).clone();
                Utilizador u = this.utilizadors.get(f.getCodUtilizador()).clone();
                if (user instanceof Transportadora) {
                    if (user.distEntre2Pts(user, l) > ((Transportadora) user).getRaio() && user.distEntre2Pts(user, u) > ((Transportadora) user).getRaio()){
                        encomendasPorRecolher.remove(f);
                    }
                }
                if(user instanceof Voluntario){
                    if(user.distEntre2Pts(user,u) > ((Voluntario) user).getRaio() && user.distEntre2Pts(user,l) > ((Voluntario) user).getRaio()){
                            encomendasPorRecolher.remove(f);
                    }
                }
            }
        }
        if(encomendasPorRecolher.size() == 0){
            System.out.println("Não tem encomendas no seu raio. ");
        }
        System.out.println(encomendasPorRecolher);
    }

    /**
     * Método que adiciona encomendas aceites à primeira transportadora ou voluntário que estejam dentro do seu raio
     */
    public void adiciona_encomendas_aceites(){
        for(Map.Entry<String, Encomenda> e : this.aceite.entrySet()){
            boolean i = false;
            Loja l = this.lojas.get(e.getValue().getCodLoja()).clone();
            Utilizador u = this.utilizadors.get(e.getValue().getCodUtilizador()).clone();
            for (Transportadora t : this.transportadoras.values()){
                if (u.distEntre2Pts(u, l) > t.getRaio() && u.distEntre2Pts(u, t) > t.getRaio()){
                    List<Encomenda> aux = t.getHistorico();
                    aux.add(e.getValue());
                    t.setHistorico(aux);
                    i = true;
                    break;
                }
            }
            if (!i){
                for (Voluntario v : this.voluntarios.values()){
                    if (u.distEntre2Pts(u, l) > v.getRaio() && u.distEntre2Pts(u, v) > v.getRaio()){
                        List<Encomenda> aux = v.getHistorico();
                        aux.add(e.getValue());
                        v.setHistorico(aux);
                        break;
                    }
                }
            }
        }
    }

    /**
     * Método que classifica o voluntário ou transportadora mediante o serviço de entrega
     * @param user
     * @return
     */
    public List<String> classifica(Utilizador user){
        List<String> aux = new ArrayList<>();
        for (Transportadora t : this.transportadoras.values()){
            for (Encomenda e : t.getHistorico()){
                if (e.getCodUtilizador().equals(user.getCodUtilizador()) && e.getPorClassificar()){
                    aux.add(t.getCodUtilizador());

                }
            }
        }
        for (Voluntario v : this.voluntarios.values()){
            for (Encomenda i : v.getHistorico()){
                if (i.getCodUtilizador().equals(user.getCodUtilizador()) && i.getPorClassificar()){
                    aux.add(v.getCodUtilizador());
                }
            }
        }
        return aux;
    }

    /**
     * Verifica as encomendas que já foram classificadas por parte do user logado
     * @param user
     * @param codigo
     */
    public void encomendas_Classificadas(Utilizador user, String codigo){
        for (Transportadora t : this.transportadoras.values()){
            if (t.getCodUtilizador().equals(codigo)){
                List<Encomenda> aux = (t.getHistorico());
                for (Encomenda e : aux){
                    if (e.getCodUtilizador().equals(user.getCodUtilizador()) && e.getPorClassificar()){
                        e.setPorClassificar(false);
                        t.setHistorico(aux);
                    }
                }
            }
        }
        for (Voluntario v : this.voluntarios.values()){
            if (v.getCodUtilizador().equals(codigo)){
                List<Encomenda> aux = (v.getHistorico());
                for (Encomenda e : aux){
                    if (e.getCodUtilizador().equals(user.getCodUtilizador()) && e.getPorClassificar()){
                        e.setPorClassificar(false);
                        v.setHistorico(aux);
                    }
                }
            }
        }
    }

    /**
     * Adiciona numa lista auxiliar as encomendas aceites de cada user
     */
    public void add_hist_user(){
        for(Encomenda e : this.aceite.values()){
            for (Utilizador u : this.utilizadors.values()){
                if(e.getCodUtilizador().equals(u.getCodUtilizador())){
                    List<Encomenda> aux = new ArrayList<>(u.getUser_historico());
                    aux.add(e);
                    u.setUser_historico(aux);
                }
            }
        }
    }

    /**
     * Método que mostra os 10 utilizadores que mais usam a app
     * @return
     */
    public Set<Utilizador> top10() {
        Set<Utilizador> aux = new TreeSet<>(new ComparatorTop10Utilizadores());
        for (Utilizador u : this.utilizadors.values()){
            aux.add(u.clone());
            if (aux.size() == 10){
                break;
            }
        }
    return aux;
    }

    /**
     * Calcula a distância entre a loja e a tranportadora e entre a loja e o user e adiciona as distâncias
     * @param l
     * @param t
     * @param u
     * @return
     */
    public double calcula_dist(Loja l, Transportadora t, Utilizador u){
        return t.distEntre2Pts(l,t) + t.distEntre2Pts(l,u);
    }

    /**
     * Calcula os KM percorridos de cada transportadora
     */
    public void calcula_KM(){
        double acumula = 0;
        for(Transportadora t : this.transportadoras.values()){
            for (Encomenda e : t.getHistorico()){
                acumula += calcula_dist(this.lojas.get(e.getCodLoja()), t, this.utilizadors.get(e.getCodUtilizador()));
            }
        t.setKm_Percorridos(acumula);
        acumula = 0;
        }
    }

    /**
     * Método que mostra as 10 transportadoras que mais usam a app(em KM feitos)
     * @return
     */
    public Set<Transportadora> top10_Tranportadoras(){
        Set<Transportadora> aux = new TreeSet<>(new ComparatorTop10Transportadoras());
        for (Transportadora t : this.transportadoras.values()){
            aux.add(t.clone());
            if (aux.size() == 10){
                break;
            }
        }
    return aux;
    }


    /**
     * metodo que grava as alteraçoes do ficheiro
     */
    public void gravaEmFicheiro(){
        try {
        FileOutputStream fos = new FileOutputStream("src/data.txt");
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }

    /**
     * metodo que le o ficheiro
     * @return
     */
    public static Parse leFicheiro() {
        Parse mParse = null;
        try {
            FileInputStream fis = new FileInputStream("src/data.txt");
            ObjectInputStream ois = new ObjectInputStream(fis);
            mParse = (Parse) ois.readObject();
            System.out.println("Dados Lidos");
        } catch (FileNotFoundException e) {
            System.out.println("Ficheiro de carregamento de dados não existe");
        } catch (Exception e) {
            //System.out.println(e.getMessage());
        }
        if (mParse == null) mParse = new Parse();
        return mParse;
    }


    /**
     * define o preço da encomenda que a transportadora estabelece
     * @param t
     * @param e
     * @return
     */

    public double precoEnc(Transportadora t, Encomenda e){
        double x = t.distEntre2Pts(t, this.lojas.get(e.getCodLoja()));
        double y = this.lojas.get(e.getCodLoja()).distEntre2Pts(this.lojas.get(e.getCodLoja()), this.utilizadors.get(e.getCodUtilizador()));
        double z = (x + y) * t.getPrecoKm() * e.getPeso();
        return z;
    }

    /**
     * total faturado por parte da transportadora num determinado tempo
     * @param t
     * @param f
     * @param l
     * @return
     */
    public double totalFaturado(Transportadora t, LocalDateTime f, LocalDateTime l){
        double x = 0;
        for(Encomenda e : t.getHistorico()){
            if (e.getData().isAfter(f.toLocalDate()) && e.getData().isBefore(l.toLocalDate())){
                x += precoEnc(t, e);
            }
        }
        return x;
    }


    /**
     * guarda numa list as encomendas efetuadas pela loja
     */
    public void historico_Lojas() {
        for(Encomenda e : this.encomendas.values()){
            for (Loja l : this.lojas.values()){
                if(e.getCodLoja().equals(l.getCodUtilizador())){
                    List<Encomenda> aux = new ArrayList<>(l.getHist_Loja());
                    aux.add(e);
                    l.setHist_Loja(aux);
                }
            }
        }
    }

    /**
     * historico das encomendas da loja logada
     * @param l
     */
    public void lojaEncomendas(Loja l) {
        List<Encomenda> encomendas = new ArrayList<>();
        for(Encomenda e : l.getHist_Loja()){
            if(e.getCodLoja().equals(l.getCodUtilizador())){
                encomendas.add(e.clone());
            }
        }
        if(encomendas.size() == 0){
            System.out.println("Não tem encomendas efetuadas!");
        }else{
            System.out.println(encomendas.toString());
        }
    }

    /**
     *  a loja define o peso das suas encomendas pendentes
     * @param l
     * @return
     */
    public List<String>  defPeso(Loja l){
        List<String> aux = new ArrayList<>();
        for(Encomenda e : this.encomendas.values()){
            if (!this.aceite.containsKey(e.getCodEncomenda()) && e.getCodLoja().equals(l.getCodUtilizador())) {
                aux.add(e.getCodEncomenda());
            }
        }
        return aux;
    }

    /**
     * metodo que altera o peso da encomenda
     * @param i
     * @param defpeso
     */
    public void alteraPeso(String i, double defpeso){
        for(Encomenda e : this.encomendas.values()){
            if (e.getCodEncomenda().equals(i)){
                e.setPeso(defpeso);
            }
        }
    }

    /**
     * metodo que a encomenda passa para aceite
     * @param s
     */
    public void encAceite(String s){
        for (Encomenda e : this.encomendas.values()){
            if(e.getCodEncomenda().equals(s)){
                this.aceite.put(e.getCodEncomenda(),e.clone());
            }
        }
    }

    /**
     * adiciona as encomendas pendentes num hashmap do user logado
     * @param t
     * @param i
     */
    public void addPendentes(Transportadora t ,String i) {
        String e = this.encomendas.get(i).getCodEncomenda();
        if(this.pendentes.containsKey(e)){
            this.pendentes.get(e).add(t.getCodUtilizador());
        }else {
            this.pendentes.put(e,new ArrayList<>());
            this.pendentes.get(e).add(t.getCodUtilizador());
        }
    }

    /**
     * adiciona em uma lista as encomendas do user logado que estao pendentes
     * @param u
     * @return
     */

    public  List<Encomenda> encPorAceitar(Utilizador u) {
        List<Encomenda> aux = new ArrayList<>();
        for (Encomenda e : this.encomendas.values()) {
            if (e.getCodUtilizador().equals(u.getCodUtilizador()) && !this.aceite.containsKey(e.getCodEncomenda())) {
                aux.add(e.clone());
            }
        }
        return aux;
    }


}



