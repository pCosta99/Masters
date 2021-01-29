import java.io.*;
import java.lang.Math;
import java.lang.Character;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

import static java.lang.Integer.MAX_VALUE;

public class TrazAquiModel implements TrazAquiModelI, java.io.Serializable{
    // Metodos de instancia
    private Map<String, UtilizadorI> utilizadores;
    private Map<String, VoluntarioI> voluntarios;
    private Map<String, Empresa_TransportadoraI> empresas;
    private Map<String, LojaI> lojas;
    private Map<String, EncomendaI> encomendas;
    private Set<String> enc_aceites;
    private Map<String, LoginI> logins;

    /**
     * Construtores para objetos da classe TrazAqui
     */
    public TrazAquiModel(){
        this.utilizadores = new TreeMap<>();
        this.voluntarios = new TreeMap<>();
        this.empresas = new TreeMap<>();
        this.lojas = new TreeMap<>();
        this.encomendas = new TreeMap<>();
        this.enc_aceites = new HashSet<>();
        this.logins = new TreeMap<>();
    }

    public TrazAquiModel(TrazAquiModel t){
        this.utilizadores = t.getUtilizadores();
        this.voluntarios = t.getVoluntarios();
        this.empresas = t.getEmpresas();
        this.lojas = t.getLojas();
        this.encomendas = t.getEncomendas();
        this.enc_aceites = t.getEnc_aceites();
        this.logins = t.getLogins();
    }

    /**
     * Metodos gets e sets,
     * clone, equlas e toString
     */
    public Map<String, UtilizadorI> getUtilizadores(){
        Map<String, UtilizadorI> res = new TreeMap<>();
        for(UtilizadorI u: this.utilizadores.values())
            res.put(u.getCodigo(), u.clone());
        return res;
    }

    public void setUtilizadores(Map<String, UtilizadorI> utilizadores){
        this.utilizadores = new TreeMap<>();
        for(UtilizadorI u: utilizadores.values())
            this.utilizadores.put(u.getCodigo(), u.clone());
    }

    public Map<String, VoluntarioI> getVoluntarios(){
        Map<String, VoluntarioI> res = new TreeMap<>();
        for(VoluntarioI v: this.voluntarios.values())
            res.put(v.getCodigo(), v.clone());
        return res;
    }

    public void setVoluntarios(Map<String, VoluntarioI> voluntarios){
        this.voluntarios = new TreeMap<>();
        for(VoluntarioI v: voluntarios.values())
            this.voluntarios.put(v.getCodigo(), v.clone());
    }

    public Map<String, Empresa_TransportadoraI> getEmpresas(){
        Map<String, Empresa_TransportadoraI> res = new TreeMap<>();
        for(Empresa_TransportadoraI e: this.empresas.values())
            res.put(e.getCodigo(), e.clone());
        return res;
    }

    public void setEmpresas(Map<String,Empresa_TransportadoraI> empresas){
        this.empresas = new TreeMap<>();
        for(Empresa_TransportadoraI e: empresas.values())
            this.empresas.put(e.getCodigo(), e.clone());
    }

    public Map<String, LojaI> getLojas(){
        Map<String, LojaI> res = new TreeMap<>();
        for(LojaI l: this.lojas.values())
            res.put(l.getCodigo(), l.clone());
        return res;
    }

    public void setLojas(Map<String, LojaI> lojas){
        this.lojas = new TreeMap<>();
        for(LojaI l: lojas.values())
            this.lojas.put(l.getCodigo(), l.clone());
    }

    public Map<String, EncomendaI> getEncomendas(){
        Map<String, EncomendaI> res = new TreeMap<>();
        for(EncomendaI e: this.encomendas.values())
            res.put(e.getCodEncomenda(), e.clone());
        return res;
    }

    public void setEncomendas(Map<String, EncomendaI> encomendas){
        this.encomendas = new TreeMap<>();
        for(EncomendaI e: encomendas.values())
            this.encomendas.put(e.getCodEncomenda(), e.clone());
    }

    public Set<String> getEnc_aceites(){
        return new HashSet<>(this.enc_aceites);
    }

    public void setEnc_aceites(Set<String> enc_aceites){
        this.enc_aceites = new HashSet<>();
        this.enc_aceites.addAll(enc_aceites);
    }

    public Map<String, LoginI> getLogins(){
        Map<String, LoginI> res = new TreeMap<>();
        for(LoginI lg: this.logins.values())
            res.put(lg.getCodigo(), lg.clone());
        return res;
    }

    public void setLogins(Map<String, LoginI> lgs){
        this.logins = new TreeMap<>();
        for (LoginI lg: lgs.values())
            this.logins.put(lg.getCodigo(), lg.clone());
    }

    public TrazAquiModel clone(){
        return new TrazAquiModel(this);
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TrazAquiModel trazAqui = (TrazAquiModel) o;
        return  trazAqui.getUtilizadores().equals(this.utilizadores) &&
                trazAqui.getVoluntarios().equals(this.voluntarios) &&
                trazAqui.getEmpresas().equals(this.empresas) &&
                trazAqui.getLojas().equals(this.lojas) &&
                trazAqui.getLogins().equals(this.logins);
    }

    // -------------------------------Metodos de leitura e escrita----------------------------------------------------------
    /**
     * Metodo que le do ficheiro logs e escreve cada linha desse
     * ficheiro numa String que depois e adicionada a um array de
     * Strings e este posteriormente é retornado
     */
    private static String[] lerLogs() throws IOException {
        String []logs = new String[1000];
        FileReader fl = new FileReader("logs_20200416.txt");
        BufferedReader lerlogs = new BufferedReader(fl);
        String linha = lerlogs.readLine();
        int i=0;
        while(linha!=null){
            logs[i] = linha;
            i++;
            linha = lerlogs.readLine();
        }

        return logs;
    }

    /**
     * Metodo que le os logs que estao por linhas num array de strings
     * e cria os dados da aplicacao com todos os utilizadores, voluntarios
     * transportadoras, lojas, logins e encomendas aceites.
     */
    public String[] lerS() throws IOException {
        return lerLogs().clone();
    }

    // Metodo caso a funçao de escrever e ler em binario nao funcione
    public void lerTA(String[] logs){
        String[] p;
        String[] p1;

        for (String log : logs) {       // ciclo for que le cada linha do array de strings
            if(log==null) break;
            p = log.split(",");
            p1 = p[0].split(":");

            switch (p1[0]) {            // switch que cria todos os elementos de Traz aqui model
                case "Voluntario":
                    VoluntarioI v = new Voluntario();
                    v.leTA(p1[1], p);
                    addVoluntario(v.clone());
                    break;

                case "VoluntarioMedicamentos":
                    VoluntarioMedicamentosI vm = new VoluntarioMedicamentos();
                    vm.leVlM(p1[1], p);
                    addVoluntario(vm.clone());
                    break;

                case "Utilizador":
                    UtilizadorI u = new Utilizador();
                    u.leTA(p1[1], p);
                    addUtilizador(u.clone());
                    break;

                case "Transportadora":
                    Empresa_TransportadoraI et = new Empresa_Transportadora();
                    et.leTA(p1[1], p);
                    addEmpresas(et.clone());
                    break;

                case "TransportadoraMedicamentos":
                    ETransportadoraMedicamentosI etm = new ETransportadoraMedicamentos();
                    etm.leETM(p1[1], p);
                    addEmpresas(etm.clone());
                    break;

                case "Loja":
                    LojaI l = new Loja();
                    l.leTA(p1[1], p);
                    addLoja(l.clone());
                    break;

                case "Encomenda":
                    EncomendaI e = new Encomenda();
                    e.leEnc(p1[1], p);
                    addEncomenda(e.clone());
                    break;

                case "Aceite":
                    this.enc_aceites.add(p1[1]);
                    break;

                case "Login":
                    LoginI lg = new Login();
                    lg.leTA(p1[1], p);
                    addLogin(lg.clone());
                    break;
                default:
                    break;
            }
        }
        // ciclos for que vao as encomendas e colocam-nas nos list de
        // voluntario transportadora e loja
        for(VoluntarioI v: this.voluntarios.values())
            v.atualizaEnc(this.encomendas);

        for(Empresa_TransportadoraI et: this.empresas.values())
            et.atualizaEnc(this.encomendas);
    }

    /**
     * Metodo que grava em ficheiro de objetos(binario) o estado total da turma.
     */
    public void guardaEstado() throws IOException{
        FileOutputStream fos = new FileOutputStream("trazAqui.dat");
        ObjectOutputStream oos = new ObjectOutputStream(fos);

        oos.writeObject(this); // guarda __ esta __ instancia de uma so vez
                               // tem de  ser Serializable
        oos.flush();
        oos.close();
    }

    /**
     * Metodo que recupera uma instancia de turma gravada em ficheiro de objetos.
     */
    public TrazAquiModel carregaEstado() throws IOException, ClassNotFoundException{
        FileInputStream fis = new FileInputStream("trazAqui.dat");
        ObjectInputStream ois = new ObjectInputStream(fis);

        TrazAquiModel ta = (TrazAquiModel) ois.readObject();
        ois.close();
        return ta;
    }

    // -------------------------------Login-------------------------------------------------------

    /**
     * Metodo que adiciona um utilizador aos logins
     */
    private void addLogin(String cod, String nome, String pass){
        Login lg = new Login(cod, nome, pass);
        this.logins.put(cod, lg.clone());
    }

    private void addLogin(LoginI lg){
        this.logins.put(lg.getCodigo(), lg.clone());
    }

    /**
     * Metodo que faz os registos na aplicacao
     * @return o objeto a ser registado
     */
    private Object registar(String code, String nome, String password, double[] gps){
        addLogin(code, nome, password);

        Object o;
        switch (code.charAt(0)) {
            case 'u':
                o = new Utilizador(code, nome, gps[0], gps[1]);
                break;
            case 'v':
                o = new Voluntario(code, nome, gps[0], gps[1]);
                break;
            case 'l':
                o = new Loja(code, nome, gps[0], gps[1]);
                break;
            default:
                o = new Empresa_Transportadora(code, nome, gps[0], gps[1]);
                break;
        }
        return o;
    }

    /**
     * Metodo que valida o login
     * @return se é valido true false caso nao seja
     */
    public boolean loginValido(String codigo, String password){
        return  (this.logins.containsKey(codigo) && this.logins.get(codigo).getPassword().equals(password));
    }

    // -------------------------------Metodos relativos ao Utilizador-------------------------------------------------------
    /**
     * Cria um codigo de utilizador novo
     */
    public String criaUtilizador(){
        String codUt;
        int cod;
        do {
            cod = (int) (Math.random() * 100);
            codUt = ("u" + cod);
        }while (this.utilizadores.containsKey(codUt));

        return codUt;
    }

    /**
     * Metodo que adiciona um utilizador, utilizador para ler os logs
     * Apenas é utilizada caso se use o metodo de ler ficheiros texto
     */
    private void addUtilizador(UtilizadorI u){
        this.utilizadores.put(u.getCodigo(), u.clone());
    }

    /**
     * Metodo que adiciona
     * e faz o seu registo
     */
    public void addUtilizador(String code, String nome, String password, double[] coordenadas){
        UtilizadorI u = (UtilizadorI) registar(code, nome, password, coordenadas);
        this.registar(code, nome, password, coordenadas);
        this.utilizadores.put(u.getCodigo(), u.clone());
    }

    /**
     * Metodo que remove um utilizador
     * Remove tambem do login
     */
    public void removeUtilizador(String codUtilizador){
        this.utilizadores.remove(codUtilizador);
        this.logins.remove(codUtilizador);
    }

    /**
     * Metodo que cria a encomenda solicitada pelo utilizador
     * @param list Recebe uma lista de strings ja com os valores ordenados pelo que se introduz na lista
     */
    public String solicitaEncomenda(String codUt, String codLoja, List<String> list){
        String codEnc;
        do{
            int cod = (int)(Math.random()*10000);
            codEnc = "e" + cod;
        } while (this.encomendas.containsKey(codEnc));

        EncomendaI e = new Encomenda();
        e.criarEncomenda(codEnc, codUt, codLoja, list); // Cria uma encomenda com os dados recebidos
        addEncomenda(e);
        return e.getCodEncomenda();
    }

    /**
     * Metodo que consulta a informacao de um transportador
     * Seja ele voluntario ou uma empresa
     */
    public String consultaInformacao(String codUtilizador, String codTr, LocalDateTime inicio, LocalDateTime fim){
        String inf;
        if(codTr.charAt(0) == 'v'){
            VoluntarioI v = new Voluntario((Voluntario) this.voluntarios.get(codTr));
            inf = v.infVoluntario() +
                    this.utilizadores.get(codUtilizador).informacaoTransporte(v.getHistEntregas(), inicio, fim);
        }
        else{
            Empresa_TransportadoraI et = new Empresa_Transportadora((Empresa_Transportadora) this.empresas.get(codTr));
            inf = et.infTransportadora() +
                    this.utilizadores.get(codUtilizador).informacaoTransporte(et.getHistEntregas(), inicio, fim);
        }
        return inf;
    }


    // -------------------------------Metodos relativos ao Voluntário-------------------------------------------------------
    /**
     * Metodo que cria codigo de voluntario
     */
    public String criaVoluntario(){
        String codUt;
        int cod;
        do {
            cod = (int) (Math.random() * 100);
            codUt = ("v" + cod);
        }while (this.utilizadores.containsKey(codUt));

        return codUt;
    }

    /**
     * Metodo que cria um novo voluntario e regista
     */
    public void newVoluntarioMedico(String code, String nome, String password, double[] coordenadas, char medico, double raio){
        VoluntarioI aux = (Voluntario) registar(code, nome, password, coordenadas);

        if(Character.toUpperCase(medico)=='S') {
            VoluntarioMedicamentosI v =
                    new VoluntarioMedicamentos(code, aux.getNome(), aux.getGpsX(), aux.getGpsY(), raio,
                            false, 0, new ArrayList<>(), false);
            addVoluntario(v.clone());
        }
        else {
            VoluntarioI v = new Voluntario(code, aux.getNome(), aux.getGpsX(), aux.getGpsY(), raio,
                    false, 0, new ArrayList<>());
            addVoluntario(v.clone());
        }
    }

    /**
     * adiciona um voluntario ao ta
     */
    private void addVoluntario(VoluntarioI v){
        this.voluntarios.put(v.getCodigo(), v.clone());
    }

    /**
     * Metodo que remove um voluntario
     */
    public void removeVoluntario(String codVoluntario){
        this.voluntarios.remove(codVoluntario);
        this.logins.remove(codVoluntario);
    }

    /**
     *  Metodo que determina a listagens dos 10 voluntarios
     *  que mais utilizam o sistema (em número de encomendas transportadas);
     */
    public List<String> voluntariosMaisUtilizam(){
         List<String> aux = this.voluntarios.values().stream().
                 sorted((v1, v2) -> v2.getHistEntregas().size() - v1.getHistEntregas().size()).
                 map(VoluntarioI::getNome).
                 collect(Collectors.toList());

        List<String> res = new ArrayList<>();
        for(int i = 0; i < 10 && i < aux.size()-1; i++)
            res.add((i+1) + " - " + aux.get(i));

        aux.clear();
        return res;
    }

    /**
     * Calcula a distancia de um voluntario a uma loja
     */
    public double distLojaVl(String codVl, String codLoja){
        double xL = this.lojas.get(codLoja).getGpsX();
        double yL = this.lojas.get(codLoja).getGpsY();
        return this.voluntarios.get(codVl).distLoja(xL, yL);
    }

    /**
     * Metodo que determina o voluntario que vai buscar a encomenda
     * Devolve null caso nenhum voluntario possa ir buscar
     * @return "" caso nao haja nenhum voluntario
     */
    public String qualVoluntario(EncomendaI e){
        String codLoja = e.getCodLoja();
        List<Double> auxd = devolveXY(codLoja, e.getCodUtilizador()); // Devolve os x e y da loja e utilizador respeticamente

        VoluntarioI res = null;
        for(VoluntarioI v: this.voluntarios.values()) { // Percorre todos os voluntarios e ve qual o que esta mais perto da loja
            if (v.podeIrBuscar(auxd.get(0), auxd.get(1), auxd.get(2), auxd.get(3)) && res == null)
                res = new Voluntario(v.clone());
            else if (v.podeIrBuscar(auxd.get(0), auxd.get(1), auxd.get(2), auxd.get(3)) && res != null &&
                    distLojaVl(v.getCodigo(), codLoja) < distLojaVl(res.getCodigo(), codLoja))
                res = v.clone();
        }

        String cod = "";
        if(res != null)
            cod = res.getCodigo();
        return cod;
    }

    /**
     * Metodo que um voluntario vai buscar uma encomenda
     */
    public LocalDateTime vaiBuscarEncVl(String codVl, String codEnc){
        VoluntarioI v = new Voluntario((Voluntario) this.voluntarios.get(codVl));
        LocalDateTime res = LocalDateTime.now();
        if(v.getHistEntregas().size() > 0) {
            res = v.getHistEntregas().get(v.getHistEntregas().size() - 1).getFimTransporte(); // Vai buscar a ultima
                                                                                                            // hora de entrega
            if (res.isBefore(LocalDateTime.now()))                                   // Caso esta seja antes da hora atual
                res = LocalDateTime.now();                                          // Atualiza
        }

        EncomendaI e = new Encomenda((Encomenda) this.encomendas.get(codEnc));
        this.voluntarios.get(codVl).disponivel(false);                      // Torna o voluntario indisponivel
        this.lojas.get(e.getCodLoja()).encomendaLevantada(e.getCodEncomenda());// Levanta a encomenda da loja pois ja n esta em fila de espera
        this.enc_aceites.add(codEnc);                                          // Adiciona a encomenda a lista das encs aceites
        return res;
    }

    /**
     * Metodo que entrega uma encomenda para voluntario
     */
    public void entregaEncVl(String vl, String codEnc, LocalDateTime inicio, int fila){
        Transporte_EncomendaI te = new Transporte_Encomenda(this.encomendas.get(codEnc));

        te.setInicioTransporte(inicio);

        List<Double> auxd = devolveXY(te.getCodLoja(), te.getCodUtilizador());

        double kms = this.voluntarios.get(vl)                                   // Calcula os kms feitos
                .distTotal(auxd.get(0), auxd.get(1), auxd.get(2), auxd.get(3)); // Usado para calcular o tempo de viagem
        this.voluntarios.get(vl).entregaEncomendaVl(te,kms,'S', fila, inicio);
    }

    /**
     * Metodo que retorna uma string com o codigo e o nome de todos os voluntarios
     */
    public String codeNomeVl(){
        StringBuilder sb = new StringBuilder();
        for(VoluntarioI v: this.voluntarios.values())
            sb.append(v.getCodigo()).append(" - ").append(v.getNome()).append("\n");
        return sb.toString();
    }

    /**
     * Metodo que diz se é voluntario medico
     */
    public boolean eMedicoVl(String codVl){
        return this.voluntarios.get(codVl).clone() instanceof VoluntarioMedicamentosI;
    }

    /**
     * Metodo disponibiliza voluntario medico
     */
    public void disponivelMedicoVl(boolean status, String codVl){
        VoluntarioMedicamentosI aux = new VoluntarioMedicamentos((VoluntarioMedicamentos) this.voluntarios.get(codVl));
        aux.aceitaMedicamentos(status);
        this.voluntarios.replace(codVl, this.getVoluntarios().get(codVl), aux.clone());
    }

    /**
     * Atualizar classificacao e depois tornar vl disponivel outra vez
     */
    public void atualizavl(String codVl, double cl){
        VoluntarioI v = new Voluntario((Voluntario) this.voluntarios.get(codVl));
        v.atualizaClassificacao(cl);
        v.disponivel(true);
        this.voluntarios.replace(codVl, this.voluntarios.get(codVl), v);
    }

    /**
     * Atualiza disponibilidade para um Voluntario
     */
    public void disponibilidadeVl(String codVl, boolean b){
        VoluntarioI v = new Voluntario((Voluntario) this.voluntarios.get(codVl));
        v.disponivel(b);
        this.voluntarios.replace(codVl, this.voluntarios.get(codVl), v);
    }

    // -------------------------------Metodos relativos a Encomendas-------------------------------------------------------
    /**
     * Metodo que adiciona uma encomenda ao ta
     */
    public void addEncomenda(EncomendaI e){
        this.encomendas.put(e.getCodEncomenda(), e.clone());
    }

    /**
     * Gerar encomendas
     * Metodos para quando se é um transportador
     */
    public String geraEnc(String codTransportador, boolean medico){
        String codL, codUt, codEnc, res;
        int cod;

        do{                                         // Gera um codigo para uma encomenda
            cod = (int)(Math.random()*10000);
            codEnc = "e" + cod;
        } while(this.encomendas.containsKey(codEnc));

        if(codTransportador.charAt(0) == 't') {    // Caso seja transportador
            codL = geraCodLojaTr(codTransportador);// Ve uma loja dentro do raio de acao
            codUt = geraCodUtTr(codTransportador); // Ve um cliente dentro do raio de acao
        }
        else {                                      // Caso seja voluntario
            codL = geraCodLojaVl(codTransportador); // Ve uma loja dentro do raio de acao
            codUt = geraCodUtVl(codTransportador);  // Ve um cliente dentro do raio de acao
        }
        if(codL.equals("") || codUt.equals(""))  // Caso nao existam um dos dois nao é possivel haver uma encomenda
            res = "";
        else {                                  // Caso em que existe encomenda
            EncomendaI e = new Encomenda();
            e.encomendaRandom(codEnc, codUt, codL, medico); // Usa-se esta funçao para gerar a lista de produtos

            this.addEncomenda(e.clone());                   // Adiciona-se a encomenda a lista de encomendas
            res = codEnc;
        }
        return res;
    }

    // Metodo usado para escolher de forma aleatoria a loja no caso de transportadora
    private String geraCodLojaTr(String codTr){
        List<String> codLojas;
        Empresa_TransportadoraI et = new Empresa_Transportadora((Empresa_Transportadora) this.empresas.get(codTr));

        codLojas = this.lojas.values().stream()
                .filter(l -> distLojaEt(et.getCodigo(), l.getCodigo()) < et.getRaio()) // Filtra as lojas que pertencem ao raio
                .map(LojaI::getCodigo)                                             // Vai buscar o codigo das lojas
                .collect(Collectors.toList());                                     // Coloca tudo numa lista

        int random = (int)(Math.random()*100) % codLojas.size();
        String res = codLojas.get(random);

        codLojas.clear();

        return res;
    }

    // Metodo usado para escolher de forma aleatoria a loja no caso de Voluntario
    private String geraCodLojaVl(String codVl){
        List<String> codLojas;
        VoluntarioI v = new Voluntario((Voluntario) this.voluntarios.get(codVl));
        String res = "";

        codLojas = this.lojas.values().stream()
                .filter(l -> distLojaVl(v.getCodigo(), l.getCodigo()) < v.getRaio()) // Filtra as lojas que pertencem ao raio
                .map(LojaI::getCodigo)                                               // Vai buscar o codigo das lojas
                .collect(Collectors.toList());                                       // Coloca tudo numa lista

        if(codLojas.size() != 0) {
            int random = (int) (Math.random() * 10000) % codLojas.size();
            res = codLojas.get(random);
        }
        codLojas.clear();
        return res;
    }

    // Metodo usado para escolher de forma aleatoria o utilizador no caso de voluntario
    private String geraCodUtVl(String codVl){
        List<String> codUt;
        VoluntarioI v = new Voluntario((Voluntario) this.voluntarios.get(codVl));
        String res = "";

        codUt = this.utilizadores.values().stream()
                .filter(l -> v.distLoja(l.getGpsX(), l.getGpsY()) < v.getRaio()) // Filtra as lojas que pertencem ao raio
                .map(UtilizadorI::getCodigo)                                     // Vai buscar o codigo das lojas
                .collect(Collectors.toList());                                   // Coloca tudo numa lista

        if(codUt.size() != 0) {
            int random = (int) (Math.random() * 10000) % codUt.size();
            res = codUt.get(random);
        }
        codUt.clear();
        return res;
    }

    // Metodo usado para escolher de forma aleatoria o utilizador no caso de transportadora
    private String geraCodUtTr(String codTr){
        List<String> codUt;
        Empresa_TransportadoraI et = new Empresa_Transportadora((Empresa_Transportadora) this.empresas.get(codTr));

        codUt = this.utilizadores.values().stream()
                .filter(l -> et.distLoja(l.getGpsX(), l.getGpsY()) < et.getRaio()) // Filtra as lojas que pertencem ao raio
                .map(UtilizadorI::getCodigo)                                       // Vai buscar o codigo das lojas
                .collect(Collectors.toList());                                     // Coloca tudo numa lista

        int random = (int)(Math.random()*100) % codUt.size();
        return codUt.get(random);
    }

    /**
     * Metodo que remove uma encomenda
     */
    public void removeEncomenda(String enc){
        this.encomendas.remove(enc);
    }

    // -------------------------------Metodos relativos a Empresas-------------------------------------------------------
    /**
     * Metodo que cria o codigo de uma transportadora
     */
    public String criaEmpresa_Transportadora(){
        String codET;
        int cod;
        do {
            cod = (int) (Math.random() * 100);
            codET = ("t" + cod);
        }while (this.empresas.containsKey(codET));

        return codET;
    }

    /**
     * Metodo que cria uma nova empresa e faz o seu registo
     */
    public void newEmpresa_Transportadora(String code, String nome, String password, double[] coordenadas,
                                          char b, String codET, int nif, double r, double taxa, int lotacao){
        Empresa_TransportadoraI aux = (Empresa_Transportadora) registar(code, nome, password, coordenadas);
        if(b=='S') {
            ETransportadoraMedicamentosI et =
                    new ETransportadoraMedicamentos(codET, aux.getNome(), aux.getGpsX(), aux.getGpsY(), nif, r, taxa,
                            false, 0, 0, lotacao, 0, new ArrayList<>(), false);
            addEmpresas(et.clone());
        }
        else {
            Empresa_TransportadoraI et =
                    new Empresa_Transportadora(codET, aux.getNome(), aux.getGpsX(), aux.getGpsY(), nif, r,
                            taxa, false, 0, 0, lotacao, 0, new ArrayList<>());
            addEmpresas(et.clone());
        }
    }

    /**
     * Metodo que adiciona uma empresa ao ta
     */
    private void addEmpresas(Empresa_TransportadoraI e){
        this.empresas.put(e.getCodigo(), e.clone());
    }

    /**
     * Metodo que remove uma empresa transportadora
     */
    public void removeEmpresas(String codEmpresa){
        this.logins.remove(codEmpresa);
        this.empresas.remove(codEmpresa);
    }

    /**
     * Metodo que determina a listagens dos 10 empresas transportadoras que mais utilizam
     * o sistema (em número de kms percorridos)
     */
    public List<String> transportadorasMaisUtilizam(){
        List<String> aux =
            this.empresas.values().stream().
                sorted(
                    (e1, e2) -> {                           // É a mesma coisa que fazer um comparator
                        double t = (e2.getKmsFeitos()-e1.getKmsFeitos());
                        if (t>0) return 1;
                        if (t==0) return 0;
                        return -1;
                    }).
                map(Empresa_TransportadoraI::getNome).
                collect(Collectors.toList());

        List<String> res = new ArrayList<>();
        for(int i = 0; i < 10 && i < aux.size()-1; i++)
            res.add((i+1) + " - " + aux.get(i));

        aux.clear();
        return res;
    }

    /**
     * Metodo que determina a empresa transportadora que vai buscar a encomenda
     * @return "" quando nenhuma das empresas pode ir buscar
     */
    public String qualTransportadora(EncomendaI encomenda, List<String> codTr) {
        String codLoja = this.lojas.get(encomenda.getCodLoja()).getCodigo();
        List<Double> auxd = devolveXY(codLoja, this.utilizadores.get(encomenda.getCodUtilizador()).getCodigo());

        TreeSet<String> codTrAux = new TreeSet<>(codTr);
        List<Empresa_TransportadoraI> ets = new ArrayList<>();
        for(Empresa_TransportadoraI tr: this.empresas.values())
            if(!codTrAux.contains(tr.getCodigo()))
                ets.add(tr);

        Empresa_TransportadoraI res = null;

        for (Empresa_TransportadoraI transportadora: ets) {  // Ciclo for que vai buscar a transportadora
                                                                                // que esta mais perto
            if (res == null                                                     // Se res é null
                    && transportadora.podeIrBuscar(auxd.get(0), auxd.get(1), auxd.get(2), auxd.get(3))) { // E pode transportadora pode ir buscar
                res = new Empresa_Transportadora((Empresa_Transportadora) transportadora);  // Como é a unica ate agora res = atual
            }
            else if(transportadora.podeIrBuscar(auxd.get(0), auxd.get(1), auxd.get(2), auxd.get(3)) // Caso possa ir buscar
                    && res!=null                                                                    // res nao seja null
                    && distLojaEt(transportadora.getCodigo(), codLoja) < distLojaEt(res.getCodigo(), codLoja)) {  // E a distancia a loja seja
                res = new Empresa_Transportadora((Empresa_Transportadora) transportadora);          // menor q atual atualiza res
            }
        }

        String cod = "";
        if(res != null){            // Vai buscar o codigo do resultado caso res tenha encontrado pelo menos uma empresa
            cod = res.getCodigo();
        }
        return cod;
    }

    /**
     * Metodo que retorna uma string com o nome e o codigo de todas as transportadoras
     */
    public String codeNomeTr(){
        StringBuilder sb = new StringBuilder();
        for(Empresa_TransportadoraI et: this.empresas.values())
            sb.append(et.getCodigo()).append(" - ").append(et.getNome()).append("\n");

        return sb.toString();
    }

    /**
     * Calcula o preco do transporte
     */
    public double precoTr(String codTr, String codEnc){
        double preco;
        String codLj = this.encomendas.get(codEnc).getCodLoja();
        String codUt = this.encomendas.get(codEnc).getCodUtilizador();
        List<Double> auxd = devolveXY(codLj, codUt);       // Devolve xL, yL, xU e yU
        double peso = this.encomendas.get(codEnc).getPeso();
        preco = this.empresas.get(codTr).precoTransporte(auxd.get(0), auxd.get(1), auxd.get(2), auxd.get(3), peso);
        return preco;
    }

    /**
     * metodo que vai buscar uma encomenda para transportadora
     */
    public LocalDateTime vaiBuscarEncTr(String codTr, String codEnc){
        Empresa_TransportadoraI et = new Empresa_Transportadora((Empresa_Transportadora) this.empresas.get(codTr));

        LocalDateTime res = LocalDateTime.now();
        if(et.getHistEntregas().size() > 0) {
            res = et.getHistEntregas().get(et.getHistEntregas().size() - 1).getFimTransporte(); // Vai buscar a ultima
            // hora de entrega
            if (res.isBefore(LocalDateTime.now()))                                   // Caso esta seja antes da hora atual
                res = LocalDateTime.now();                                          // Atualiza
        }
        EncomendaI e = new Encomenda((Encomenda) this.encomendas.get(codEnc));
        this.lojas.get(e.getCodLoja()).encomendaLevantada(codEnc);  // Levanta a encomenda da loja pois ja n esta em fila de espera

        this.empresas.get(codTr).disponivel(false);         // Torna a transportadora indisponive
        this.enc_aceites.add(codEnc);                          // Adiciona a encomenda a lista das encs aceites

        return res;
    }

    /**
     * Metodo que entrega encomenda para transportadora
     */
    public void entregaEncTr(String tr, String codEnc, LocalDateTime inicio, int fila){
        Transporte_EncomendaI te = new Transporte_Encomenda(this.encomendas.get(codEnc));

        List<Double> auxd  =devolveXY(te.getCodLoja(), te.getCodUtilizador());

        double kms = this.empresas.get(tr).clone().distTotal(auxd.get(0), auxd.get(1), auxd.get(2), auxd.get(3));
        double custo = this.empresas.get(tr).clone().precoTransporte(auxd.get(0), auxd.get(1), auxd.get(2), auxd.get(3), te.getPeso());
        te.setInicioTransporte(inicio);

        te.clone().setCustoTransporte(custo);
        this.empresas.get(tr).entregaEncomendaTr(te, kms,'S', fila, inicio);
    }

    /**
     * Atualiza Transportadora
     */
    public void atualizaTr(String codTr, double cl){
        Empresa_TransportadoraI et = new Empresa_Transportadora((Empresa_Transportadora) this.empresas.get(codTr));
        et.atualizaClassificacao(cl);
        et.disponivel(true);
        this.empresas.replace(codTr, this.empresas.get(codTr), et);
    }

    /**
     * Metodo que calcula a distancia de uma empresa a uma loja
     */
    public double distLojaEt(String codTr, String codLoja){
        double xL = this.lojas.get(codLoja).getGpsX();
        double yL = this.lojas.get(codLoja).getGpsY();
        return this.empresas.get(codTr).distLoja(xL, yL);
    }

    /**
     * Loja mais perto
     */
    public String lojaMaisPerto(String codTr, List<String> codEncs){
        String codLoja = this.encomendas.get(codEncs.get(0)).getCodLoja();
        String l;

        for(String e: codEncs){
            l = this.encomendas.get(e).getCodLoja();
            if(distLojaEt(codTr, codLoja) > distLojaEt(codTr, l)) {
                codLoja = l;
            }
        }
        return codLoja;
    }

    /**
     * Metodo que calcula distancias entre lojas
     */
    public double distEntreLojas(String codL1, String codL2){
        LojaI l1 = new Loja((Loja) this.lojas.get(codL1));
        LojaI l2 = new Loja((Loja) this.lojas.get(codL2));

        double xL1 = l1.clone().getGpsX();
        double yL1 = l1.clone().getGpsY();
        double xL2 = l2.clone().getGpsX();
        double yL2 = l2.clone().getGpsY();

        return Math.sqrt(Math.pow((xL1 - xL2),2) + Math.pow((yL1 - yL2),2));
    }

    /**
     * Metodo que ordena as lojas por distancia
     */
    public List<String> ordenaLojas(String codL1, List<String> codLojas){
        Set<String> aux = new TreeSet<>((d1, d2) -> {                           // É a mesma coisa que fazer um comparator
            double t = (distEntreLojas(codL1, d1)-distEntreLojas(codL1, d2));
            if (t>0) return 1;
            if (t==0) return 0;
            return -1;
        });
        aux.addAll(codLojas);
        Iterator<String> it = aux.iterator();
        List<String> res = new ArrayList<>();
        res.add(codL1);
        while (it.hasNext()){
            res.add(it.next());
        }

        return res;
    }

    /**
     * Metodo que calcula distancia entre loja e ut mais perto
     */
    public String utMaisPerto(String codLoja, List<String> uts){
        LojaI l = new Loja((Loja) this.lojas.get(codLoja));
        double xL = l.getGpsX();
        double yL = l.getGpsY();
        double xU = MAX_VALUE, yU = MAX_VALUE, xUAux, yUAux;
        String cod = "";
        for(String u: uts){
            xUAux = this.utilizadores.get(u).getGpsX();
            yUAux = this.utilizadores.get(u).getGpsY();
            double dist = Math.sqrt(Math.pow((xL - xU),2) + Math.pow((yL - yU),2));
            double distAux = Math.sqrt(Math.pow((xL - xUAux),2) + Math.pow((yL - yUAux),2));
            if(distAux < dist){
                xU = xUAux;
                yU = yUAux;
                cod = u;
            }
        }
        return cod;
    }

    /**
     * Metodo que calcula distancias entre Utilizadores
     */
    private double distEntreUts(String codUt1, String codUt2){
        UtilizadorI ut1 = new Utilizador((Utilizador) this.utilizadores.get(codUt1));
        UtilizadorI ut2 = new Utilizador((Utilizador) this.utilizadores.get(codUt2));

        double xU1 = ut1.clone().getGpsX();
        double yU1 = ut1.clone().getGpsY();
        double xU2 = ut2.clone().getGpsX();
        double yU2 = ut2.clone().getGpsY();

        return Math.sqrt(Math.pow((xU1 - xU2),2) + Math.pow((yU1 - yU2),2));
    }

    /**
     * Metodo que ordena as Utilizadores por distancia
     */
    public List<String> ordenaUts(String codUt1, List<String> codUts){
        Set<String> aux = new TreeSet<>((d1, d2) -> {                           // É a mesma coisa que fazer um comparator
            double t = (distEntreUts(codUt1, d1)-distEntreUts(codUt1, d2));
            if (t>0) return 1;
            if (t==0) return 0;
            return -1;
        });
        aux.addAll(codUts);
        Iterator<String> it = aux.iterator();
        List<String> res = new ArrayList<>();
        res.add(codUt1);
        while (it.hasNext()){
            res.add(it.next());
        }

        return res;
    }

    /**
     * Metodo que vai entrega uma encomenda para tr de varias encomendas
     */
    public void entregaEncTrEncs(String codTr, String codEnc, double kms, LocalDateTime inicio, int nFilaLojas){
        Empresa_TransportadoraI et = new Empresa_Transportadora((Empresa_Transportadora) this.empresas.get(codTr));
        Transporte_EncomendaI te = new Transporte_Encomenda(this.encomendas.get(codEnc));

        te.setInicioTransporte(inicio);
        LocalDateTime fim = et.somaHoras(inicio, et.tempoTransporte(nFilaLojas, kms));
        te.setFimTransporte(fim);

        List<Double> auxd = devolveXY(this.encomendas.get(codEnc).getCodLoja(), this.encomendas.get(codEnc).getCodUtilizador());

        double preco = et.precoTransporte(auxd.get(0), auxd.get(1), auxd.get(2), auxd.get(3), et.getTaxa());
        te.setCustoTransporte(preco);
        et.addEncomenda(te.clone());

        this.empresas.replace(codTr, this.empresas.get(codTr), et);
    }

    /**
     * Metodo que diz se é Empresa medico
     */
    public boolean eMedicoTr(String codTr){
        return this.empresas.get(codTr).clone() instanceof ETransportadoraMedicamentosI;
    }

    /**
     * Metodo que torna disponivel uma empresa medica
     */
    public void disponivelMedicoTr(boolean status, String codTr){
        ETransportadoraMedicamentosI aux = new ETransportadoraMedicamentos((ETransportadoraMedicamentos) this.empresas.get(codTr));
        aux.aceitaMedicamentos(status);
        this.empresas.replace(codTr, this.empresas.get(codTr), aux.clone());
    }

    /**
     * Atualiza disponibilidade para uma transportadora
     */
    public void disponibilidadeTr(String codTr, boolean b){
        Empresa_TransportadoraI et = new Empresa_Transportadora((Empresa_Transportadora) this.empresas.get(codTr));
        et.disponivel(b);
        this.empresas.replace(codTr, this.empresas.get(codTr), et);
    }
    /**
     * Distancia entre do atual ao ultimo visitado
     */
    public double distUlt(String ult, String at){
        double dist;
        if(ult.charAt(0) == 'l'){
            List<Double> xy = devolveXY(ult, at);
            dist = Math.sqrt(Math.pow((xy.get(0) - xy.get(2)),2) + Math.pow((xy.get(1) - xy.get(3)),2));
        }
        else {
            dist = distEntreUts(ult, at);
        }
        return dist;
    }

    /**
     * Metodo que atualiza kms de uma transportadora
     */
    public void atualizaKms(String codTr, double kms){
        Empresa_TransportadoraI et = new Empresa_Transportadora((Empresa_Transportadora) this.empresas.get(codTr));
        double kmsFeitos = et.getKmsFeitos() + kms;
        et.setKmsFeitos(kmsFeitos);
        this.empresas.replace(codTr, this.empresas.get(codTr), et);
    }

    /**
     * Devolve a distancia total percorrida
     */
    public double distTotal(String codTr, String codEnc){
        String codLj = this.encomendas.get(codEnc).getCodLoja();
        String codut = this.encomendas.get(codEnc).getCodUtilizador();
        Empresa_TransportadoraI et = new Empresa_Transportadora((Empresa_Transportadora) this.empresas.get(codTr));

        List<Double> xy = devolveXY(codLj, codut);
        return et.distTotal(xy.get(0), xy.get(1), xy.get(2), xy.get(3));
    }


    // -------------------------------Metodos relativos a Lojas-------------------------------------------------------
    /**
     * Metodo que cria o codigo de uma loja
     */
    public String criaLoja(){
        String codL;
        int cod;
        do {
            cod = (int) (Math.random() * 100);
            codL = ("l" + cod);
        }while (this.lojas.containsKey(codL));

        return codL;
    }

    /**
     * Metodo que cria uma nova loja
     */
    public void newLoja(String code, String nome, String password, double[] coordenadas){
        LojaI aux = (Loja) registar(code, nome, password, coordenadas);
        LojaI l = new Loja(code, aux.getNome(), aux.getGpsX(), aux.getGpsY(), new ArrayList<>());
        addLoja(l.clone());
    }

    /**
     * Metodo que adiciona uma loja ao ta
     */
    private void addLoja(LojaI l){
        this.lojas.put(l.getCodigo(), l.clone());
    }

    /**
     * Metodo que remove uma loja
     */
    public void removeLoja(String codLoja){
        this.logins.remove(codLoja);
        this.lojas.remove(codLoja);
    }

    /**
     * Metodo que adiciona o peso de uma encomenda
     */
    public void adicionaPesoEnc(String codLoja, String codEnc, double peso){
        EncomendaI e = new Encomenda((Encomenda) this.encomendas.get(codEnc));
        LojaI lj = new Loja((Loja) this.lojas.get(codLoja));
        e = lj.addPeso(e, peso);

        this.encomendas.replace(codEnc, this.encomendas.get(codEnc), e);
    }

    /**
     * Metodo que devolve uma string com o codigo e o nome de todas as lojas
     */
    public String codeNomeLojas(){
        StringBuilder sb = new StringBuilder();
        for(LojaI l: this.lojas.values())
            sb.append(l.getCodigo()).append(" - ").append(l.getNome()).append("\n");

        return sb.toString();
    }

    /**
     * Metodo que gera encomendas para as lojas
     */
    public String encLoja(String codLoja){
        String codEnc, codUt;
        int cod;
        do{
            cod = (int)(Math.random()*10000);
            codEnc = "e" + cod;
        } while(this.encomendas.containsKey(codEnc));

        do{
            cod = (int)(Math.random()*10000);
            codUt = "u" + cod;
        } while(!this.utilizadores.containsKey(codUt));

        EncomendaI e = new Encomenda();
        e.encomendaRandom(codEnc, codUt, codLoja, false);
        addEncomenda(e.clone());
        this.lojas.get(codLoja).addEncomenda(codEnc);

        return codEnc;
    }

    // ------------------ decisao entre voluntario e transportador ------------------------

    /**
     * Metodo que decide entre voluntario e transportadora (qual deles vai buscar a encomenda)
     * criterio, e o que esta mais perto
     */
    public String vlOuTr(String codTr, String codVl, String codLj){
        String res;
        if(distLojaVl(codVl, codLj) < distLojaEt(codTr, codLj)){
            res = "vl";
        } else res = "tr";

        return res;
    }

    // ------------------ Funçoes Auxiliares que involvem mais que uma instancia ---------------
    /**
     * Calculo dos x e y
     * @return 0 - xL, 1 - yL, 2 - xU, 3 - yU
     */
    private List<Double> devolveXY(String codLoja, String codUt) {
        List<Double> res = new ArrayList<>();
        res.add(this.lojas.get(codLoja).clone().getGpsX());
        res.add(this.lojas.get(codLoja).clone().getGpsY());
        res.add(this.utilizadores.get(codUt).clone().getGpsX());
        res.add(this.utilizadores.get(codUt).clone().getGpsY());

        return res;
    }

}
