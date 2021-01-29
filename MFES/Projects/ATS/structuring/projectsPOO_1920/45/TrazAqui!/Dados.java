
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.format.FormatStyle;
import java.util.ArrayList;
import java.util.*;
import java.lang.*;
import java.io.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;

import static java.lang.Character.isDigit;

/**
 * Dados - Registo com todos os utilizadores, voluntarios, transportadoras, lojas e encomendas existentes no sistema.
 *
 * @author (Eduardo Araujo, Ricardo Machado, Guilherme Araujo)
 * @version 11/06/2020
 */
public class Dados implements Serializable {
    private Map<String, Utilizador> utilizadores; //email,User
    private Map<String, Voluntario> voluntarios;
    private Map<String, Transportadora> transportadoras;
    private Map<String, Loja> lojas;
    private Map<String, Double> classificacoes; //codEnc,Score
    private List<Encomenda> encomendas;

    /**
     * Construtor para objetos da classe Dados (por omissao)
     */
    public Dados() {
        this.utilizadores = new HashMap<>();
        this.voluntarios = new HashMap<>();
        this.transportadoras = new HashMap<>();
        this.lojas = new HashMap<>();
        this.classificacoes = new HashMap<>();
        this.encomendas = new ArrayList<>();
    }

    /**
     * Construtor para objetos da classe Dados (parameterizado)
     *
     * @param user       os utilizadores
     * @param volu       os voluntarios
     * @param tran       as transportadoras
     * @param loja       as lojas
     * @param encomendas as encomendas
     */
    public Dados(Map<String, Utilizador> user, Map<String, Voluntario> volu, Map<String, Transportadora> tran, Map<String, Loja> loja, Map<String, Double> classificacoes, List<Encomenda> encomendas) {
        this.setUtilizadores(user);
        this.setVoluntarios(volu);
        this.setTransportadoras(tran);
        this.setLojas(loja);
        this.setEncomendas(encomendas);
        this.setClassificacoes(classificacoes);
    }

    /**
     * Construtor para objetos da classe Dados (de copia)
     *
     * @param d os dados
     */
    public Dados(Dados d) {
        this.utilizadores = d.getUtilizadores();
        this.voluntarios = d.getVoluntarios();
        this.transportadoras = d.getTransportadoras();
        this.lojas = d.getLojas();
        this.classificacoes = d.getClassificacoes();
        List<Encomenda> encomendasClone = new ArrayList<>();
        for (Encomenda e : d.encomendas) {
            encomendasClone.add(e.clone());
        }
        this.encomendas = encomendasClone;
    }

    /**
     * Metodo que devolve os utilizadores de um conjunto de dados
     *
     * @return os utilizadores dos dados
     */
    public Map<String, Utilizador> getUtilizadores() {
        Map<String, Utilizador> aux = new HashMap<>();
        for (String em : this.utilizadores.keySet()) {
            Utilizador c = this.utilizadores.get(em);
            aux.put(em, c.clone());
        }
        return aux;
    }

    /**
     * Metodo que devolve os voluntarios de um conjunto de dados
     *
     * @return os voluntarios dos dados
     */
    public Map<String, Voluntario> getVoluntarios() {
        Map<String, Voluntario> aux = new HashMap<>();
        for (String em : this.voluntarios.keySet()) {
            Voluntario c = this.voluntarios.get(em);
            aux.put(em, c.clone());
        }
        return aux;
    }

    /**
     * Metodo que devolve as transportadoras de um conjunto de dados
     *
     * @return as transportadoras dos dados
     */
    public Map<String, Transportadora> getTransportadoras() {
        Map<String, Transportadora> aux = new HashMap<>();
        for (String em : this.transportadoras.keySet()) {
            Transportadora c = this.transportadoras.get(em);
            aux.put(em, c.clone());
        }
        return aux;
    }

    /**
     * Metodo que devolve as lojas de um conjunto de dados
     *
     * @return as lojas dos dados
     */
    public Map<String, Loja> getLojas() {
        Map<String, Loja> aux = new HashMap<>();
        for (String em : this.lojas.keySet()) {
            Loja c = this.lojas.get(em);
            aux.put(em, c.clone());
        }
        return aux;
    }

    /**
     * Metodo que devolve as classificacoes de um conjunto de dados
     *
     * @return as classificacoes dos dados
     */
    public Map<String, Double> getClassificacoes() {
        Map<String, Double> aux = new HashMap<>();
        for (String em : this.classificacoes.keySet()) {
            Double c = this.classificacoes.get(em);
            aux.put(em, c);
        }
        return aux;
    }


    /**
     * Metodo que devolve as encomendas de um conjunto de dados
     *
     * @return as encomendas dos dados
     */
    public List<Encomenda> getEncomendas() {
        List<Encomenda> aux = new ArrayList<>();
        for (Encomenda a : this.encomendas)
            aux.add(a.clone());
        return aux;
    }

    /**
     * Metodo que altera os utilizadores de um conjunto de dados
     *
     * @param usr os novos utilizadores
     */
    public void setUtilizadores(Map<String, Utilizador> usr) {
        this.utilizadores = usr.values().stream()
                .collect(Collectors
                        .toMap((Utilizador::getEmail), Utilizador::clone));
    }

    public void setClassificacoes(Map<String, Double> cla) {
        for(String s : cla.keySet()){
            classificacoes.put(s,cla.get(s));
        }
    }


    /**
     * Metodo que altera os voluntarios de um conjunto de dados
     *
     * @param vol os novos voluntarios
     */
    public void setVoluntarios(Map<String, Voluntario> vol) {
        this.voluntarios = vol.values().stream()
                .collect(Collectors
                        .toMap((Voluntario::getEmail), Voluntario::clone));
    }

    /**
     * Metodo que altera as trasnportadoras de um conjunto de dados
     *
     * @param tra as novas transportadoras
     */
    public void setTransportadoras(Map<String, Transportadora> tra) {
        this.transportadoras = tra.values().stream()
                .collect(Collectors
                        .toMap((Transportadora::getEmail), Transportadora::clone));
    }

    /**
     * Metodo que altera as lojas de um conjunto de dados
     *
     * @param loj as novas lojas
     */
    public void setLojas(Map<String, Loja> loj) {
        this.lojas = loj.values().stream()
                .collect(Collectors
                        .toMap((Loja::getEmail), Loja::clone));
    }

    /**
     * Metodo que altera as encomendas de um conjunto de dados
     *
     * @param al as novas encomendas
     */
    public void setEncomendas(List<Encomenda> al) {
        this.encomendas = new ArrayList<>();
        for (Encomenda a : al)
            this.encomendas.add(a.clone());
    }

    /**
     * Metodo que le dados de um ficheiro
     *
     * @return Dados do ficheiro lido
     */
    public static Dados abrirFicheiro(String nomeFicheiro) throws IOException, ClassNotFoundException, ClassCastException {
        Dados d;
        FileInputStream in = new FileInputStream(new File(nomeFicheiro));
        ObjectInputStream o = new ObjectInputStream(in);
        d = (Dados) o.readObject();
        o.close();
        in.close();
        d.updateEncs();
        return d;
    }

    /**
     * Metodo que guarda os dados num ficheiro
     *
     * @param nomeFicheiro nome do ficheiro
     */
    public void guardaFicheiro(String nomeFicheiro) throws FileNotFoundException, IOException {
        FileOutputStream out = new FileOutputStream(new File(nomeFicheiro));
        ObjectOutputStream o = new ObjectOutputStream(out);
        o.writeObject(this);
        o.flush();
        o.close();
        out.close();
    }

    /**
     * Metodo que retorna o utilizador correspondente ao email
     *
     * @param email nome do ficheiro
     * @return utilizador lido
     */
    public Utilizador emailToUser(String email) {
        Map<String, Utilizador> aux = this.getUtilizadores();
        return aux.get(email);
    }

    /**
     * Metodo que retorna o voluntarios correspondente ao email
     *
     * @param email nome do ficheiro
     * @return voluntario lido
     */
    public Voluntario emailToVolu(String email) {
        Map<String, Voluntario> aux = this.getVoluntarios();
        return aux.get(email);
    }

    /**
     * Metodo que retorna a transportadora correspondente ao email
     *
     * @param email nome do ficheiro
     * @return transportadora lida
     */
    public Transportadora emailToTran(String email) {
        Map<String, Transportadora> aux = this.getTransportadoras();
        return aux.get(email);
    }

    /**
     * Metodo que retorna a loja correspondente ao email
     *
     * @param email nome do ficheiro
     * @return loja lida
     */
    public Loja emailToLoja(String email) {
        Map<String, Loja> aux = this.getLojas();
        return aux.get(email);
    }

    /**
     * Metodo que verifica se existe o email
     *
     * @param email email a verificar
     */
    public void existeEmail(String email) throws NoEmailException {
        Utilizador u = this.utilizadores.get(email);
        if (u != null) return;
        Voluntario v = this.voluntarios.get(email);
        if (v != null) return;
        Transportadora t = this.transportadoras.get(email);
        if (t != null) return;
        Loja l = this.lojas.get(email);
        if (l != null) return;
        throw new NoEmailException(email);
    }

    /**
     * Metodo que verifica se o email e valido para registar
     *
     * @param email email a verificar
     */
    public void isEmail(String email) throws EmailInvalidoException {
        try {
            existeEmail(email);
        } catch (NoEmailException e) {
            if (!((email.contains(".com") || email.contains(".pt")) && email.contains("@")))
                throw new EmailInvalidoException(email);
        }

    }

    /**
     * Metodo que devolve o codigo do utilizador/voluntario/transportadora/loja dado o seu email
     *
     * @param email email do utilizador/voluntario/transportadora/loja
     * @return codigo obtido
     */
    public String emailToCod(String email) throws NoEmailException {
        Utilizador u = this.utilizadores.get(email);
        if (u != null) return u.getCodUser();
        Voluntario v = this.voluntarios.get(email);
        if (v != null) return v.getCodVol();
        Transportadora t = this.transportadoras.get(email);
        if (t != null) return t.getCodTran();
        Loja l = this.lojas.get(email);
        if (l != null) return l.getCodLoja();
        throw new NoEmailException(email);
    }

    /**
     * Metodo que retorna o utilizador correspondente ao codigo do utilizador
     *
     * @param codUser codigo do utilizador
     * @return utilizador correspondente
     */
    public Utilizador codUserToU(String codUser) {
        Utilizador ret = new Utilizador();
        for (Utilizador u : utilizadores.values())
            if (codUser.equals(u.getCodUser())) {
                ret = u.clone();
                break;
            }
        return ret;
    }

    /**
     * Metodo que retorna o voluntario correspondente ao codigo do voluntario
     *
     * @param codVolu codigo do voluntario
     * @return voluntario correspondente
     */
    public Voluntario codVolToV(String codVolu) {
        Voluntario ret = new Voluntario();
        for (Voluntario v : voluntarios.values())
            if (codVolu.equals(v.getCodVol())) {
                ret = v.clone();
                break;
            }
        return ret;
    }

    /**
     * Metodo que retorna a transportadora correspondente ao codigo da transportadora
     *
     * @param codTran codigo da transportadora
     * @return transportadora correspondente
     */
    public Transportadora codTranToT(String codTran) {
        Transportadora ret = new Transportadora();
        for (Transportadora t : transportadoras.values())
            if (codTran.equals(t.getCodTran())) {
                ret = t.clone();
                break;
            }
        return ret;
    }

    /**
     * Metodo que retorna a loja correspondente ao codigo da loja
     *
     * @param codLoja codigo da loja
     * @return loja correspondente
     */
    public Loja codLojaToL(String codLoja) {
        Loja ret = new Loja();
        for (Loja l : lojas.values())
            if (codLoja.equals(l.getCodLoja())) {
                ret = l.clone();
                break;
            }
        return ret;
    }

    /**
     * Metodo que aceita uma encomenda dos logs
     *
     * @param codEnc codigo da encomenda
     */
    public void encAceiteLogs(String codEnc) {
        int max = encomendas.size();
        int ind = -1;
        Encomenda e = new Encomenda();
        for (int i = 0; i < max; i++) {
            e = encomendas.get(i);
            if (codEnc.equals(e.getCodEnc())) {
                ind = i;
                break;
            }
        }
        Loja l = codLojaToL(e.getCodLoja());
        int vol = 0;
        for (Voluntario v : voluntarios.values()) {
            if (isInRaio(v.getRaio(), v.getGPSX(), v.getGPSY(), l.getGPSX(), l.getGPSY()) && v.getDisp() != 0) {
                e.setEstado(0);
                String data = LocalDateTime.now().toString();
                e.setData(data);
                encomendas.set(ind, e);
                vol = 1;
                List<Encomenda> encs = v.getEncomendas();
                encs.add(e);
                v.setEncomendas(encs);
                voluntarios.put(v.getEmail(), v);
                break;
            }
        }

        int tran = 0;
        if (vol == 0) {
            for (Transportadora t : transportadoras.values()) {
                if (isInRaio(t.getRaio(), t.getGPSX(), t.getGPSY(), l.getGPSX(), l.getGPSY()) && t.getDisp() != 0) {
                    e.setEstado(3);
                    encomendas.set(ind, e);
                    tran = 1;
                    List<Encomenda> encs = t.getEncomendas();
                    encs.add(e);
                    t.setEncomendas(encs);
                    transportadoras.put(t.getEmail(), t);
                    break;
                }
            }
        }

        if (tran == 0 && vol == 0) {
            System.out.println("Impossivel aceitar encomenda " + codEnc + "!\n");
        }
    }

    /**
     * Metodo que retorna a distancia entre uma encomenda e um voluntario
     *
     * @param e encomenda
     * @param v voluntario
     * @return distancia
     */
    public double getDisEncVol(Encomenda e, Voluntario v) {
        Loja l = codLojaToL(e.getCodLoja());
        return distanciaGPS(l.getGPSX(), l.getGPSY(), v.getGPSY(), v.getGPSY());
    }

    /**
     * Metodo que retorna a distancia entre uma encomenda e uma transportadora
     *
     * @param e encomenda
     * @param t transportadora
     * @return distancia
     */
    public double getDisEncTran(Encomenda e, Transportadora t) {
        Loja l = codLojaToL(e.getCodLoja());
        return distanciaGPS(l.getGPSX(), l.getGPSY(), t.getGPSY(), t.getGPSY());
    }

    /**
     * Metodo que verifica se duas passwords coincidirem
     *
     * @param password primeira password
     * @param pass     segunda password
     * @return 0 se coincidirem
     */
    public int passValida(String password, String pass) throws PassInvalidaException {
        if (password.equals(pass))
            return 0;
        else
            throw new PassInvalidaException(pass);
    }

    /**
     * Metodo que verifica se um nome e valido
     *
     * @param nome nome
     * @return 0 se for valido
     */
    public int nomeValido(String nome) throws NomeInvalidoException {
        int i;
        char c;

        for (i = 0; i < nome.length(); i++) {
            c = nome.charAt(i);
            if (!Character.isLetter(c) && c != ' ')
                throw new NomeInvalidoException(nome);
        }

        return 0;
    }

    /**
     * Metodo que verifica se um nif e valido
     *
     * @param nif nif
     * @return 0 se for valido
     */
    public int nifValido(long nif) throws NifInvalidoException {
        if (nif <= 999999999 && nif > 99999999) return 0;
        else throw new NifInvalidoException();
    }

    /**
     * Metodo que adiciona um utilizador aos dados
     *
     * @param u utilizador
     */
    public void addUser(Utilizador u){
        utilizadores.put(u.getEmail(), u);
    }

    /**
     * Metodo que adiciona um voluntario aos dados
     *
     * @param v voluntario
     */
    public void addVolu(Voluntario v) {
        voluntarios.put(v.getEmail(), v);
    }

    /**
     * Metodo que adiciona uma transportdora aos dados
     *
     * @param t transportadora
     */
    public void addTran(Transportadora t) {
        transportadoras.put(t.getEmail(), t);
    }

    /**
     * Metodo que adiciona uma loja aos dados
     *
     * @param l loja
     */
    public void addLoja(Loja l) {
        lojas.put(l.getEmail(), l);
    }

    /**
     * Metodo que gera um novo codigo de utilizador
     *
     * @return novo codigo
     */
    public String newCodUser() {
        int done = 1;
        String ret = null;
        int aux = 0;
        while (done != 0) {
            ret = "u" + aux;
            done = 0;
            for (Utilizador u : utilizadores.values()) {
                if (u.getCodUser().equals(ret)) {
                    aux++;
                    done = 1;
                    break;
                }
            }
        }
        return ret;
    }

    /**
     * Metodo que gera um novo codigo de voluntario
     *
     * @return novo codigo
     */
    public String newCodVolu() {
        int done = 1;
        String ret = null;
        int aux = 0;
        while (done != 0) {
            ret = "v" + aux;
            done = 0;
            for (Voluntario v : voluntarios.values()) {
                if (v.getCodVol().equals(ret)) {
                    aux++;
                    done = 1;
                    break;
                }
            }
        }
        return ret;
    }

    /**
     * Metodo que gera um novo codigo de transportadora
     *
     * @return novo codigo
     */
    public String newCodTran() {
        int done = 1;
        String ret = null;
        int aux = 0;
        while (done != 0) {
            ret = "t" + aux;
            done = 0;
            for (Transportadora t : transportadoras.values()) {
                if (t.getCodTran().equals(ret)) {
                    aux++;
                    done = 1;
                    break;
                }
            }
        }
        return ret;
    }

    /**
     * Metodo que gera um novo codigo de loja
     *
     * @return novo codigo
     */
    public String newCodLoja() {
        int done = 1;
        String ret = null;
        int aux = 0;
        while (done != 0) {
            ret = "l" + aux;
            done = 0;
            for (Loja l : lojas.values()) {
                if (l.getCodLoja().equals(ret)) {
                    aux++;
                    done = 1;
                    break;
                }
            }
        }
        return ret;
    }

    public String newCodEnc() {
        int done = 1;
        String ret = null;
        int aux = 0;
        while (done != 0) {
            ret = "e" + aux;
            done = 0;
            for(Encomenda e : encomendas) {
                if(e.getCodEnc().equals(ret)) {
                    aux++;
                    done = 1;
                    break;
                }
            }
        }
        return ret;
    }

    /**
     * Metodo que calcula as encomendas acessiveis a um Voluntario
     *
     * @param v Voluntario em causa
     * @return lista de encomendas acessiveis
     */
    public List<Encomenda> getEncomendasAcsVol(Voluntario v) {
        List<Loja> ljs = lojas
                .values()
                .stream()
                .filter(l -> isInRaio(v.getRaio(), v.getGPSX(), v.getGPSY(), l.getGPSX(), l.getGPSY()))
                .collect(Collectors.toList());

        List<Encomenda> ret = new ArrayList<>();
        for (Loja l : ljs) {
            //System.out.println(l);
            for (Encomenda e : l.getEncomendas()) {
                // System.out.println(e);
                if(e.getEstado() == 2) {
                    if(e.isMed()){
                        if(v.getDisp() == 2) ret.add(e.clone());
                    }else ret.add(e.clone());

                }

            }
        }
        return ret;
    }

    /**
     * Metodo que calcula as encomendas acessiveis a uma transportadora
     *
     * @param t Transportadora em causa
     * @return lista de encomendas acessiveis
     */
    public List<Encomenda> getEncomendasAcsTran(Transportadora t) {
        List<Loja> ljs = lojas
                .values()
                .stream()
                .filter(l -> isInRaio(t.getRaio(), t.getGPSX(), t.getGPSY(), l.getGPSX(), l.getGPSY()))
                .collect(Collectors.toList());

        List<Encomenda> ret = new ArrayList<>();
        for (Loja l : ljs) {
            //System.out.println(l);
            for (Encomenda e : l.getEncomendas()) {
                // System.out.println(e);
                if(e.getEstado() == 2){
                    if(e.isMed()){
                        if(t.getDisp() == 2) ret.add(e.clone());
                    }else ret.add(e.clone());
                }
            }
        }
        return ret;
    }

    /**
     * Metodo no qual uma transportadora escolhe uma encomenda
     *
     * @param t transportadora em causa
     */
    public void escolheEncTran(Transportadora t) {
        ArrayList<String> aux = new ArrayList<>();
        List<Encomenda> enc = getEncomendasAcsTran(t);
        if(enc.size() != 0) {
            for (Encomenda e : enc) {
                //System.out.println(e);
                aux.add(e.getCodEnc() + "(" + ((int) (getDisEncTran(e, t) + 0.5)) + "Km)");
            }
            String[] s = new String[aux.size()];
            for (int i = 0; i < aux.size(); i++) {
                s[i] = aux.get(i);
            }

            UInterface ui = new UInterface(s);
            System.out.println("\n\n\n\n\n\nEscolha encomenda a transportar:");
            int op = ui.exec();

            if (op != 0) {
                Encomenda e = enc.get(op - 1).clone();
                e.setEstado(3);
                int ind = 0;
                for (Encomenda ed : encomendas) {
                    if (e.getCodEnc().equals(ed.getCodEnc())) {
                        break;
                    }
                    ind++;
                }
                encomendas.set(ind, e);
                t.addEncomenda(e);
                transportadoras.put(t.getEmail(), t.clone());
                updateEncs();
                System.out.println("Encomenda selecionada!\n");
            }
        }else System.out.println("\nNao existem encomendas disponiveis!");
    }

    public String geraDataVol(Voluntario v, Loja l, Utilizador u){
        List<String> listMet = new ArrayList<>();
        listMet.add("Sol");
        listMet.add("Nublado");
        listMet.add("Chuva");
        listMet.add("Nevoeiro");
        listMet.add("Tempestade");
        Random rand = new Random();
        String meteo = listMet.get(rand.nextInt(listMet.size()));
        double dist = getDistTotalVol(v,l,u);
        double vel = 0;
        if(meteo.equals("Sol")) vel = 110;
        if(meteo.equals("Nublado")) vel = 90;
        if(meteo.equals("Chuva")) vel = 70;
        if(meteo.equals("Nevoeiro")) vel = 60;
        if(meteo.equals("Tempestade")) vel = 30;
        double tempo = dist/vel;
        int minutos = (int) tempo*60;
        LocalDateTime now = LocalDateTime.now().plusMinutes(minutos);
        return now.toString();
    }

    public String geraDataTran(Transportadora t, Loja l, Utilizador u){
        List<String> listMet = new ArrayList<>();
        listMet.add("Sol");
        listMet.add("Nublado");
        listMet.add("Chuva");
        listMet.add("Nevoeiro");
        listMet.add("Tempestade");
        Random rand = new Random();
        String meteo = listMet.get(rand.nextInt(listMet.size()));
        double dist = getDistTotalTran(t,l,u);
        double vel = 0;
        if(meteo.equals("Sol")) vel = 110;
        if(meteo.equals("Nublado")) vel = 90;
        if(meteo.equals("Chuva")) vel = 70;
        if(meteo.equals("Nevoeiro")) vel = 60;
        if(meteo.equals("Tempestade")) vel = 30;
        double tempo = dist/vel;
        int minutos = (int) tempo*60;
        LocalDateTime now = LocalDateTime.now().plusMinutes(minutos);
        return now.toString();
    }

    /**
     * Metodo no qual um voluntario escolhe uma encomenda
     *
     * @param v voluntario em causa
     */
    public void escolheEncVol(Voluntario v) {
        ArrayList<String> aux = new ArrayList<>();
        List<Encomenda> enc = getEncomendasAcsVol(v);
        if(enc.size() != 0) {
            for (Encomenda e : enc){
                aux.add(e.getCodEnc() + "(" + ((int) (getDisEncVol(e, v) + 0.5)) + "Km)");
            }
            String[] s = new String[aux.size()];
            for (int i = 0; i < aux.size(); i++) {
                s[i] = aux.get(i);
            }

            UInterface ui = new UInterface(s);
            System.out.println("\n\n\n\n\n\nEscolha encomenda a transportar:");
            int op = ui.exec();

            if(op != 0) {
                Encomenda e = enc.get(op - 1).clone();
                e.setEstado(0);
                Loja l = codLojaToL(e.getCodLoja());
                Utilizador u = codUserToU(e.getCodUser());
                String data = geraDataVol(v,l,u);
                e.setData(data);
                LocalDateTime ldt = LocalDateTime.parse(data);
                DateTimeFormatter formatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.SHORT, FormatStyle.SHORT);
                System.out.println("Chegada ao utilizador as: " + ldt.format(formatter));
                int ind = 0;
                for (Encomenda ed : encomendas) {
                    if (e.getCodEnc().equals(ed.getCodEnc())) {
                        break;
                    }
                    ind++;
                }
                encomendas.set(ind, e);
                v.addEncomenda(e);
                voluntarios.put(v.getEmail(), v.clone());
                updateEncs();
                System.out.println("Encomenda entregue!\n");
            }
        }else System.out.println("\nNao existem encomendas disponiveis!");
    }

    /**
     * Metodo que da update a todos os objetos necessarios consoante a lista de encomendas
     */
    public void updateEncs() {
        for (Voluntario v : voluntarios.values()) updateEncsVol(v);

        for (Transportadora t : transportadoras.values()) updateEncsTran(t);

        for (Utilizador u : utilizadores.values()) updateEncsUser(u);

        for (Loja l : lojas.values()) updateEncsLoja(l);
    }

    /**
     * Metodo que da update a todos os voluntarios consoante a lista de encomendas
     */
    public void updateEncsVol(Voluntario v) {
        List<Encomenda> encV = v.getEncomendas();
        for (Encomenda e : encomendas) {
            int ind2 = 0;
            for (Encomenda en : encV) {
                if (e.getCodEnc().equals(en.getCodEnc())) {
                    encV.set(ind2, e.clone());
                    v.setEncomendas(encV);
                    voluntarios.put(v.getEmail(), v.clone());
                    //System.out.println("++++++++++++++++++++++\n" + v);
                }
                ind2++;
            }
        }
    }

    /**
     * Metodo que da update a todos os utilizadores consoante a lista de encomendas
     */
    public void updateEncsUser(Utilizador u) {
        List<Encomenda> encU = u.getEncomendas();
        for (Encomenda e : encomendas) {
            int ind = 0;
            for (Encomenda en : encU) {
                if(e.getCodEnc().equals(en.getCodEnc())) {
                    encU.set(ind, e.clone());
                    u.setEncomendas(encU);
                    utilizadores.put(u.getEmail(), u.clone());
                    //System.out.println("++++++++++++++++++++++\n" + u);
                }
                ind++;
            }
        }
    }

    /**
     * Metodo que da update a todas as transportadoras consoante a lista de encomendas
     */
    public void updateEncsTran(Transportadora t) {
        List<Encomenda> encT = t.getEncomendas();
        for (Encomenda e : encomendas) {
            int ind = 0;
            for (Encomenda en : encT) {
                if (e.getCodEnc().equals(en.getCodEnc())) {
                    encT.set(ind, e.clone());
                    t.setEncomendas(encT);
                    transportadoras.put(t.getEmail(), t.clone());
                    //System.out.println("++++++++++++++++++++++\n" + t);
                }
                ind++;
            }
        }
    }

    /**
     * Metodo que da update a todas as lojas consoante a lista de encomendas
     */
    public void updateEncsLoja(Loja l) {
        List<Encomenda> encL = l.getEncomendas();
        for (Encomenda e : encomendas) {
            int ind = 0;
            for (Encomenda en : encL) {
                if (e.getCodEnc().equals(en.getCodEnc())) {
                    encL.set(ind, e.clone());
                    l.setEncomendas(encL);
                    lojas.put(l.getEmail(), l.clone());
                    //System.out.println("++++++++++++++++++++++\n" + l);
                }
                ind++;
            }
        }
    }

    public void isCodProd(String codProd) throws CodProdInvalidoException{
        if(codProd.charAt(0) != 'p') throw new CodProdInvalidoException(codProd);
        for(int i = 1; i < codProd.length(); i++)
            if(!isDigit(codProd.charAt(i)) || 3 < i ) throw new CodProdInvalidoException(codProd);
    }

    public ArrayList<LinhaEncomenda> criarProdutos(){
        int op = 1;
        ArrayList<LinhaEncomenda> ret= new ArrayList<>();
        String codProd = null;
        String desc = null;
        double qntd = 0.0;
        double valuni = 0.0;


        while(op != 0){
            LinhaEncomenda le = new LinhaEncomenda();
            int val = 1;
            Scanner in = new Scanner(System.in);
            while(val != 0){
                System.out.println("Insira codigo do produto (pXX)");
                codProd = in.nextLine();
                try{
                    isCodProd(codProd);

                    val = 0;
                }catch(CodProdInvalidoException e){
                    System.out.println("Valor invalido");
                    val = 1;
                }
            }

            val = 1;

            while(val != 0){
                System.out.println("Insira descricao do produto");
                desc = in.nextLine();
                try{
                    nomeValido(desc);
                    val = 0;
                }catch(NomeInvalidoException e){
                    System.out.println("Valor invalido");
                    val = 1;
                }
            }

            val = 1;

            while(val != 0){
                System.out.println("Insira a quantidade");
                try{
                    qntd = in.nextDouble();
                    val = 0;
                }catch(InputMismatchException e){
                    System.out.println("Valor invalido");
                    val = 1;
                }
            }

            val = 1;

            while(val != 0){
                System.out.println("Insira o valor unitario");
                valuni = -1;
                try{
                    valuni = in.nextInt();

                    val = 0;
                }catch(InputMismatchException e){
                    System.out.println("Valor invalido");
                    val = 1;
                }
            }
            le.setCodProduto(codProd);
            le.setDescricao(desc);
            le.setQuantidade(qntd);
            le.setValorUni(valuni);
            ret.add(le.clone());

            val = 1;

            int insmais = -1;
            while(val != 0){
                System.out.println("Inserir mais produtos?\n(0 - NAO) (1 - SIM)");
                try{
                    insmais = in.nextInt();
                    val = 0;
                }catch (InputMismatchException e){
                    System.out.println("Valor invalido");
                    val = 1;
                }
            }
            op = insmais;
        }
        return ret;
    }

    public void criarEncomenda(String codUser){
        Encomenda e = new Encomenda();
        e.setCodUser(codUser);
        int op = 1;
        Scanner inp = new Scanner(System.in);
        while(op != 0){
            System.out.println("(0 - Sair)\nInsira codigo da loja: ");
            String in = inp.nextLine();
            try {
                if (Integer.parseInt(in) == 0) return;
            }catch (NumberFormatException ignored){}
            Loja l = codLojaToL(in);
            if(l.getEmail() == null) System.out.println("Loja inexistente");
            else{
                e.setCodLoja(in);
                op = 0;
            }
        }

        op = 1;

        while(op != 0) {
            System.out.println("(0 - Sair)\nInsira o peso da encomenda: ");
            double in;
            try {
                in = inp.nextInt();
                if (in == 0) return;
                op = 0;
                e.setPeso((double)in);
            } catch (Exception ex) {
                System.out.println("Valor invalido!");
                op = 1;
            }
        }

        op = 1;

        System.out.println("\nEncomenda medica? (0 - SIM)");
        double in = -1;
        try{in = inp.nextInt();}catch (InputMismatchException ignored) {}
        if(in == 0)e.setMed(true);
        else e.setMed(false);
        e.setEstado(1);
        e.setCodEnc(newCodEnc());
        e.setProdutos(criarProdutos());
        encomendas.add(e);
        updateEncs();
    }


    /**
     * Metodo que informa o sistema que uma encomenda de uma loja esta pronta
     *
     * @param l loja em causa
     */
    public void reqEntrega(Loja l) {
        List<Encomenda> enc = l.getEncomendasEstado(1);
        if (enc.size() != 0){
            String[] encs = new String[enc.size()];
            int i = 0;
            for (Encomenda e : enc) {
                //System.out.println(e);
                encs[i] = e.getCodEnc();
                i++;
            }

            UInterface ui = new UInterface(encs);
            int op = ui.exec();
            if (op == 0) return;
            Encomenda es = enc.get(op - 1);
            es.setEstado(2);
            int indd = 0;
            for (Encomenda e : encomendas) {
                if (es.getCodEnc().equals(e.getCodEnc())) break;
                indd++;
            }
            encomendas.set(indd, es.clone());
            System.out.println("\nEncomenda disponibilizada com pronta para entrega!\n");

            //System.out.println("abcd");
        } else System.out.println("\nNao ha encomendas pendentes!\n");

        updateEncs();
    }

    public void veAvaliacoes(String codVolTran){
        int vol = 0;
        Voluntario v = codVolToV(codVolTran);
        Transportadora t = codTranToT(codVolTran);
        if(v.getEmail()==null){ vol = 1;}
        List<Encomenda> encs = new ArrayList<>();
        if(vol == 0){ encs = v.getEncomendasEstado(0);}
        else{encs = t.getEncomendasEstado(0);}

        Map<String, Double> classif = new TreeMap<>();

        for(String codE : classificacoes.keySet()){
            for(Encomenda e : encs){
                if(codE.equals(e.getCodEnc())){
                    classif.put(codE, classificacoes.get(codE));
                }
            }
        }
        double size = classif.size();
        if(size == 0) System.out.println("Nao existem classificacoes!");
        else{
            double media = 0;
            for(String e : classif.keySet()){
                Double d = classif.get(e);
                System.out.println("Encomenda " + e + " obteve classificacao de " + d);
                media += d;
            }
            media = media/size;
            System.out.println("Media: " + media);
        }

    }


    public void avaliaEncomenda(String codUser){
        Utilizador u = codUserToU(codUser);
        List<Encomenda> encs = u.getEncomendasEstado(0);

        for(Encomenda e : encs){
            for(String s : classificacoes.keySet()){
                if(s.equals(e.getCodEnc())){
                    encs.remove(e);
                }
            }
        }
        int i = 0;
        String[] str = new String[encs.size()];
        System.out.println("Escolha encomenda a avaliar:");
        for(Encomenda e : encs){
            str[i] = ("Encomenda " + e.getCodEnc() + " da loja " + e.getCodLoja());
            i++;
        }

        UInterface ui = new UInterface(str);
        int op = ui.exec();
        if(op != 0){
            Encomenda e = encs.get(op-1);
            System.out.println("Avalie a encomenda "+ e.getCodEnc() + " (0-5):");
            Scanner ss = new Scanner(System.in);
            double avaliacao = 0.0;
            int val = 1;
            while(val!=0) {
                try { avaliacao = ss.nextInt();
                    if(avaliacao > 5.0) avaliacao = 5.0;
                    val = 0;
                } catch (InputMismatchException g) {
                    System.out.print("Valor inválido!");
                    val = 1;
                }
            }
            classificacoes.put(e.getCodEnc(),avaliacao);
            System.out.println("Encomenda avaliada com sucesso!\n\n" + avaliacao);
        }
    }

    /**
     * Metodo imprime o historico de encomendas de um objeto do tipo utilizador, voluntario, transportadora, loja
     *
     * @param o objeto em questao
     */
    public void histEncomendas(Object o){
        List <Encomenda> encomendas = new ArrayList<>();
        if(o instanceof Utilizador){
            Utilizador u = (Utilizador) o;
            encomendas = u.getEncomendasEstado(0);
        }
        if(o instanceof Voluntario){
            Voluntario v = (Voluntario) o;
            encomendas = v.getEncomendasEstado(0);
        }
        if(o instanceof Transportadora){
            Transportadora t = (Transportadora) o;
            encomendas = t.getEncomendasEstado(0);
        }
        if(o instanceof Loja){
            Loja l = (Loja) o;
            encomendas = l.getEncomendasEstado(0);
        }

        Scanner input = new Scanner(System.in);

        int inp = 1;
        int op = 0;

        if(encomendas.size() != 0) {
            while (inp != 0) {
                System.out.println("(0) - Por data\n(1) - Todas");
                try {
                    op = input.nextInt();
                    inp = 0;
                } catch (InputMismatchException e) {
                    System.out.println("Valor invalido!");
                    inp = 1;
                }
            }
            if (op == 0) {
                input.nextLine();
                int val = 1;
                LocalDateTime date1 = null;
                LocalDateTime date2 = null;
                while (val != 0) {
                    System.out.print("\n(aaaa-mm-dd)\nEntre a data: \n");
                    String dateString = input.nextLine();
                    dateString += "T00:00:00";
                    try {
                        date1 = LocalDateTime.parse(dateString);
                        val = 0;
                    } catch (DateTimeParseException e) {
                        System.out.println("Data invalida!");
                        val = 1;
                    }
                }

                val = 1;

                while (val != 0) {
                    System.out.print("e a data :");
                    String dateString = input.nextLine();
                    dateString += "T00:00:00";
                    try {
                        date2 = LocalDateTime.parse(dateString);
                        val = 0;
                    } catch (DateTimeParseException e) {
                        System.out.println("Data invalida!");
                        val = 1;
                    }
                }

                List<Encomenda> encomendasInData = new ArrayList<>();
                LocalDateTime dataEnc;

                for (Encomenda e : encomendas) {
                    if (e.getData() != null) {
                        dataEnc = LocalDateTime.parse(e.getData());
                        if (dataEnc.isAfter(date1) && dataEnc.isBefore(date2)) encomendasInData.add(e);
                    }
                }
                int arrSize = encomendasInData.size();
                if (arrSize != 0) {
                    System.out.println("Encomendas:");
                    for (Encomenda e : encomendasInData)
                        System.out.println("-" + e.getCodEnc() + " (" + e.getDataPrint() + ")");
                } else System.out.println("\nNao existem encomendas terminadas nestas datas!\n");
            } else {
                System.out.println("Encomendas:");
                for (Encomenda e : encomendas) {
                    System.out.println("-" + e.getCodEnc() + " (" + e.getDataPrint() + ")");
                }
            }
        }else{
            System.out.println("\nNao existem encomendas terminadas!\n");
        }
        System.out.println("");
    }

    /**
     * Metodo que povoa alguns dados apos o import de logs
     */
    public void povoaDados() {
        for (Encomenda e : encomendas) {
            Utilizador u = codUserToU(e.getCodUser());
            int ind1 = 0;
            int indU = -1;
            for (Encomenda en : u.getEncomendas()) {
                if (e.getCodEnc().equals(en.getCodEnc())) {
                    List<Encomenda> encU = u.getEncomendas();
                    encU.set(ind1, e.clone());
                    u.setEncomendas(encU);
                    utilizadores.put(u.getEmail(), u.clone());
                    //System.out.println("++++++++++++++++++++++\n" + u);
                    indU = ind1;
                }
                ind1++;
            }
            if (indU == -1) {
                List<Encomenda> encU = u.getEncomendas();
                encU.add(e.clone());
                u.setEncomendas(encU);
                utilizadores.put(u.getEmail(), u);
            }

            Voluntario v = codVolToV(e.getCodUser());
            int ind2 = 0;
            for (Encomenda en : v.getEncomendas()) {
                if (e.getCodEnc().equals(en.getCodEnc())) {
                    List<Encomenda> encV = v.getEncomendas();
                    encV.set(ind2, e.clone());
                    v.setEncomendas(encV);
                    voluntarios.put(v.getEmail(), v.clone());
                    //System.out.println("++++++++++++++++++++++\n" + v);
                }
                ind2++;
            }

            Transportadora t = codTranToT(e.getCodUser());
            int ind3 = 0;
            for (Encomenda en : t.getEncomendas()) {
                if (e.getCodEnc().equals(en.getCodEnc())) {
                    List<Encomenda> encT = t.getEncomendas();
                    encT.set(ind3, e.clone());
                    t.setEncomendas(encT);
                    transportadoras.put(t.getEmail(), t.clone());
                    //System.out.println("++++++++++++++++++++++\n" + t);
                }
                ind3++;
            }

            Loja l = codLojaToL(e.getCodLoja());
            int ind4 = 0;
            int indL = -1;
            for (Encomenda en : l.getEncomendas()) {
                if (e.getCodEnc().equals(en.getCodEnc())) {
                    List<Encomenda> encL = l.getEncomendas();
                    encL.set(ind4, e.clone());
                    l.setEncomendas(encL);
                    lojas.put(l.getEmail(), l);
                    indL = ind4;
                }
                ind4++;
            }
            if (indL == -1) {
                List<Encomenda> encL = l.getEncomendas();
                ;
                encL.add(e.clone());
                l.setEncomendas(encL);
                lojas.put(l.getEmail(), l);
            }
        }
    }

    /**
     * Metodo que faz parse dos logs para a estrutura dados
     *
     * @param file ficheiro de logs
     */
    public void parse(String file) {
        int pov = 0;
        List<String> linhas = lerFicheiro(file); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    //System.out.println(u.toString()); //enviar para o ecra apenas para teste
                    utilizadores.put(u.getEmail(), u);
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    //System.out.println(l.toString());
                    lojas.put(l.getEmail(), l);
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    voluntarios.put(v.getEmail(), v);
                    //System.out.println(v.toString());
                    povoaDados();
                    break;
                case "Transportadora":
                    Transportadora t = parseTransportadora(linhaPartida[1]);
                    transportadoras.put(t.getEmail(), t);
                    //System.out.println(t.toString());
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    //System.out.println(e.toString());
                    encomendas.add(e);
                    break;
                case "Aceite":
                    if (pov == 0) povoaDados();
                    encAceiteLogs(linhaPartida[1]);
                    pov = 1;
                    break;
                default:
                    System.out.println("Linha invalida.");
                    break;
            }

        }
        updateEncs();
        System.out.println("\n\n\n\nLogs importados com sucesso!\n");
    }

    /**
     * Metodo que faz parse de uma linha de logs correspondente a um utilizador
     *
     * @param input linha de logs
     * @return utilizador obtido
     */
    public Utilizador parseUtilizador(String input) {
        Utilizador u = new Utilizador();
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsX = Double.parseDouble(campos[2]);
        double gpsY = Double.parseDouble(campos[3]);
        String password = nome.replace(" ", "").toLowerCase();
        String email = password + "@gmail.com";
        u.setCodUser(codUtilizador);
        u.setNome(nome);
        u.setGPS(gpsX, gpsY);
        u.setEmail(email);
        u.setPassword(password);
        return u;
    }

    /**
     * Metodo que faz parse de uma linha de logs correspondente a um voluntario
     *
     * @param input linha de logs
     * @return voluntario obtido
     */
    public Voluntario parseVoluntario(String input) {
        Voluntario v = new Voluntario();
        String[] campos = input.split(",");
        String codVol = campos[0];
        String nome = campos[1];
        double gpsX = Double.parseDouble(campos[2]);
        double gpsY = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        String password = nome.replace(" ", "").toLowerCase();
        String email = password + "@gmail.com";
        v.setCodVol(codVol);
        v.setNome(nome);
        v.setGPS(gpsX, gpsY);
        v.setEmail(email);
        v.setPassword(password);
        v.setRaio(raio);
        return v;
    }

    /**
     * Metodo que faz parse de uma linha de logs correspondente a uma transportadora
     *
     * @param input linha de logs
     * @return transportadora obtida
     */
    public Transportadora parseTransportadora(String input) {
        Transportadora t = new Transportadora();
        String[] campos = input.split(",");
        String codTran = campos[0];
        String nome = campos[1];
        double gpsX = Double.parseDouble(campos[2]);
        double gpsY = Double.parseDouble(campos[3]);
        long nif = Long.parseLong(campos[4]);
        double raio = Double.parseDouble(campos[5]);
        double precokm = Double.parseDouble(campos[6]);
        String password = nome.replace(" ", "").toLowerCase();
        String email = password + "@gmail.com";
        t.setCodTran(codTran);
        t.setNome(nome);
        t.setGPS(gpsX, gpsY);
        t.setNIF(nif);
        t.setRaio(raio);
        t.setPrecoKM(precokm);
        t.setEmail(email);
        t.setPassword(password);
        return t;
    }

    /**
     * Metodo que faz parse de uma linha de logs correspondente a uma loja
     *
     * @param input linha de logs
     * @return loja obtida
     */
    public Loja parseLoja(String input) {
        double x = ThreadLocalRandom.current().nextDouble(-100, 100);
        double y = ThreadLocalRandom.current().nextDouble(-100, 100);
        Loja l = new Loja();
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nome = campos[1];
        String password = nome.replace(" ", "").toLowerCase();
        String email = password + "@gmail.com";
        l.setCodLoja(codLoja);
        l.setnome(nome);
        l.setGPS(x, y);
        l.setEmail(email);
        l.setPassword(password);
        return l;
    }

    /**
     * Metodo que faz parse de uma linha de logs correspondente a uma lista de produtos
     *
     * @param input linha de logs
     * @return lista de produtos obtida
     */
    public ArrayList<LinhaEncomenda> parseProdutos(String input) {
        String[] campos = input.split(",");

        ArrayList<LinhaEncomenda> produtos = new ArrayList<>();

        int camposLength = campos.length;

        int done = 0;
        for (int i = 0; done != 1; i += 4) {
            LinhaEncomenda le = new LinhaEncomenda();
            String codProduto = campos[i];
            String descricao = campos[i + 1];
            double quantidade = Double.parseDouble(campos[i + 2]);
            double valorUni = Double.parseDouble(campos[i + 3]);
            le.setCodProduto(codProduto);
            le.setDescricao(descricao);
            le.setQuantidade(quantidade);
            le.setValorUni(valorUni);
            produtos.add(le);
            if (i + 4 >= camposLength) done = 1;
        }
        return produtos;
    }

    /**
     * Metodo que faz parse de uma linha de logs correspondente a uma encomenda
     *
     * @param input linha de logs
     * @return encomenda obtida
     */
    public Encomenda parseEncomenda(String input) {
        Encomenda e = new Encomenda();
        String[] campos = input.split(",", 5);
        String codEnc = campos[0];
        String codUser = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        ArrayList<LinhaEncomenda> produtos = parseProdutos(campos[4]);
        e.setCodEnc(codEnc);
        e.setCodUser(codUser);
        e.setCodLoja(codLoja);
        e.setPeso(peso);
        e.setProdutos(produtos);
        return e;
    }

    /**
     * Metodo le todas as linhas de um ficheiro
     *
     * @param nomeFich nome do ficheiro
     * @return linhas obtidas
     */
    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
        }
        return lines;
    }

    /**
     * Metodo que calcula a distancia entre duas entidades
     *
     * @param gpsX1 coordenada x da primeira entidade
     * @param gpsY1 coordenada y da primeira entidade
     * @param gpsX2 coordenada x da segunda entidade
     * @param gpsY2 coordenada y da segunda entidade
     * @return distancia
     */
    public static double distanciaGPS(double gpsX1, double gpsY1, double gpsX2, double gpsY2) {
        return Math.sqrt(Math.pow((gpsX2 - gpsX1 + gpsY2 - gpsY1), 2.0));
    }

    /**
     * Metodo que calcula se uma entidade se encontra no raio de outra
     *
     * @param raio  raio
     * @param gpsX1 coordenada x da primeira entidade
     * @param gpsY1 coordenada y da primeira entidade
     * @param gpsX2 coordenada x da segunda entidade
     * @param gpsY2 coordenada y da segunda entidade
     * @return true se estiver, false se nao estiver
     */
    public static boolean isInRaio(double raio, double gpsX1, double gpsY1, double gpsX2, double gpsY2) {
        return distanciaGPS(gpsX1, gpsY1, gpsX2, gpsY2) <= raio;
    }

    /**
     * Metodo que verifica se existe uma resposta pendente de um utilizador
     *
     * @param u utilizador em causa
     * @return true se existir, false se nao existir
     */
    public boolean existeRespPend(Utilizador u){
        List<Encomenda> aux = u.getEncomendasEstado(3);
        return aux.size() != 0;
    }

    public Transportadora getTranEnc(String codEnc){
        for(Transportadora t : transportadoras.values()){
            List<Encomenda> aux = t.getEncomendas();
            for(Encomenda e : aux){
                if(e.getCodEnc().equals(codEnc)) return t.clone();
            }
        }
        return null;
    }

    public double getDistTotalTran(Transportadora t, Loja l, Utilizador u){
        return distanciaGPS(t.getGPSX(),t.getGPSY(),l.getGPSX(),l.getGPSY()) + distanciaGPS(l.getGPSX(),l.getGPSY(),u.getGPSX(),u.getGPSY());
    }

    public double getDistTotalVol(Voluntario v, Loja l, Utilizador u){
        return distanciaGPS(v.getGPSX(),v.getGPSY(),l.getGPSX(),l.getGPSY()) + distanciaGPS(l.getGPSX(),l.getGPSY(),u.getGPSX(),u.getGPSY());
    }

    public double getPreco(double peso, Transportadora t, Loja l, Utilizador u){
        double custoKM = t.getPrecoKM();
        return (peso/1.5)*(getDistTotalTran(t,l,u)*custoKM);
    }

    /**
     * Metodo que gere uma resposta pendesnte de um utilizador
     *
     * @param u utilizador em causa
     */
    public void gereRespPend(Utilizador u){
        List<Encomenda> aux = u.getEncomendasEstado(3);
        for(Encomenda e : aux){
            Transportadora t = getTranEnc(e.getCodEnc());
            Loja l = codLojaToL(e.getCodLoja());
            double precoD = getPreco(e.getPeso(),t,l,u);
            String preco = String.format("%.02d", precoD);
            System.out.println("Encomenda: " + e.getCodEnc() + "\nTransportadora: " + t.getCodTran() + "\nPreco esperado: " + preco);
            System.out.println("(0) - Recusar\n(1) - Aceitar");
            int op = 0;
            int inp = 1;
            Scanner s = new Scanner(System.in);
            while(inp != 0) {
                try{
                    op = s.nextInt();
                    inp = 0;
                }catch(InputMismatchException i) {
                    System.out.println("Valor invalido!");
                    inp = 1;
                }
            }
            if(op == 0){
                List<Encomenda> auxt = t.getEncomendas();
                auxt.remove(e);
                t.setEncomendas(auxt);
                transportadoras.put(t.getEmail(),t.clone());
                e.setEstado(2);
            }else{
                e.setEstado(0);
                e.setData(geraDataTran(t,l,u));
                e.setPrecoEntrega(precoD);
            }
            int ind = 0;
            for(Encomenda ed : encomendas){
                if(e.getCodEnc().equals(ed.getCodEnc())){
                    break;
                }
                ind++;
            }
            encomendas.set(ind,e.clone());
            updateEncs();
        }
    }

    public void totFatTrans(){
        List<Transportadora> trans = new ArrayList<>(transportadoras.values());
        String[] str = new String[trans.size()];
        int ind = 0;
        for(Transportadora t : trans){
            str[ind] = t.getCodTran();
            ind++;
        }

        while(true){
            UInterface ui = new UInterface(str);
            int op = ui.exec();
            if(op != 0){
                Transportadora tp = trans.get(op-1);
                System.out.println("Total faturado pela transportadora " + tp.getCodTran() + " e: " + totFatTran(tp) + "€");
            }else break;
        }

    }

    public double totFatTran(Transportadora t){
        double total = 0;
        List<Encomenda> encs = t.getEncomendas();
        for(Encomenda e : encs){
            if(e.getEstado() == 0) total += e.getPrecoEntrega();
        }
        return total;
    }

    public void dezUsrMaisUsaram(){
        List<Utilizador> usrs = new ArrayList<>(utilizadores.values());
        usrs.sort(UserComparator);
        int i = 1;
        for(Utilizador u : usrs){
            if(i > 10) break;
            System.out.println(i + " - " + u.getNome() + "(" + u.getEncomendasEstado(0).size() + " Encomendas)");
            i++;
        }
        System.out.println("\n");
    }

    public void dezTranMaisUsaram(){
        Map<Double, Transportadora> mapTran = new TreeMap<>();
        for(Transportadora t : transportadoras.values()){
            System.out.println(getKmDone(t));
            double kmd = getKmDone(t);
            while(mapTran.containsKey(kmd)){
                kmd += 0.00001;
            }
            mapTran.put(kmd,t.clone());
        }

        List<Double> sortedKeys = new ArrayList<>(mapTran.keySet());
        sortedKeys.sort(Collections.reverseOrder());

        List<Transportadora> trans = new ArrayList<>();

        for(Double d : sortedKeys){
            trans.add(mapTran.get(d));
        }

        int i = 1;
        for(Transportadora t : trans){
            if(i > 10) break;
            System.out.println(i + " - " + t.getNome() + "(" + getKmDone(t) + " KM)");
            i++;
        }
        System.out.println("\n");
    }

    public void verPerfilTran(Transportadora o){
        int opcao = -1;
        while(opcao != 0){
            System.out.print(o.toString());
            System.out.print("\n(1) - Ficar indisponivel" +
                    "\n(2) - Ficar disponivel" +
                    "\n(3) - Ficar disponivel para encomendas medicas" +
                    "\n(0) - Sair\n");
            Scanner input = new Scanner(System.in);

            try{
                opcao = input.nextInt();
            }
            catch(InputMismatchException e){ // Não foi escolhido um inteiro
                opcao = -1;
            }
            if(opcao < 0 || opcao > 3){
                System.out.println("Valor Inválido!");
                opcao = -1;
            }
            if(opcao == 1){
                o.setDisp(0);
            }
            if(opcao == 2){
                o.setDisp(1);
            }
            if(opcao == 3){
                o.setDisp(2);
            }
        }
        transportadoras.put(o.getEmail(),o.clone());
    }

    public void verPerfilVol(Voluntario o){
        int opcao = -1;
        while(opcao != 0){
            System.out.print(o.toString());
            System.out.print("\n(1) - Ficar indisponivel" +
                    "\n(2) - Ficar disponivel" +
                    "\n(3) - Ficar disponivel para encomendas medicas" +
                    "\n(0) - Sair\n");
            Scanner input = new Scanner(System.in);

            try{
                opcao = input.nextInt();
            }
            catch(InputMismatchException e){ // Não foi escolhido um inteiro
                opcao = -1;
            }
            if(opcao < 0 || opcao > 3){
                System.out.println("Valor Inválido!");
                opcao = -1;
            }
            if(opcao == 1){
                o.setDisp(0);
            }
            if(opcao == 2){
                o.setDisp(1);
            }
            if(opcao == 3){
                o.setDisp(2);
            }
        }
        voluntarios.put(o.getEmail(),o.clone());
    }

    public double getKmDone(Transportadora t){
        List<Encomenda> encsT = t.getEncomendasEstado(0);
        double kms = 0;
        for(Encomenda e : encsT){
            if(e.getEstado() == 0) {
                Utilizador u = codUserToU(e.getCodUser());
                Loja l = codLojaToL(e.getCodLoja());
                kms += getPreco(e.getPeso(),t, l, u);
            }
        }
        return kms;
    }


    public List<Encomenda> encAccessiveisVol(Voluntario v) {
        List<Encomenda> ret = new ArrayList<>();
        for (Loja lj : lojas.values()) {
            if (isInRaio(v.getRaio(), v.getGPSX(), v.getGPSY(), lj.getGPSX(), lj.getGPSY()))
                ret = lj.getEncomendas();
        }
        return ret;
    }

    public List<Encomenda> encAccessiveisTra(Transportadora t) {
        List<Encomenda> ret = new ArrayList<>();
        for (Loja lj : lojas.values()) {
            if (isInRaio(t.getRaio(), t.getGPSX(), t.getGPSY(), lj.getGPSX(), lj.getGPSY()))
                ret = lj.getEncomendas();
        }
        return ret;
    }

    public static Comparator<Utilizador> UserComparator = new Comparator<Utilizador>(){

        public int compare(Utilizador u1, Utilizador u2) {
            int nEncsu1 = u1.getEncomendasEstado(0).size();
            int nEncsu2 = u2.getEncomendasEstado(0).size();

            return nEncsu2 - nEncsu1;

        }
    };

}