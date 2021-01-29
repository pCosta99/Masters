/**
 * Escreva a descrição da classe Infos aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.Scanner;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.TreeSet;
import java.util.Comparator;
import java.util.Iterator;
import java.time.LocalDateTime;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.time.Duration;
public class Infos implements Serializable
{
    // String = codigo da Empresa Transportadora
    private Map<String,Transportadora> transportadoras;
    // String = codigo do Voluntario
    private Map<String,Voluntario> voluntarios;
    // String = codigo da Loja
    private Map<String,Loja> lojas;
    // String = codigo do Utilizador
    private Map<String,Utilizador> utilizadores;
    // String = codigo da Encomenda
    private Map<String,Encomenda> encomendas;
    // Lista com os codigos das encomendas aceites
    private List<EncomendasAceites> encomendasAceites;
    // String = codigo da empresa Transportadora
    private Map<String,List<Encomenda>> encomendasTransportadora;
    // String = codigo do Voluntario
    private Map<String,Encomenda> encomendasVoluntario;
    // String = codigo da encomenda
    private List<String> encomendasMedicas;
    // String = codigo da encomenda
    private Map<String,Encomenda> encomendasProntas;
    // lista dos voluntarios que estao disponiveis
    private List<Voluntario> voluntariosDisponiveis;
    // lista das transportadoras que estao disponiveis
    private List<Transportadora> transportadorasDisponiveis;
    // lista dos codigos das transportadoras que nao estao disponiveis
    private List<String> transportadorasIndisponiveis;
    // string = codigo da encomenda
    private Map<String,LocalDateTime> encEntregueData;
    // string = codigo da encomenda
    private Map<String,Duration> encEntregueDuracao;
    // string = codigo do utilizador e lista de codigos das encomendas por aceitar
    private Map<String,List<String>> encsPorAceitarU;
    
    public Infos(){
        this.transportadoras = new HashMap<String,Transportadora>();
        this.voluntarios = new HashMap<String,Voluntario>();
        this.lojas = new HashMap<String,Loja>();
        this.utilizadores = new HashMap<String,Utilizador>();
        this.encomendas = new HashMap<String,Encomenda>();
        this.encomendasAceites = new ArrayList<>();
        this.encomendasTransportadora = new HashMap<>();
        this.encomendasVoluntario = new HashMap<>();
        this.encomendasMedicas = new ArrayList<>();
        this.encomendasProntas = new HashMap<>();
        this.voluntariosDisponiveis = new ArrayList<>();
        this.transportadorasDisponiveis = new ArrayList<>();
        this.transportadorasIndisponiveis = new ArrayList<>();
        this.encEntregueData = new HashMap<>();
        this.encEntregueDuracao = new HashMap<>();
        this.encsPorAceitarU = new HashMap<>();
    }
    
    public Infos(Map<String,Transportadora> transportadoras, Map<String,Voluntario> voluntarios,
                 Map<String,Loja> lojas, Map<String,Utilizador> utilizadores,
                 Map<String,Encomenda> encomendas, List<EncomendasAceites> ea,
                 Map<String,List<Encomenda>> eT, Map<String,Encomenda> eV,
                 List<String> em, Map<String,Encomenda> eP, List<Voluntario> vD,
                 List<Transportadora> tD, List<String> tI,
                 Map<String,LocalDateTime> eED, Map<String,Duration> eEDur,
                 Map<String,List<String>> encsU){
        this.transportadoras = new HashMap<String,Transportadora>();
        for (Transportadora t: transportadoras.values()){
            this.transportadoras.put(t.getCodE(),t.clone());
        }
        this.voluntarios = new HashMap<String,Voluntario>();
        for (Voluntario v: voluntarios.values()){
            this.voluntarios.put(v.getCodV(),v.clone());
        }
        this.lojas = new HashMap<String,Loja>();
        for (Loja l: lojas.values()){
            this.lojas.put(l.getCodL(),l.clone());
        }
        this.utilizadores = new HashMap<String,Utilizador>();
        for (Utilizador u: utilizadores.values()){
            this.utilizadores.put(u.getCodU(),u.clone());
        }
        this.encomendas = new HashMap<String,Encomenda>();
        for (Encomenda e: encomendas.values()){
            this.encomendas.put(e.getCodEnc(),e.clone());
        }
        this.encomendasAceites = new ArrayList<>();
        for (EncomendasAceites e : ea){
            this.encomendasAceites.add(e.clone());
        }
        this.encomendasTransportadora = new HashMap<>();
        for (Map.Entry<String,List<Encomenda>> e : eT.entrySet()){
            this.encomendasTransportadora.put(e.getKey(),e.getValue());
        }
        this.encomendasVoluntario = new HashMap<>();
        for (Map.Entry<String,Encomenda> e : eV.entrySet()){
            this.encomendasVoluntario.put(e.getKey(),e.getValue().clone());
        }
        this.encomendasMedicas = new ArrayList<>();
        for (String cod : em){
            this.encomendasMedicas.add(cod);
        }
        this.encomendasProntas = new HashMap<>();
        for (Map.Entry<String,Encomenda> ent : eP.entrySet()){
            this.encomendasProntas.put(ent.getKey(),ent.getValue().clone());
        }
        this.voluntariosDisponiveis = new ArrayList<>();
        for (Voluntario v : vD){
            this.voluntariosDisponiveis.add(v.clone());
        }
        this.transportadorasDisponiveis = new ArrayList<>();
        for (Transportadora t : tD){
            this.transportadorasDisponiveis.add(t.clone());
        }
        this.transportadorasIndisponiveis = new ArrayList<>();
        for (String s : tI){
            this.transportadorasIndisponiveis.add(s);
        }
        this.encEntregueData = new HashMap<>();
        for (Map.Entry<String,LocalDateTime> entry : eED.entrySet()){
            this.encEntregueData.put(entry.getKey(),entry.getValue());
        }
        this.encEntregueDuracao = new HashMap<>();
        for (Map.Entry<String,Duration> entry : eEDur.entrySet()){
            this.encEntregueDuracao.put(entry.getKey(),entry.getValue());
        }
        this.encsPorAceitarU = new HashMap<>();
        for (Map.Entry<String,List<String>> entry : encsU.entrySet()){
            this.encsPorAceitarU.put(entry.getKey(),entry.getValue());
        }
    }
    
    public static Infos carregaObjetoInfos() throws IOException, ClassNotFoundException, FileNotFoundException
    {
        FileInputStream carregaFicheiro = new FileInputStream("Informacoes");
        if (carregaFicheiro == null) {
            throw new FileNotFoundException("");
        } else {
            ObjectInputStream obj = new ObjectInputStream(carregaFicheiro);
            if (obj == null) {
                throw new IOException("");
            } else {
                Infos i = (Infos) obj.readObject();
                if (i == null) {
                    throw new ClassNotFoundException("");
                } else {
                    obj.close();
                    return i;
                }
            }
        }
    }

    /**
     * Guarda num ficheiro um objecto da classe Informacao (esse ficheiro tem sempre o nome BaseDados)
     */
    public void gravaObjetoInfos() throws IOException, FileNotFoundException
    {
        FileOutputStream guardaFicheiro = new FileOutputStream("Informacoes");
        if (guardaFicheiro == null) {
            throw new FileNotFoundException("");
        } else {
            ObjectOutputStream objeto = new ObjectOutputStream(guardaFicheiro);
            if (objeto == null) {
                throw new IOException("");
            } else {
                objeto.writeObject(this);
                objeto.flush();
                objeto.close();
            }
        }
    }
    
    // metodo que devolve as 10 transportadoras que mais utilizam a aplicacao em funcao do numero de kms percorridos
    public Set<Transportadora> transportadorasMaisFreqs(){
        TreeSet transportadoras = new TreeSet<Transportadora>(new Comparator<Transportadora>(){
            public int compare (Transportadora t1, Transportadora t2){
                if (numKmsT(t1.getCodE()) > numKmsT(t2.getCodE())) return 1;
                else if (numKmsT(t1.getCodE()) < numKmsT(t2.getCodE())) return -1;
                else return compareCodigosTransportadoras(t1,t2);
            }
        });
        Iterator<Map.Entry<String,Transportadora>> it = this.transportadoras.entrySet().iterator();
        while(it.hasNext()){
            transportadoras.add(it.next().getValue().clone());
        }
        while(transportadoras.size() > 10){
            transportadoras.remove(transportadoras.first());
        }
        return transportadoras;
    }
    
    // metodo que devolve o numero de kms percorridos por uma transportadora
    public double numKmsT(String codE){
        double total = 0.0;
        Utilizador u = null;
        Loja l = null;
        List<Encomenda> encs = getEncomendasTransportadora(codE);
        for (Encomenda e : encs){
            u = getUtilizador(e.getCodU());
            l = getLoja(e.getCodL());
            total += distanciaLojaT(codE,e.getCodL()) + distancia(u.getCoordX(),u.getCoordY(),l.getCoordX(),l.getCoordY());
        }
        return total;
    }
    
    // metodo que devolve os 10 utilizadores que mais utilizam a aplicacao em funcao do numero de encomendas efetuadas
    public Set<Utilizador> utilizadoresMaisFreqs(){
        TreeSet users = new TreeSet<Utilizador>(new Comparator<Utilizador>(){
            public int compare (Utilizador u1, Utilizador u2){
                if (numEncsUsers(u1.getCodU()) > numEncsUsers(u2.getCodU())) return 1;
                else if (numEncsUsers(u1.getCodU()) < numEncsUsers(u2.getCodU())) return -1;
                else return compareNomeUsers(u1,u2);
            }
        });
        Iterator<Map.Entry<String,Utilizador>> it = this.utilizadores.entrySet().iterator();
        while(it.hasNext()){
            users.add(it.next().getValue().clone());
        }
        while(users.size() > 10){
            users.remove(users.first());
        }
        return users;
    }
    
    // metodo que compara os codigos de duas transportadoras
    public int compareCodigosTransportadoras(Transportadora t1, Transportadora t2){
        if (t1.getCodE().compareTo(t2.getCodE())<0) return 1;
        if (t1.getCodE().compareTo(t2.getCodE())>0) return -1;
        return 0;
    }
    
    // metodo que devolve o numero de encomendas por utilizador
    public int numEncsUsers(String codU){
        int numEncs = 0;
        for (Encomenda e : this.encomendas.values()){
            if (e.getCodU().equals(codU)){
                numEncs += 1;
            }
        }
        return numEncs;
    }
    
    // metodo que compara o numero de encomendas entre 2 utilizadores
    public int compareNumEncs(Utilizador u1, Utilizador u2){
        if (numEncsUsers(u1.getCodU())<numEncsUsers(u2.getCodU())) return 1;
        if (numEncsUsers(u2.getCodU())<numEncsUsers(u1.getCodU())) return -1;
        return compareNomeUsers(u1,u2);
    }
    
    // metodo que compara o nome dos utilizadores (apenas usada caso 2 utilizadores tenham efetuado o mesmo numero de encomendas)
    public int compareNomeUsers(Utilizador u1, Utilizador u2){
        if (u1.getNomeU().compareTo(u2.getNomeU())<0) return 1;
        if (u1.getNomeU().compareTo(u2.getNomeU())>0) return -1;
        return 0;
    }
    
    // metodo que devolve uma lista com as encomendas de um utilizador
    public List<Encomenda> s(String codU){
        List<Encomenda> encs = new ArrayList<>();
        for (Encomenda e : this.encomendas.values()){
            if (e.getCodU().equals(codU)){
                encs.add(e.clone());
            }
        }
        return encs;
    }
    
    // metodo que devolve um map que associa o codigo de uma loja as encomendas dessa mesma loja
    public Map<String,Encomenda> encomendasLoja(String codL) throws Exception{
        Map<String,Encomenda> encs = new HashMap<>();
        for(Encomenda e: this.encomendas.values()){
            if (e.getCodL().equals(codL)){
                encs.put(codL,e.clone());
            }
        }
        if (encs == null) throw new Exception ("Loja "+codL+" nao existe");
        return encs;
    }
    
    // metodo que devolve a distancia entre dois pontos
    public double distancia (double x, double y, double cx, double cy){
        double res = 0.0;
        double aux1 = cx - x;
        double aux2 = cy - y;
        res = Math.sqrt(Math.pow(aux1,2) + Math.pow(aux2,2));
        return res;
    }
    
    // metodo que determina a distancia ate a uma loja, passada como parametro, por parte de um voluntario ou de uma transportadora
    public double distanciaLojaV(String cod, String codL){
        double dist = 0.0;
        Voluntario v = this.voluntarios.get(cod).clone();
        System.out.println(v.toString());
        Loja l = this.lojas.get(codL);
        if (v == null){
            System.out.println("Voluntario nao existe");
        }
        else {
            dist = distancia(v.getCoordX(),v.getCoordY(),l.getCoordX(),l.getCoordY());
            return dist;
        }
        return dist;
    }
    
    public double distanciaLojaT(String cod, String codL){
        double dist = 0.0;
        Transportadora t = this.transportadoras.get(cod).clone();
        Loja l = this.lojas.get(codL);
        if (t == null){
            System.out.println("Transportadora nao existe");
        }
        else {
            dist = distancia(t.getCoordX(),t.getCoordY(),l.getCoordX(),l.getCoordY());
            return dist;
        }
        return dist;
    }
    
    // metodo que determina a distancia ate um utilizador, passado como parametro, por parte de um voluntario ou de uma transportadora
    public double distanciaUser(String cod, String codU){
        double dist = 0.0;
        Voluntario v = this.voluntarios.get(cod);
        Utilizador u = this.utilizadores.get(codU);
        if (v == null){
            Transportadora t = this.transportadoras.get(cod).clone();
            if (t != null){
                dist = distancia(t.getCoordX(),t.getCoordY(),u.getCoordX(),u.getCoordY());
            }
        }
        else {
            dist = distancia(v.getCoordX(),v.getCoordY(),u.getCoordX(),u.getCoordY());
        }
        return dist;
    }
    
    // metodo que verifica se uma encomenda esta dentro do raio de um voluntario, caso este mesmo exista
    public boolean encomendaAcessivelV(Encomenda e, String codV) throws Exception{
        boolean res = false;
        Voluntario v = this.voluntarios.get(codV).clone();
        if (v == null) throw new Exception("Voluntario "+codV+" nao existe");
        if (distanciaLojaV(codV,e.getCodL()) <= v.getRaio() && distanciaUser(codV,e.getCodU()) <= v.getRaio()) res = true;
        return res;
    }
    
    // metodo que verifica se uma encomenda esta dentro do raio de uma empresa transportadora, caso esta mesma exista
    public boolean encomendaAcessivelE(Encomenda e, String codE) throws Exception{
        boolean res = false;
        Transportadora t = this.transportadoras.get(codE).clone();
        if (t == null) throw new Exception("Transportadora "+codE+" nao existe");
        if (distanciaLojaT(codE,e.getCodL()) <= t.getRaio() && distanciaUser(codE,e.getCodU()) <= t.getRaio()) res = true;
        return res;
    }
    
    public String assocEncomendaTransporte(Encomenda e) throws Exception{
        for (Voluntario v : getVoluntarios().values()){
            if (e instanceof EncomendasMedicas && v.aceitoTransporteMedicamentos() != true){
                throw new Exception("Nao aceita encomendas medicas");
            }
            else{
                if (this.getVoluntariosDisponiveis().contains(v) && encomendaAcessivelV(e,v.getCodV())){
                    addEncomendaVoluntario(v.getCodV(),e);
                    this.voluntariosDisponiveis.remove(v);
                    System.out.println("Codigo do voluntario associado: "+v.getCodV());
                    entregaEncomendaVoluntario(e, v);
                    return v.getCodV();
                }
            }
        }

        for (Transportadora t : getTransportadoras().values()){
            if (e instanceof EncomendasMedicas && t.aceitoTransporteMedicamentos() != true){
                throw new Exception("Nao aceita encomendas medicas");
            }
            else{
                if (!this.transportadorasIndisponiveis.contains(t.getCodE()) && encomendaAcessivelE(e,t.getCodE())){
                    if(this.encsPorAceitarU.containsKey(e.getCodEnc())){
                        this.encsPorAceitarU.get(e.getCodEnc()).add(e.getCodEnc());
                    }
                    else{
                        List<String> enc = new ArrayList<>();
                        enc.add(e.getCodEnc());
                        this.encsPorAceitarU.put(e.getCodU(), enc);
                    }
                    System.out.println("Codigo da transportadora associada: "+t.getCodE());
                    return t.getCodE();
                }
            }
        }
        System.out.println("Não foi possivel associar a encomenda");
        return null;
    }

    public String transportadoraAssociada(String codE) throws Exception{
        for (Transportadora t : getTransportadoras().values()){
            if (!this.transportadorasIndisponiveis.contains(t.getCodE()) && encomendaAcessivelE(this.encomendas.get(codE),t.getCodE())){
                return t.getCodE();
            }
        }
        return null;
    }

    public void entregaEncomendaVoluntario(Encomenda e, Voluntario v) throws Exception{
        LocalDateTime time = LocalDateTime.now();
        addEncEntregueData(e.getCodEnc(), time);
        Duration tempoEntrega = tempoEntregaEncomenda(e.getCodEnc());
        addEncEntregueDuracao(e.getCodEnc(), tempoEntrega);
        System.out.println("Custo: " + e.getCusto());
        System.out.println("Duracao: " + tempoEntrega.toString());
        System.out.println("Data/Hora de entrega: " + time.toString());
    }
    
    // metodo que determina se um utilizador aceita o custo de uma encomenda que sera entregue por uma transportadora
    public void aceitaEncomenda(String codE, String codU) throws Exception {
        Scanner input = new Scanner(System.in);
        String codT = transportadoraAssociada(codE);
        int x;
        double custo = custoEncomenda(codE, codT);
        this.encomendas.get(codE).setCusto(custo);
        System.out.println("Aceita esta encomenda?\nCusto: " + custo + "\n1-Sim\n0-Nao");
        x = input.nextInt();
        if (x == 1) {
            LocalDateTime time = LocalDateTime.now();
            addEncEntregueData(codE, time);
            Duration tempoEntrega = tempoEntregaEncomenda(codE);
            addEncEntregueDuracao(codE, tempoEntrega);
            getEncomenda(codE).setCusto(custo);
        }
        this.encsPorAceitarU.get(codU).remove(codE);
        if(this.transportadorasIndisponiveis.size() == 3){
            this.transportadorasIndisponiveis.remove(2);
        }
        Iterator<Map.Entry<String,List<Encomenda>>> it = this.encomendasTransportadora.entrySet().iterator();
        while(it.hasNext()){
            Map.Entry<String,List<Encomenda>> mentry = it.next();
            for(Encomenda e : it.next().getValue()){
                if(e.getCodEnc().equals(codE)){
                    this.transportadorasIndisponiveis.add(0, mentry.getKey());  
                }
            }
        }
        addEncomendaTransportadora(codT,getEncomendas().get(codE));
    }
    
    // metodo que adiciona uma encomenda ao map de encomendas, caso esta ainda nao exista
    public void addEncomenda(Encomenda e){
        if (!existeEncomenda(e.getCodEnc())){
            this.encomendas.put(e.getCodEnc(),e.clone());
        }
    }
    
    // metodo que adiciona uma encomenda aceite no caso de ainda nao existir
    public void addEncomendaA(EncomendasAceites ea){
        if (!existeEncomendaA(ea.getCodEnc())){
            this.encomendasAceites.add(ea);
        }
    }
    
    // metodo que determina se uma encomenda existe no map de encomendas
    public boolean existeEncomenda(String codEncomenda){
        boolean encontrado = false;
        for (String s: this.encomendas.keySet()){
            if (s.equals(codEncomenda)) encontrado = true;
        }
        return encontrado;
    }
    
    // metodo que determina se uma encomenda existe na lista de encomendas aceites
    public boolean existeEncomendaA(String codEncomenda){
        boolean encontrado = false;
        for (EncomendasAceites ea : this.encomendasAceites){
            if (ea.getCodEnc().equals(codEncomenda)) encontrado = true;
        }
        return encontrado;
    }
    
    // metodo que devolve o peso de uma encomenda
    public double pesoTransportado(String codEncomenda){
       return this.encomendas.get(codEncomenda).getPeso();
    }
    
    // metodo que adiciona uma classificacao a uma encomenda
    public void addClassificacao(Encomenda e, double classificacao){
        for (Encomenda enc : this.encomendas.values()){
            if (enc.getCodEnc().equals(e.getCodEnc())){
                e.setClassificacao(classificacao);
            }
        }
    }
    
    // metodo que devolve uma lista com os produtos transportados numa encomenda
    public List<String> produtosTransportados(String codEncomenda){
        List<String> produtos = new ArrayList<>();
        for (Encomenda e: this.encomendas.values()){
            if (e.getCodEnc().equals(codEncomenda)){
                for (LinhaEncomenda le : e.getLe()){
                    produtos.add(le.getDescr());
                }
            }
        }
        return produtos;
    }
    
    // metodo que devolve a distancia total percorrida por um voluntario ou por uma transportadora ao entrega
    public double distanciaPercorridaEnc(String codEnc) throws Exception{
        Encomenda e = this.encomendas.get(codEnc);
        Utilizador u = this.utilizadores.get(e.getCodU());
        double ux = u.getCoordX();
        double uy = u.getCoordY();
        Loja l = this.lojas.get(e.getCodL());
        double lx = l.getCoordX();
        double ly = l.getCoordY();
        String tp = transportadoraAssociada(codEnc);
        Transportadora t = this.transportadoras.get(tp);
        double tx = t.getCoordX();
        double ty = t.getCoordY();
        double totalUL = Math.abs(Math.sqrt(((ux-lx)*(ux-lx))+(uy-ly)*(uy-ly)));
        double totalTL = Math.abs(Math.sqrt(((tx-lx)*(tx-lx))+(ty-ly)*(ty-ly)));
        double total = totalUL + totalTL;
        return total;
    }
    
    // metodo que calcula o custo de uma encomenda
    public double custoEncomenda(String codEnc, String codT) throws Exception {
        double custo = 0.0;
        Encomenda e = this.encomendas.get(codEnc);
        Transportadora t = this.transportadoras.get(codT);
        custo += e.getPeso() * distanciaPercorridaEnc(codEnc) * t.getPrecoKm() * tempoEntregaEncomenda(codEnc).toMinutes();
        return custo;
    }


    
    public Duration tempoEntregaEncomenda(String codEnc) throws Exception{
        Encomenda e = this.encomendas.get(codEnc);
        Duration total = (tempoEsperaEnc(e.getCodL(),codEnc)).multipliedBy((long)(distanciaPercorridaEnc(codEnc) * 2.0));
        return total;
    }
    
    // metodo que calcula montante faturado por uma empresa transportadora num certo periodo de tempo
    public double totalFaturado(Transportadora t, LocalDateTime ti, LocalDateTime tf) throws Exception{
        double total = 0.0;
        List<Encomenda> encd = encsEntreDatas(ti,tf);
        List<Encomenda> enct = this.encomendasTransportadora.get(t.getCodE());
        for(Encomenda e : encd){
            if(enct.contains(e)){
                total+=this.encomendas.get(e.getCodEnc()).getCusto();
            }
        }
        return total;
    }
    
    // metodo que devolve quantas pessoas estao em filas de espera
    public int pessoasFilaEspera(String codL) throws Exception{
        return encomendasLoja(codL).size();
    }
    
    // metodo que adiciona uma encomenda medica a lista de encomendas medicas
    public void addEncomendaMedica(Encomenda e) throws Exception{
        boolean found = false;
        for (String s : this.encomendasMedicas){
            if (s.equals(e.getCodEnc())){
                found = true;
                throw new Exception("Encomenda "+e.getCodEnc()+" ja existe");
            }
        }
        if (found == false) this.encomendasMedicas.add(e.getCodEnc());
    }
    
    // metodo que determina se uma encomenda e uma encomenda medica
    public boolean isEncomendaMedica(String codEnc){
        boolean found = false;
        for (String cod : this.encomendasMedicas){
            if (cod.equals(codEnc)){
                found = true;
                break;
            }
        }
        return false;
    }
    
    // metodo que determina o tempo de espera para o levantamento de uma encomenda em loja
    public Duration tempoEsperaEnc(String codL, String codEnc) throws Exception{
        Map<String,Encomenda> encs = encomendasLoja(codL);
        Duration tempo = Duration.ZERO;
        tempo = tempo.plusMinutes(5);
        boolean found = false;
        Iterator<Encomenda> it = encs.values().iterator();
        while (it.hasNext() && !found) {
            if (it.next().getCodEnc().equals(codEnc)) found = true;
            else tempo = tempo.plusMinutes(5);
        }
        return tempo;
    }
    
    // metodo que devolve um map das transportadoras
    public Map<String,Transportadora> getTransportadoras(){
        Map<String,Transportadora> transportadoras = new HashMap<>();
        for (Transportadora t : this.transportadoras.values()){
            transportadoras.put(t.getCodE(),t.clone());
        }
        return transportadoras;
    }
    
    // metodo que devolve um map dos voluntarios
    public Map<String,Voluntario> getVoluntarios(){
        Map<String,Voluntario> voluntarios = new HashMap<>();
        for (Voluntario v : this.voluntarios.values()){
            voluntarios.put(v.getCodV(),v.clone());
        }
        return voluntarios;
    }
    
    // metodo que devolve um map das lojas
    public Map<String,Loja> getLojas(){
        Map<String,Loja> lojas = new HashMap<>();
        for (Loja l : this.lojas.values()){
            lojas.put(l.getCodL(),l.clone());
        }
        return lojas;
    }
    
    // metodo que devolve um map dos utilizadores
    public Map<String,Utilizador> getUtilizadores(){
        Map<String,Utilizador> utilizadores = new HashMap<>();
        for (Utilizador u : this.utilizadores.values()){
            utilizadores.put(u.getCodU(),u.clone());
        }
        return utilizadores;
    }
    
    // metodo que devolve um map das encomendas
    public Map<String,Encomenda> getEncomendas(){
        return this.encomendas;
    }

    public Encomenda adminClass(String cod){
        return this.encomendas.get(cod);
    }
    
    // metodo que devolve uma lista das encomendas aceites
    public List<EncomendasAceites> getEncomendasAceites(){
        List<EncomendasAceites> ea = new ArrayList<>();
        for (EncomendasAceites e : this.encomendasAceites){
            ea.add(e.clone());
        }
        return ea;
    }
    
    // metodo que devolve as encomendas prontas a ser levantadas em loja
    public Map<String,Encomenda> getEncomendasProntas(){
       Map<String,Encomenda> encsProntas = new HashMap<>();
        for (Map.Entry<String,Encomenda> ent : this.encomendasProntas.entrySet()){
            encsProntas.put(ent.getKey(),ent.getValue());
        }
        return encsProntas;
    }
    
    public Map<String,LocalDateTime> getEncomendasData(){
        Map<String,LocalDateTime> encsData = new HashMap<>();
        for (Map.Entry<String,LocalDateTime> entry : this.encEntregueData.entrySet()){
            encsData.put(entry.getKey(),entry.getValue());
        }
        return encsData;
    }

    public Map<String,Duration> getEncomendasDuracao(){
        Map<String,Duration> encsDuracao = new HashMap<>();
        for (Map.Entry<String,Duration> entry : this.encEntregueDuracao.entrySet()){
            encsDuracao.put(entry.getKey(),entry.getValue());
        }
        return encsDuracao;
    }

    public Map<String,List<String>> getEncsPorAceitarU(){
        Map<String,List<String>> encsPorAceitar = new HashMap<>();
        Iterator<Map.Entry<String,List<String>>> it = this.encsPorAceitarU.entrySet().iterator();
        while(it.hasNext()){
            Map.Entry<String,List<String>> x = it.next();
            encsPorAceitar.put(x.getKey(),x.getValue());
        }
        return encsPorAceitar;        
    }
    
    // metodo para definir um map de transportadoras
    public void setTransportadoras(Map<String,Transportadora> trans){
        this.transportadoras = new HashMap<>();
        for (Transportadora t : trans.values()){
            this.transportadoras.put(t.getCodE(),t.clone());
        }
    }
    
    // metodo para definir um map de voluntarios
    public void setVoluntarios(Map<String,Voluntario> voluntarios){
        this.voluntarios = new HashMap<>();
        for (Voluntario v : voluntarios.values()){
            this.voluntarios.put(v.getCodV(),v.clone());
        }
    }
    
    // metodo para definir um map de lojas
    public void setLojas(Map<String,Loja> lojas){
        this.lojas = new HashMap<>();
        for (Loja l : lojas.values()){
            lojas.put(l.getCodL(),l.clone());
        }
    }
    
    // metodo para definir um map de utilizadores
    public void setUtilizadores(Map<String,Utilizador> users){
        this.utilizadores = new HashMap<>();
        for (Utilizador u : users.values()){
            this.utilizadores.put(u.getCodU(),u.clone());
        }
    }
    
    // metodo para definir um map de encomendas
    public void setEncomendas(Map<String,Encomenda> encs){
        this.encomendas = new HashMap<>();
        for (Encomenda e: encs.values()){
            this.encomendas.put(e.getCodEnc(),e.clone());
        }
    }
    
    // metodo para definir uma lista de encomendas aceites
    public void setEncomendasAceites(List<EncomendasAceites> lea){
        this.encomendasAceites = new ArrayList<>();
        for (EncomendasAceites ea : lea){
            this.encomendasAceites.add(ea.clone());
        }
    }

    public void setEncomendasData(Map<String,LocalDateTime> encsData){
        this.encEntregueData = new HashMap<>();
        for (Map.Entry<String,LocalDateTime> entry : encsData.entrySet()){
            this.encEntregueData.put(entry.getKey(),entry.getValue());
        }
    }

    public void setEncomendasDuracao(Map<String,Duration> encsDuracao){
        this.encEntregueDuracao = new HashMap<>();
        for (Map.Entry<String,Duration> entry : encEntregueDuracao.entrySet()){
            this.encEntregueDuracao.put(entry.getKey(),entry.getValue());
        }
    }
    
    // metodo para devolver um utilizador a partir do seu codigo
    public Utilizador getUtilizador(String codU){
        return this.utilizadores.get(codU);
    }
    
    // metodo para devolver uma loja a partir do seu codigo
    public Loja getLoja(String codL){
        return this.lojas.get(codL);
    }
    
    // metodo para devolver uma transportadora a partir do seu codigo
    public Transportadora getTransportadora(String codE){
        return this.transportadoras.get(codE);
    }
    
    // metodo para dev, olver um voluntario a partir do seu codigo
    public Voluntario getVoluntario(String codV){
        return this.voluntarios.get(codV);
    }
    
    // metodo para devolver uma encomenda a partir do seu codigo
    public Encomenda getEncomenda(String codEnc){
        return this.encomendas.get(codEnc);
    }
    
    // metodo que devolve uma lista das encomendas transportadas por uma empresa transportadora
    public List<Encomenda> getEncomendasTransportadora(String codE){
        List<Encomenda> nova = new ArrayList<>();
        for (String s : this.encomendasTransportadora.keySet()){
            if (s.equals(codE)){
                for (Encomenda e : this.encomendasTransportadora.get(codE)){
                    nova.add(e.clone());
                }
            }
        }
        return nova;
    }
    
    // metodo que devolve uma lista das encomendas transportadas por um voluntario
    public List<Encomenda> getEncomendasVoluntario(String codV){
        List<Encomenda> nova = new ArrayList<>();
        for (String s : this.encomendasVoluntario.keySet()){
            if (s.equals(codV)){
                nova.add(this.encomendasVoluntario.get(codV).clone());
            }
        }
        return nova;
    }
    
    // metodo que devolve as encomendas prontas de uma loja
    public List<Encomenda> getEncomendasProntas(String codL){
        List<Encomenda> encs = new ArrayList<>();
        for (String s : this.encomendasProntas.keySet()){
            if (s.equals(codL)){
                encs.add(this.encomendasProntas.get(s).clone());
            }
        }
        return encs;
    }
    
    public List<Voluntario> getVoluntariosDisponiveis(){
        List<Voluntario> volsDisp = new ArrayList<>();
        for (Voluntario v : this.voluntariosDisponiveis){
            volsDisp.add(v.clone());
        }
        return volsDisp;
    }
    
    public List<Transportadora> getTransportadorasDisponiveis(){
        List<Transportadora> empsDisp = new ArrayList<>();
        for (Transportadora t : this.transportadorasDisponiveis){
            empsDisp.add(t.clone());
        }
        return empsDisp;
    }

    public List<String> getTransportadorasIndisponiveis(){
        List<String> empsIndisp = new ArrayList<>();
        for (String s : this.transportadorasIndisponiveis){
            empsIndisp.add(s);
        }
        return empsIndisp;
    }
    
    public void setEncsEntreguesData(Map<String,LocalDateTime> encsData){
        this.encEntregueData = new HashMap<>();
        for (Map.Entry<String,LocalDateTime> entry : encsData.entrySet()){
            this.encEntregueData.put(entry.getKey(),entry.getValue());
        }
    }
    
    public void addVoluntarioDisponivel(Voluntario v){
        if (!this.voluntariosDisponiveis.contains(v))this.voluntariosDisponiveis.add(v.clone());
    }
    
    public void addTransportadoraDisponivel(Transportadora t){
        if (!this.transportadorasDisponiveis.contains(t))this.transportadorasDisponiveis.add(t.clone());
    }

    public void removeTransportadoraDisponivel(Transportadora t){
        this.transportadorasDisponiveis.remove(t);
    }

    public void addTransportadoraIndisponivel(String s){
        this.transportadorasIndisponiveis.add(s);
    }

    public void removeTransportadoraIndisponivel(String s){
        this.transportadorasIndisponiveis.remove(s);
    }
    
    // metodo para adicionar um utilizador ao map de utilizadores
    public void addUtilizador(Utilizador u){
        this.utilizadores.put(u.getCodU(),u.clone());
    }
    
    // metodo para adicionar uma loja ao map de lojas
    public void addLoja(Loja l){
        this.lojas.put(l.getCodL(),l.clone());
    }
    
    // metodo para adicionar uma empresa transportadora ao map de transportadoras
    public void addTransportadora(Transportadora t){
        this.transportadoras.put(t.getCodE(),t.clone());
    }
    
    // metodo para adicionar um voluntario ao map de voluntarios
    public void addVoluntario(Voluntario v){
        this.voluntarios.put(v.getCodV(),v.clone());
    }
    
    // metodo para adicionar uma encomenda aceite a lista de encomendas aceites
    public void addEncomendaAceite(EncomendasAceites ea){
        if (!this.encomendasAceites.contains(ea))this.encomendasAceites.add(ea.clone());
    }
    
    public void addEncomendaVoluntario(String codV, Encomenda e){
        this.encomendasVoluntario.put(codV,e.clone());
    }
    
    public void addEncomendaTransportadora(String codE, Encomenda e){
        if(this.encomendasTransportadora.get(codE) != null){
            getEncomendasTransportadora(codE).add(e.clone());
        }
        else {
            List<Encomenda> enc = new ArrayList<Encomenda>();
            enc.add(e.clone());
            this.encomendasTransportadora.put(codE, enc);
        }
    }
    
    // metodo que altera a classificacao de uma encomenda
    public void setClass (String codE, double x){
        this.encomendas.get(codE).setClassificacao(x);
    }

    // metodo que adiciona ao map uma entrada que associa a um codigo de encomenda a data
    public void addEncEntregueData(String codEnc, LocalDateTime data){
        this.encEntregueData.put(codEnc,data);
    }
    
    // metodo que adiciona ao map uma entrada que associa a um codigo de encomenda a duracao
    public void addEncEntregueDuracao(String codEnc, Duration duracao){
        this.encEntregueDuracao.put(codEnc,duracao);
    }
    
    // metodo que devolve uma lista de encomendas entre duas datas passadas como argumento
    public List<Encomenda> encsEntreDatas(LocalDateTime ti, LocalDateTime tf){
        List<Encomenda> encomendasD = new ArrayList<>();
        for (String s : this.encEntregueData.keySet()){
            if (this.encEntregueData.get(s).isAfter(ti) && this.encEntregueData.get(s).isBefore(tf)){
                encomendasD.add(this.encomendas.get(s).clone());
            }
        }
        return encomendasD;
    }
}