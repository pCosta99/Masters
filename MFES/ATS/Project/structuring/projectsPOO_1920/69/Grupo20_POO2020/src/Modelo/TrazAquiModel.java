package Modelo;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que contém a implementação da estrutura TrazAquiModel.
 */
public class TrazAquiModel implements Serializable{
    private Map<String,Utilizador> utilizador;
    private Map<String,Voluntario> voluntario;
    private Map<String,Encomenda> encomenda;
    private Map<String,Transportadora> transportador;
    private Map<String,Loja> loja;
    private Map<String,Encomenda> aceites;

    /**
     * Construtor por omissão
     */
    public TrazAquiModel(){
        utilizador = new HashMap<>();
        voluntario = new HashMap<>();
        encomenda = new HashMap<>();
        transportador = new HashMap<>();
        loja = new HashMap<>();
        aceites = new HashMap<>();
    }

    /**
     * Construtor parametrizado
     * @param utilizador                    Map de Utilizadores
     * @param voluntario                    Map de Voluntários
     * @param encomenda                     Map de Encomendas
     * @param transportador                 Map de Transportadoras
     * @param loja                          Map de Lojas
     * @param aceites                       Map de Aceites
     */
    public TrazAquiModel(Map<String, Utilizador> utilizador, Map<String, Voluntario> voluntario,
                         Map<String, Encomenda> encomenda, Map<String, Transportadora> transportador,
                         Map<String, Loja> loja, Map<String,Encomenda>  aceites) {
        setUtilizador(utilizador);
        setVoluntario(voluntario);
        setEncomenda(encomenda);
        setTransportador(transportador);
        setLoja(loja);
        setAceites(aceites);
    }

    /**
     * Construtor por cópia
     * @param p             TraqAquiModel
     */
    public TrazAquiModel(TrazAquiModel p){
        setUtilizador(p.getUtilizador());
        setVoluntario(p.getVoluntario());
        setEncomenda(p.getEncomenda());
        setTransportador(p.getTransportador());
        setLoja(p.getLoja());
        setAceites(p.getAceites());
    }

    /**
     * Devolve a map de encomendas aceites
     * @return Map<String, Encomenda>
     */
    public Map<String, Encomenda> getAceites() {
        Map<String,Encomenda> aux = new HashMap<>();
        for(Encomenda a: this.aceites.values()){
            aux.put(a.getCodEncomenda(),a);
        }
        return aceites;
    }

    /**
     * Define a map de encomendas aceites
     * @param aceites               Map de aceites
     */
    public void setAceites(Map<String,Encomenda> aceites) {
        this.aceites = new HashMap<>();
        for(Encomenda a: aceites.values()){
            this.aceites.put(a.getCodEncomenda(),a);
        }
    }

    /**
     * Devolve a map de utilizadores
     * @return Map<String, Utilizador>
     */
    public Map<String, Utilizador> getUtilizador() {
        return utilizador;
    }

    /**
     * Define a map de utilizadores
     * @param utilizador            Map de Utilizadores
     */
    public void setUtilizador(Map<String, Utilizador> utilizador) {
        this.utilizador = utilizador;
    }

    /**
     * Devolve a map de voluntários
     * @return Map<String, Voluntário>
     */
    public Map<String, Voluntario> getVoluntario() {
        return voluntario;
    }

    /**
     * Define a map de voluntários
     * @param voluntario            Map de Voluntários
     */
    public void setVoluntario(Map<String, Voluntario> voluntario) {
        this.voluntario = voluntario;
    }

    /**
     * Devolve a map de Encomendas
     * @return Map<String, Encomenda>
     */
    public Map<String, Encomenda> getEncomenda() {
        return encomenda;
    }

    /**
     * Define a map de Encomendas
     * @param encomenda             Map de Encomendas
     */
    public void setEncomenda(Map<String, Encomenda> encomenda) {
        this.encomenda = encomenda;
    }

    /**
     * Devolve a map de Transportadoras
     * @return Map<String, Trasnportadora>
     */
    public Map<String, Transportadora> getTransportador() {
        return transportador;
    }

    /**
     * Define a map de Transportadoras
     * @param transportador             Map de Transportadoras
     */
    public void setTransportador(Map<String, Transportadora> transportador) {
        this.transportador = transportador;
    }

    /**
     * Devolve a map de Lojas
     * @return Map<String, Loja>
     */
    public Map<String, Loja> getLoja() {
        return loja;
    }

    /**
     * Define a map de Lojas
     * @param loja              Map de Lojas
     */
    public void setLoja(Map<String, Loja> loja) {
        this.loja = loja;
    }

    /**
     * Verifica a igualdade com outro objeto
     * @param o          Objeto a comparar
     * @return boolean
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TrazAquiModel)) return false;
        TrazAquiModel parse = (TrazAquiModel) o;
        return Objects.equals(getUtilizador(), parse.getUtilizador()) &&
                Objects.equals(getVoluntario(), parse.getVoluntario()) &&
                Objects.equals(getEncomenda(), parse.getEncomenda()) &&
                Objects.equals(getTransportador(), parse.getTransportador()) &&
                Objects.equals(getLoja(), parse.getLoja());
    }

    /**
     * Método hashCode do objeto
     * @return hash do objeto
     */
    @Override
    public int hashCode() {
        return Objects.hash(getUtilizador(), getVoluntario(), getEncomenda(), getTransportador(), getLoja());
    }

    /**
     * Método toString do objeto
     * @return Objeto em modo string
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Modelo.Parse{");
        sb.append("utilizador=").append(utilizador);
        sb.append(", voluntario=").append(voluntario);
        sb.append(", encomenda=").append(encomenda);
        sb.append(", transportador=").append(transportador);
        sb.append(", loja=").append(loja);
        sb.append('}');
        return sb.toString();
    }

    /**
     * Devolve uma cópia da instância
     * @return TrazAquiModel
     */
    public TrazAquiModel clone(){
        return new TrazAquiModel(this);
    }

    /**
     * Função que lê o ficheirod log inicial e completa as estruturas.
     */
    public void parseDefault(){
        List<String> linhas = lerFicheiro("Logs.txt"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "Utilizador":
                    parseUtilizador(linhaPartida[1]); // criar um Modelo.Utilizador
                    break;
                case "Loja":
                    parseLoja(linhaPartida[1]);
                    break;
                case "Transportadora":
                    parseTransportadora(linhaPartida[1]);
                    break;
                case "Voluntario":
                    parseVoluntario(linhaPartida[1]);
                    break;
                case "Encomenda":
                    parseEncomenda(linhaPartida[1]);
                    break;
                case "Aceite":
                    parseAceite(linhaPartida[1]);
                    break;
            }
        }
    }


    /**
     * Parse do utlizador
     * @param input       Informação do utilizador
     */
    public void parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas c = new Coordenadas(gpsx,gpsy);
        List<Encomenda> pendentes = new ArrayList<>();
        List<String> transporte = new ArrayList<>();
        Utilizador uti = new Utilizador(codUtilizador,nome,c,0,pendentes,transporte);
        insereUtilizadorMap(uti);
    }

    /**
     * Parse da loja
     * @param input       Informação da loja
     */
    public void parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        Random ran = new Random();
        double tempoAtend = Math.round(ran.nextDouble()*10);
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas c = new Coordenadas(gpsx,gpsy);
        List<Encomenda> enc = new ArrayList<>();
        boolean temTempo = ran.nextBoolean();
        double tempoFila;
        if(temTempo)
            tempoFila = Math.round(ran.nextDouble()*10);
        else
            tempoFila = -1;
        Map<String,LinhaEncomenda> catalogo = new HashMap<>();
        List<Encomenda> e = new ArrayList<>();
        Loja l = new Loja(codLoja,nomeLoja,tempoAtend,c,enc,tempoFila,temTempo,e,catalogo);
        insereLojaMap(l);
    }

    /**
     * Parse da transportadora
     * @param input       Informação para a transportadora
     */
    public void parseTransportadora(String input){
        String[] campos = input.split(",");
        String codTran = campos[0];
        String nomeEmpresa = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Random ran = new Random();
        int nif = Integer.parseInt(campos[4]);
        boolean meds = ran.nextBoolean();
        boolean multi = ran.nextBoolean();
        Coordenadas c = new Coordenadas(gpsx,gpsy);
        double raio = Double.parseDouble(campos[5]);
        double preco_km = Double.parseDouble(campos[6]);
        double preco_peso = Math.round(ran.nextDouble()*10);
        double velocidade = Math.round(ran.nextDouble()*100);
        Map<Encomenda, LocalDateTime> enc = new HashMap<>();
        List<Integer> clas = new ArrayList<>();
        if(multi) {
            int numero = 0;
            List<Encomenda> pen = new ArrayList<>();
            while(numero == 0 || numero == 1)
                numero = ran.nextInt(10);
            TransportadoraMulti t = new TransportadoraMulti(codTran, nomeEmpresa, nif, multi, meds, c, raio, preco_km, preco_peso, velocidade,0,clas, enc, numero,pen);
            insereTransportadoraMap(t);
        }
        else {
            TransportadoraNormal t = new TransportadoraNormal(codTran, nomeEmpresa, nif,multi,meds, c, raio, preco_km, preco_peso, velocidade,0,clas, enc);
            insereTransportadoraMap(t);
        }
    }

    /**
     * Parse do voluntário
     * @param input       Informação para o voluntário
     */
    public void parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas c = new Coordenadas(gpsx,gpsy);
        double raio = Double.parseDouble(campos[4]);
        Random ran = new Random();
        boolean livre = ran.nextBoolean();
        Map<Encomenda,LocalDateTime> enc = new HashMap<>();
        List<Integer> clas= new ArrayList<>();
        boolean meds = ran.nextBoolean();
        Voluntario vol = new Voluntario(codVoluntario,nome,c,raio,livre,meds,clas,enc);
        insereVoluntarioMap(vol);
    }

    /**
     * Parse da Encomenda
     * @param input       Informação da Encomenda
     */
    public void parseEncomenda(String input){
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        boolean entregue = false;
        int i = 4;
        int len = campos.length-4;
        List<LinhaEncomenda> en = new ArrayList<>();
        while(i < len){
            String codP = campos[i++];
            String descricao = campos[i++];
            double pesoU = Double.parseDouble(campos[i++]);
            double precoU = Double.parseDouble(campos[i++]);
            LinhaEncomenda nova = new LinhaEncomenda(codP,descricao,pesoU,precoU);
            en.add(nova);
        }
        if(loja.containsKey(codLoja)){
            Loja l = loja.get(codLoja);
            for(LinhaEncomenda aux: en){
                l.adicionaMapCatalogo(aux);
            }
        }
        double tempo = 0;
        Encomenda e = new Encomenda(codEncomenda,codUtilizador,codLoja,peso,entregue,tempo,false,en);
        insereEncomendaMap(e);
    }

    /**
     * Parse das encomendas aceites
     * @param input       Informação das aceites
     */
    public void parseAceite(String input) {
        String[] campos = input.split(",");
        String codEncomenda =campos[0];
        Encomenda enc = encomenda.get(codEncomenda);
        Utilizador u = this.utilizador.get(enc.getCodUtilizador());
        u.adicionaPendentes(enc);
        insereAceite(enc.getCodEncomenda());
    }

    /**
     * Função que lê o ficheiro
     * @param nomeFich           Nome do ficheiro
     * @return List<String>      Leitura do  ficheiro
     */
    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }


    // --------------------- I N S E R C O E S ------------------------

    /**
     * Função que insere  uma encomenda na Map
     * @param e           Encomenda a adicionar
     */
    public void insereEncomendaMap(Encomenda e){
        String cod = e.getCodEncomenda();
        if(!(this.encomenda.containsKey(cod))){
            this.encomenda.put(cod,e);
        }
    }

    /**
     * Função que insere  um voluntário na Map
     * @param v           Voluntário a adicionar
     */
    public void insereVoluntarioMap(Voluntario v){
        String cod = v.getCodVolu();
        if(!(this.voluntario.containsKey(cod))){
            this.voluntario.put(cod,v);
        }
    }

    /**
     * Função que insere  um utilizador na Map
     * @param u           Utilizador a adicionar
     */
    public void insereUtilizadorMap(Utilizador u){
        String cod = u.getCodUti() ;
        if(!(this.utilizador.containsKey(cod))){
            this.utilizador.put(cod,u);
        }
    }

    /**
     * Função que insere  uma transportadora na Map
     * @param t           Transportadora a adicionar
     */
    public void insereTransportadoraMap(Transportadora t){
        String cod = t.getCodEmpr();
        if(!this.transportador.containsKey(cod)){
            this.transportador.put(cod,t);
        }
    }

    /**
     * Função que insere  uma loja na Map
     * @param l           Loja a adicionar
     */
    public void insereLojaMap(Loja l){
        String cod = l.getCodLoja();
        if(!this.loja.containsKey(cod)){
            this.loja.put(cod,l);
        }
    }

    /**
     * Função que insere  uma encomenda aceita na Map
     * @param codEnc           Código da encomenda aceite a adicionar
     */
    public void insereAceite(String codEnc){
        if(!(this.aceites.containsKey(codEnc))) {
            this.aceites.put(codEnc,this.encomenda.get(codEnc));
        }
    }

    /**
     * Função que devolve dos voluntários disponiveis, o voluntário mais perto do utilizador
     * @param u               Utilizador que quer a  encomenda
     * @param e               Encomenda a transportar
     * @return Voluntário     Voluntário escolhido
     */
    public Voluntario voluntariosDisponiveis(Utilizador u,Encomenda e){
        if(e.isMedica())
            return this.voluntario.values().stream().filter(a -> a.isLivre() && a.dentroRaio(u.getGps()) && a.aceitoTransporteMedicamentos())
                                           .min((a, b) -> a.compareDistance(u, b)).orElse(null);
        else
            return this.voluntario.values().stream().filter(a -> a.isLivre() && a.dentroRaio(u.getGps()))
                    .min((a, b) -> a.compareDistance(u, b)).orElse(null);

    }

    /**
     * Função que remove uma encomenda aceite
     * @param codEnc         Codigo da encomenda aceite
     */
    public void removeAceite(String codEnc){
        if(this.aceites.containsKey(codEnc)){
            Encomenda e = this.encomenda.get(codEnc);
            this.aceites.remove(codEnc);
        }
    }

    /**
     * Função que devolve o código da transportadora  (key),
     *                    o custo de transporte da transportadora (value.key),
     *                    o tempo de transporte da transportadora (value.value)
     * @param u           Utilizador para fazer o transporte
     * @param e           Encomenda a transportar
     * @param l           Loja onde estáa  encomenda
     * @return  Informação necessária para obter tempos e custos.
     */
    public Map<String,Map.Entry<Double,Double>> transportadorasCustos(Utilizador u,Encomenda e,Loja l){
        Map<String,Map.Entry<Double,Double>> trans = new HashMap<>();
        if(e.isMedica()){
            List<Transportadora> tran =this.transportador.values().stream().filter(a->a.aceitoTransporteMedicamentos() && a.dentroRaio(u.getGps())).collect(Collectors.toList());
            for(Transportadora  t: tran){
                double custo = t.custoEncomenda(e,u,l.getGps());
                double tempo = t.tempodeEncomenda(e,u,l);
                trans.put(t.getCodEmpr(),new AbstractMap.SimpleEntry<>(custo,tempo));
            }
        }
        else {
            List<Transportadora> tran =this.transportador.values().stream().filter(a->a.dentroRaio(u.getGps())).collect(Collectors.toList());
            for(Transportadora  t: tran) {
                double custo = t.custoEncomenda(e, u, l.getGps());
                double tempo = t.tempodeEncomenda(e, u, l);
                trans.put(t.getCodEmpr(), new AbstractMap.SimpleEntry<>(custo, tempo));
            }
        }
        return trans;
    }

    /**
     * Função que carrega informação já guardada
     * @param nomeFicheiro           Nome do ficheiro
     * @return  Devolve a  classe TrazAquiModel inicializada
     * @throws IOException
     * @throws ClassNotFoundException
     */
    public TrazAquiModel carregaEstado(String nomeFicheiro) throws IOException,ClassNotFoundException{
        FileInputStream f = new FileInputStream(nomeFicheiro);
        ObjectInputStream o = new ObjectInputStream(f);
        TrazAquiModel g = (TrazAquiModel) o.readObject();
        o.close();
        return g;
    }

    /**
     * Função que guarda informação num  ficheiro
     * @param nomeFicheiro          Nome do  ficheiro
     * @throws IOException
     */
    public void guardaEstado(String nomeFicheiro) throws IOException{
        FileOutputStream f = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream os = new ObjectOutputStream(f);
        os.writeObject(this);
        os.flush();
        os.close();
    }


    /**
     * Função que devolve os 10 utilizadores que mais  utilizam o sistema
     * @return List<Utilizador>
     */
    public List<Utilizador> dezUtilizador(){
        return this.utilizador.entrySet().stream().sorted((a,b) -> a.getValue().compareNum(b.getValue()))
                                         .map(Map.Entry::getValue)
                                         .limit(10)
                                         .collect(Collectors.toList());
    }

    /**
     * Função que devolve os 10 transportadoras que mais kms fizeram
     * @return List<Transportadora>
     */
    public List<Transportadora> dezTransportadoras(){
        return this.transportador.entrySet().stream().sorted((a,b) -> a.getValue().compareKm(b.getValue()))
                                            .map(Map.Entry::getValue)
                                            .limit(10)
                                            .collect(Collectors.toList());
    }



}
