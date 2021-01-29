package Model;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe responsável pela gestão dos dados do ficheiro de logs
 */
public class Leitura {
    private List<Utilizador> utilizadores;
    private List<Voluntario> voluntarios;
    private List<Transportadora> transportadoras;
    private List<Loja> lojas;
    private Map<String, Set<Encomenda>> encomendas;
    private List<String> encomendasAceites;

    /**
     * Construtor da classe
     */
    public Leitura() {
        this.utilizadores = new ArrayList<>();
        this.voluntarios = new ArrayList<>();
        this.transportadoras = new ArrayList<>();
        this.lojas = new ArrayList<>();
        this.encomendas = new HashMap<>();
        this.encomendasAceites = new ArrayList<>();
    }

    /**
     * Indica os utilizadores do ficheiro de logs
     * @return Os utilizadores do ficheiro de logs
     */
    public List<Utilizador> getUtilizadores(){
        return utilizadores.stream().map(Utilizador::clone).collect(Collectors.toList());
    }

    /**
     * Indica os voluntários do ficheiro de logs
     * @return Os voluntários do ficheiro de logs
     */
    public List<Voluntario> getVoluntarios(){
        return voluntarios.stream().map(Voluntario::clone).collect(Collectors.toList());
    }

    /**
     * Indica as transportadoras do ficheiro de logs
     * @return As transportadoras do ficheiro de logs
     */
    public List<Transportadora> getTransportadoras(){
        return transportadoras.stream().map(Transportadora::clone).collect(Collectors.toList());
    }

    /**
     * Indica as lojas do ficheiro de logs
     * @return As lojas do ficheiro de logs
     */
    public List<Loja> getLojas(){
        return lojas.stream().map(Loja::clone).collect(Collectors.toList());
    }

    /**
     * Indica as encomendas aceites do ficheiro de logs
     * @return As encomendas aceites do ficheiro de logs
     */
    public List<String> getEncomendasAceites(){
        return new ArrayList<>(encomendasAceites);
    }

    /**
     * Indicas as encomendas do ficheiro de logs
     * @return As encomendas do ficheiro de logs
     */
    public Map<String, Set<Encomenda>> getEncomendas(){
        return encomendas.entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, p->p.getValue().stream()
                        .map(Encomenda::clone)
                        .collect(Collectors.toSet())));

    }

    /**
     * Lê o ficheiro de logs
     * @param file O nome do ficheiro que se pretende ler
     * @throws IOException ???
     */
    public void lerFicheiro(String file) throws IOException {
        BufferedReader buffr = new BufferedReader(new FileReader(file));

        String linha = buffr.readLine();
        while (linha != null && !linha.equals("Dados de LOGS:")) {
            linha = buffr.readLine();
        }

        linha = buffr.readLine();
        while (linha != null) {
            processaDados(linha);
            linha = buffr.readLine();
        }
        buffr.close();
    }

    /**
     * Processa os dados do ficheiro de logs
     * @param dados Os dados do ficheiro de logs
     */
    private void processaDados(String dados){
        int fim;
        fim = dados.indexOf(':');

        //significa que não tem ':' logo não é uma linha de dados
        if(fim < 0) return;

        String tipo = dados.substring(0, fim);
        switch (tipo) {
            case "Utilizador": {
                processaUtilizador(dados);
                break;
            }
            case "Voluntario": {
                processaVoluntario(dados);
                break;
            }
            case "Transportadora": {
                processaTransportadora(dados);
                break;
            }
            case "Loja": {
                processaLoja(dados);
                break;
            }
            case "Encomenda": {
                processaEncomenda(dados);
                break;
            }
            case "Aceite": {
                processaEncomendaAceite(dados);
                break;
            }
            default:
                //linhas que não são de dados mas têm ':' passam para este ponto
                break;
        }

    }

    /**
     * Processa a informação de um utilizador em formato de String
     * @param utilizadorString A informação do utilizador em formato de String
     */
    private void processaUtilizador(String utilizadorString){
        Utilizador utilizador = new Utilizador();
        String[] partes;
        String util;
        int size;

        //Separacao da String nas componentes do utilizador
        util = utilizadorString.substring(utilizadorString.indexOf(':')+1);
        partes = util.split(",");

        //Inicializacao do Model.GPS
        double lon,lat;
        GPS gps;
        lon = Double.parseDouble(partes[2]);
        lat = Double.parseDouble(partes[3]);
        gps = new GPS(lon,lat);

        //Criacao do Model.Utilizador
        utilizador.setCode(partes[0]);
        utilizador.setNome(partes[1]);
        utilizador.setGps(gps);

        //adicionar à lista de utilizadores
        utilizadores.add(utilizador);
    }

    /**
     * Processa a informação de um voluntário em formato de String
     * @param voluntarioString A informação de um voluntário em formato de String
     */
    private void processaVoluntario(String voluntarioString){
        Voluntario voluntario = new Voluntario();
        String[] partes;
        String util;
        int size;

        //Separacao da String nas componentes do voluntario
        util = voluntarioString.substring(voluntarioString.indexOf(':')+1);
        partes = util.split(",");

        //Inicializacao do Model.GPS
        double lon,lat;
        GPS gps;
        lon = Double.parseDouble(partes[2]);
        lat = Double.parseDouble(partes[3]);
        gps = new GPS(lon,lat);

        //conversao do raio
        double raio = Double.parseDouble(partes[4]);

        //Criacao do voluntario
        voluntario.setCode(partes[0]);
        voluntario.setNome(partes[1]);
        voluntario.setGps(gps);
        voluntario.setRaio(raio);

        //adicionar à lista de voluntarioes
        voluntarios.add(voluntario);
    }

    /**
     * Processa a informação de uma transportadora em formato de String
     * @param transportadoraString A informação de uma transportadora em formato de String
     */
    private void processaTransportadora(String transportadoraString){
        Transportadora transportadora = new Transportadora();
        String[] partes;
        String util;
        int size;

        //Separacao da String nas componentes do transportadora
        util = transportadoraString.substring(transportadoraString.indexOf(':')+1);
        partes = util.split(",");

        //Inicializacao do Model.GPS
        double lon,lat;
        GPS gps;
        lon = Double.parseDouble(partes[2]);
        lat = Double.parseDouble(partes[3]);
        gps = new GPS(lon,lat);

        //nif
        String nif = partes[4];

        //conversao do raio
        double raio = Double.parseDouble(partes[5]);

        //conversao do preco
        double precKM = Double.parseDouble(partes[6]);

        //Criacao do transportadora
        transportadora.setCode(partes[0]);
        transportadora.setNome(partes[1]);
        transportadora.setNif(nif);
        transportadora.setGps(gps);
        transportadora.setRaio(raio);
        transportadora.setPrec_km(precKM);

        //adicionar à lista de transportadoras
        transportadoras.add(transportadora);
    }

    /**
     * Processa a informação de uma loja em formato de String
     * @param lojaString A informação de uma loja em formato de String
     */
    private void processaLoja(String lojaString){
        Loja loja = new Loja();
        String[] partes;
        String util;
        int size;

        //Separacao da String nas componentes do loja
        util = lojaString.substring(lojaString.indexOf(':')+1);
        partes = util.split(",");

        //Inicializacao do Model.GPS
        double lon,lat;
        GPS gps;
        lon = Double.parseDouble(partes[2]);
        lat = Double.parseDouble(partes[3]);
        gps = new GPS(lon,lat);

        //Criacao do loja
        loja.setCode(partes[0]);
        loja.setNome(partes[1]);
        loja.setGps(gps);

        //adicionar à lista de lojas
        lojas.add(loja);
    }

    //Processamento de Encomendas a partir daqui

    /**
     * Processa a informação das um linhas de encomenda em formato de String
     * @param linhaEnc A informação das um linhas de encomenda em formato de String
     * @return
     */
    private List<LinhaEncomenda> processaLinhaEnc(String[] linhaEnc){
        ArrayList<LinhaEncomenda> lencList = new ArrayList<>();
        for (int i = 0; i < linhaEnc.length ; i+=4) {
            //Inicialização da linha de encomenda e preparação dos parametros
            LinhaEncomenda lenc = new LinhaEncomenda();
            double preco = Double.parseDouble(linhaEnc[i+3]);
            double qnt = Double.parseDouble(linhaEnc[i+2]);

            //Adição dos elementos
            lenc.setReferencia(linhaEnc[i]);
            lenc.setDescricao(linhaEnc[i+1]);
            lenc.setPreco(preco);
            lenc.setQuantidade(qnt);

            //Insercao no arraylist
            lencList.add(lenc);
        }

        return lencList;
    }

    /**
     * Processa a informação de uma encomenda em formato de String
     * @param encomenda A informação de uma encomenda em formato de String
     */
    private void processaEncomenda(String encomenda){
        Encomenda enc = new Encomenda();
        String[] partes;
        String util;
        int size;

        //separar a string em partes
        util = encomenda.substring(encomenda.indexOf(':')+1);
        partes = util.split(",");
        size = partes.length;

        //separar os componentens da linha de encomenda e inicializá-la
        String[] linhaEnc = new String[size-4];
        System.arraycopy(partes,4,linhaEnc,0,size-4);
        List<LinhaEncomenda> linhaEncList = processaLinhaEnc(linhaEnc);

        //inserir tudo na encomenda
        enc.setNumEnc(partes[0]);
        enc.setData(LocalDateTime.now());
        enc.setNome(partes[1]);
        enc.setElementos(linhaEncList);
        enc.setPeso(Double.parseDouble(partes[3]));

        //guardar encomenda
        Set<Encomenda> encSet= encomendas.get(partes[2]);
        if(encSet != null) {
            encSet.add(enc);
        } else{
            encSet = new HashSet<>();
            encSet.add(enc);
            encomendas.put(partes[2],encSet);
        }

    }

    /**
     * Processa a informação de uma encomenda aceite em formato de String
     * @param encAceiteString A informação de uma encomenda aceite em formato de String
     */
    private void processaEncomendaAceite(String encAceiteString){
        String util;

        //separar a parte incial dos codigos
        util = encAceiteString.substring(encAceiteString.indexOf(':')+1);

        //adicionar à lista de encomendas aceites
        encomendasAceites.add(util);
    }

}
