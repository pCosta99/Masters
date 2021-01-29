package Projeto.Model;
import Projeto.Encomenda.Encomenda;
import Projeto.Encomenda.LinhaDeEncomenda;
import Projeto.Entidades.*;
import Projeto.Exceptions.IdRepetidoException;
import Projeto.Interfaces.*;
import Projeto.Util.GPS;
import Projeto.Util.Estado;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.nio.file.Files;

/**
* Classe que implementa o model(TrazAqui).
* Esta classe tem o poder de gerir e manipular todos os dados correspondentes às classes Utilizador,
* Loja, Empresa, Voluntario e Encomenda.
* Foi criado com o propósito de "combinar" e controlar tudo aquilo que provém das classes anteriormente referidas.
*/
public class TrazAqui implements Serializable {
    /**
     * Variáveis de instância:
     * correspondem aos maps de utilizadores, lojas, empresas, voluntários e encomendas.
     * A key de cada um é o respetivo id.
     */
    private final Map<String, IUtilizador> clientes;
    private final Map<String, ILoja> lojas;
    private final Map<String, IEmpresa> emps;
    private final Map<String, IVoluntario> vols;
    private final Map<String, IEncomenda> encs;


    /**
     * Construtor por omissao de TrazAqui.
     */
    public TrazAqui() {
        this.clientes = new TreeMap<>();
        this.lojas = new TreeMap<>();
        this.emps = new TreeMap<>();
        this.vols = new TreeMap<>();
        this.encs = new TreeMap<>();
    }

    /*
     * Métodos de instância
     */
    /**
     * Metodo que retorna a lista de lojas que aderirem a app.
     */
    public Collection<ILoja> getLojas() {
        return this.lojas.values().stream()
                .map(ILoja::clone)
                .collect(Collectors.toList());
    }

    /**
     * Metodo que retorna a lista de empresas que aderiram a app.
     */
    public Collection<IEmpresa> getEmpresas() {
        return this.emps.values().stream()
                .map(IEmpresa::clone)
                .collect(Collectors.toList());
    }

    /**
     * Metodo que retorna a lista de clientes que aderiram a app.
     */
    public Collection<IUtilizador> getClientes() {
        return this.clientes.values().stream()
                .map(IUtilizador::clone)
                .collect(Collectors.toList());
    }

    public Collection<IVoluntario> getVoluntarios() {
        return this.vols.values().stream()
                .map(IVoluntario::clone)
                .collect(Collectors.toList());
    }

    public IEncomenda getEncomenda(String id) {
        return this.encs.get(id);
    }

    public Collection<IAviso> getAvisosEmpresa(String idEmpresa) {
        return this.emps.get(idEmpresa).getNotificacoes();
    }

    /**
     * Metodo que adiciona uma cliente ao map de Clientes.
     * @param c - Cliente em questao
     */
    public void addCliente(IUtilizador c) throws IdRepetidoException {
        if(!this.clientes.containsKey(c.getId())) {
            this.clientes.put(c.getId(), c.clone());
        } else throw new IdRepetidoException("Id já existente!");
    }

    /**
     * Metodo que adiciona uma loja ao map de Lojas.
     * @param l - loja a ser adicionada
     */
    public void addLoja(ILoja l) throws IdRepetidoException {
        if (!this.lojas.containsKey(l.getId())) {
            this.lojas.put(l.getId(), l.clone());
        } else throw new IdRepetidoException("Id já existente!");
    }

    /**
     * Metodo que adiciona uma empresa ao map de empresas.
     * @param e - empresa a ser adicionada
     */
    public void addEmpresa(IEmpresa e) throws IdRepetidoException {
        if (!this.emps.containsKey(e.getId())) {
            this.emps.put(e.getId(), e.clone());
        } else throw new IdRepetidoException("Id já existente!");
    }

    /**
     * Metodo que adiciona um voluntario ao map de Voluntarios.
     * @param v - voluntario a ser adicionado
     */
    public void addVols(IVoluntario v) throws IdRepetidoException {
        if(!this.vols.containsKey(v.getId())) {
            this.vols.put(v.getId(), v.clone());
        } else throw new IdRepetidoException("Id já existente!");
    }

    /**
     * Método que adiciona uma encomenda ao Map.
     */
    public void addEncomenda(IEncomenda enc) {
        this.encs.put(enc.getID(), enc);
    }

    /**
     * Método que remove uma encomenda ao Map.
     */
    public void removeEncomenda(IEncomenda enc) {
        this.encs.remove(enc.getID(), enc);
    }

    /**
     * Metodo que remove um cliente do map de clientes.
     * @param c - cliente a ser removido
     */
    public void remCliente(IUtilizador c) {
        this.clientes.remove(c.getId());
    }
    /**
     * Metodo que remove uma loja do map de lojas.
     * @param l - loja a ser removida
     */
    public void remLoja(ILoja l) {
        this.lojas.remove(l.getId());
    }
    /**
     * Metodo que remove uma empresa do map de empresas.
     * @param e - empresa a ser removida
     */
    public void remEmpresa(IEmpresa e) {
        this.emps.remove(e.getId());
    }
    /**
     * Metodo que remove um voluntario do map de voluntarios.
     * @param v - voluntario a ser removido
     */
    public void remVols(IVoluntario v) {
        this.vols.remove(v.getId());
    }

    /**
     * Metodo que retorna uma loja através do seu id.
     */
    public ILoja getLoja(String idLoja) {
        return this.lojas.get(idLoja);
    }

    /**
     * Metodo que retorna um voluntario através do seu id.
     */
    public IVoluntario getVoluntario(String idVoluntario) {
        return this.vols.get(idVoluntario);
    }

    /**
     * Metodo que retorna uma empresa atraves do seu id.
     */
    public IEmpresa getEmpresa(String idEmpresa) {
        return this.emps.get(idEmpresa);
    }

    /**
     * Metodo que retorna um cliente atraves do seu id.
     */
    public IUtilizador getCliente(String idCliente) {
        return this.clientes.get(idCliente);
    }

    /**
     * Metodo que adiciona uma encomenda à lista de encomendas do cliente, à fila da loja e
     * à lista do trazAqui.
     * @param e - encomenda a ser adicionada
     */
    public void adicionaEncomenda(IEncomenda e) {
        IUtilizador c = this.clientes.get(e.getUserID());
        ILoja l = this.lojas.get(e.getLojaID());
        this.encs.put(e.getID(), e.clone());
        if(c != null) {
            c.adicionaEnc(e.clone());
        }
        if(l != null) {
            l.adicionaEnc(e.clone());
        }
    }

    public Collection<IEncomenda> encsPorTransportarEmpresa(IEmpresa e) {
        return this.encs.values().stream()
                .filter(enc -> this.lojas.get(enc.getLojaID()).getLocalizacao().isInsideRaio(e.getRaio(), e.getLocalizacao()))
                .map(IEncomenda::clone)
                .collect(Collectors.toList());
    }

    /**
     * Lista dos top n clientes que mais usaram uma determinada empresa
     * @param idEmpresa - id da Empresa
     * @param n - inteiro correspondente ao tamanho da lista
     * @return lista dos n clientes
     */
    public Collection<IUtilizador> topNClientesEmpresa(String idEmpresa, int n) {
        Comparator<IUtilizador> comp = Comparator.comparing(c -> c.getEncomendas().size());
        return this.clientes.values().stream()
                .filter(c -> c.clienteComprouEmpresa(idEmpresa))
                .sorted(comp.reversed())
                .limit(n)
                .map(IUtilizador::clone)
                .collect(Collectors.toList());
    }

    /**
     * Metodo que retorna quantos clientes existem registados na app.
     */
    public int quantosClientes() {
        return this.clientes.size();
    }

    /**
     * Metodo que retorna quantas empresas existem registados na app.
     */
    public int quantasEmps() {
        return this.emps.size();
    }

    /**
     * Metodo que retorna quantas lojas existem registados na app.
     */
    public int quantasLojas() {
        return this.lojas.size();
    }

    /**
     * Metodo que retorna quantos voluntarios existem registados na app.
     */
    public int quantosVols() {
        return this.vols.size();
    }

    /**
     * Metodo que atualiza o tamanho da fila de espera de uma dada loja.
     * @param l - loja em questao
     * @param size - novo tamanho da fila de espera
     */
    public void setSizeFilaLoja(ILoja l, int size) {
        l.setSize(size);
    }

    /**
     * Método que verifica se um dado id existe na estrutura de dados
     * onde estão guardadas as instâncias de clientes.
     */
    public boolean existeCliente(String id) {
        return this.clientes.containsKey(id);
    }

    /**
     * Método que verifica se um dado id existe na estrutura de dados
     * onde estão guardadas as instâncias de lojas.
     */
    public boolean existeLoja(String id) {
        return this.lojas.containsKey(id);
    }

    /**
     * Método que verifica se um dado id existe na estrutura de dados
     * onde estão guardadas as instâncias de empresas.
     */
    public boolean existeEmpresa(String id) {
        return this.emps.containsKey(id);
    }

    /**
     * Método que verifica se um dado id existe na estrutura de dados
     * onde estão guardadas as instâncias de voluntários.
     */
    public boolean existeVoluntario(String id) {
        return this.vols.containsKey(id);
    }

    public Collection<String> topNEmpresasMaisUsadas(int n) {
        Comparator<IEmpresa> comp = Comparator.comparing(e -> e.getEncomendas().size());
        return this.emps.values().stream()
                .sorted(comp.reversed())
                .limit(n)
                .map(e -> e.clone().getId())
                .collect(Collectors.toList());
    }

    /**
     * Lista das Top n empresas que percorreram mais kms
     * @param n - inteiro correspondente ao tamanho da lista
     * @return lista das empresas
     */
    public Collection<String> topNEmpresasDist(int n) {
        Comparator<IEmpresa> comp = Comparator.comparing(IEmpresa::getDist);
        return this.emps.values().stream()
                .sorted(comp.reversed())
                .limit(n)
                .map(e -> e.clone().getId())
                .collect(Collectors.toList());
    }

    public Collection<IUtilizador> topNClientesMaisEncomendaram(int n) {
        Comparator<IUtilizador> comp = Comparator.comparing(x -> x.getEncomendas().size());
        return this.clientes.values().stream()
                                     .sorted(comp.reversed())
                                     .limit(n)
                                     .map(IUtilizador::clone)
                                     .collect(Collectors.toList());
    }

    public Collection<IVoluntario> volsDisponiveis(GPS gps1, GPS gps2, boolean med) {
        return this.vols.values().stream()
                                 .filter(x -> x.getLocalizacao().isInsideRaio(x.getRaio(), gps1)
                                              && x.getLocalizacao().isInsideRaio(x.getRaio(), gps2)
                                              && x.temCapacidade(med))
                                 .collect(Collectors.toList());
    }


    public void encomendaAceite(String id) {
        IEncomenda e = this.encs.get(id);
        if(e != null) {
            this.encs.remove(e.getID());
            String lojaID = e.getLojaID();
            ILoja l = this.lojas.get(lojaID);
            l.removeEnc(e);
        }
    }

    /**
     * Retorna a faturação de uma empresa dentro de um determinado intervalo de tempo
     * @param idEmpresa id da Empresa
     * @param tInicial intervalo inicial
     * @param tFinal intervalo final
     * @return faturação total desse período de tempo
     */
    public float getFaturacaoPeriodo(String idEmpresa, int[] tInicial, int[] tFinal) {
        if (this.getEmpresa(idEmpresa) != null) return 0;
        float res = 0;
        for(IEncomenda enc : this.encs.values())
            if (enc.empTransportouTempo(idEmpresa, tInicial, tFinal))
                res += enc.calculaPrecoTotal()*0.85;
        return res;
    }

    /**
     * Retorna a faturação total de uma empresa num determinado tempo
     * @param idEmpresa id da Empresa
     * @param tempo tempo
     * @return total faturado nesse tempo
     */
    public float getFaturacaoPeriodo(String idEmpresa, int[] tempo) {
        if (this.getEmpresa(idEmpresa) != null) return 0; //exceptions here too
        float res = 0;
        for (IEncomenda enc : this.encs.values())
            if (enc.empTransportouTempo(idEmpresa, tempo))
                res += enc.calculaPrecoTotal()*0.85;
        return res;
    }

    public boolean passCertaCliente(String id, String pw) {
        return this.getCliente(id).getPassword().equals(pw);
    }

    public boolean passCertaLoja(String id, String pw) {
        return this.getLoja(id).getPassword().equals(pw);
    }

    public boolean passCertaEmpresa(String id, String pw) {
        return this.getEmpresa(id).getPassword().equals(pw);
    }

    public boolean passCertaVoluntario(String id, String pw) {
        return this.getVoluntario(id).getPassword().equals(pw);
    }

    /* ************************************** TRATAMENTO DE FICHEIROS ************************************************* */

    /**
     * Grava um ficheiro em binario
     */
    public void gravarObj() throws IOException{
        ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream("Projeto/docs/objeto.txt"));
        o.writeObject(this);
        o.flush();
        o.close();
    }

    /**
     * Le um ficheiro gravado em binario
     */
    public TrazAqui lerObj(String filename)  throws IOException, ClassNotFoundException{
        ObjectInputStream o = new ObjectInputStream(new FileInputStream(filename));
        TrazAqui t = (TrazAqui) o.readObject();
        o.close();
        return t;
    }

    /**
     * Le o ficheiro de logs
     */
     public void parse() {
        Collection<String> linhas = lerFicheiro("Projeto/docs/logs.txt");
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    IUtilizador c = parseCliente(linhaPartida[1]);
                    try {
                        this.addCliente(c);
                    } catch (IdRepetidoException e) {
                        System.out.println("Ups! " + e.getMessage());
                    }
                    break;
                case "Voluntario":
                    IVoluntario v = parseVoluntario(linhaPartida[1]);
                    try {
                        this.addVols(v);
                    } catch (IdRepetidoException e) {
                        System.out.println("Ups! " + e.getMessage());
                    }
                    break;
                case "Transportadora":
                    IEmpresa e = parseEmpresa(linhaPartida[1]);
                    try {
                        this.addEmpresa(e);
                    } catch (IdRepetidoException exc) {
                        System.out.println("Ups! " + exc.getMessage());
                    }
                    break;
                case "Loja":
                    ILoja l = parseLoja(linhaPartida[1]);
                    try {
                        this.addLoja(l);
                    } catch (IdRepetidoException exc) {
                        System.out.println("Ups! " + exc.getMessage());
                    }
                    break;
                case "Encomenda":
                    IEncomenda enc = parseEncomenda(linhaPartida[1]);
                    this.adicionaEncomenda(enc);
                    break;
                case "Aceite":
                    String aceite = linhaPartida[1];
                    this.encomendaAceite(aceite);
                    break;
                default:
                    break;
            }
        }
     }

     private IUtilizador parseCliente(String input){
         String[] campos = input.split(",");

         String id = campos[0];
         String nome = campos[1];
         float longi =  Float.parseFloat(campos[2]);
         float lat =  Float.parseFloat(campos[3]);
         GPS loc = new GPS(lat, longi);

         return new Utilizador(id, id, nome, loc, new ArrayList<>(), "");
     }

     public IVoluntario parseVoluntario(String input){
         String[] campos = input.split(",");

         String id = campos[0];
         String nome = campos[1];
         float longi =  Float.parseFloat(campos[2]);
         float lat =  Float.parseFloat(campos[3]);
         float raio = Float.parseFloat(campos[4]);

         GPS loc = new GPS(lat, longi);
         Collection<IEncomenda> encs = new ArrayList<>();
         Collection<IEncomenda> encsPorEntregar = new ArrayList<>();
         Collection<Float> vels = new ArrayList<>();
         Collection<Integer> cls = new ArrayList<>();
         Estado e = new Estado();

         return new Voluntario(id, id, nome, loc, encs, vels, raio, false, cls, e, 1, encsPorEntregar);
     }

     public IEmpresa parseEmpresa(String input) {
        String[] campos = input.split(",");

        String id = campos[0];
        String nome = campos[1];
        float longi =  Float.parseFloat(campos[2]);
        float lat =  Float.parseFloat(campos[3]);
        String nif = campos[4];
        float raio = Float.parseFloat(campos[5]);
        float taxa = Float.parseFloat(campos[6]);

        GPS loc = new GPS(lat, longi);
        Estado e = new Estado();
        Collection<IEncomenda> encs = new ArrayList<>();
        Collection<IEncomenda> encsPorEntregar = new ArrayList<>();
        Collection<Float> vel = new ArrayList<>();
        Collection<Integer> cls = new ArrayList<>();
        return new Empresa(id, id, nome, loc, encs, vel, raio, false, cls, e, 15, taxa, nif, 0, encsPorEntregar);
     }

     public ILoja parseLoja(String input){
        String[] campos = input.split(",");

        String idLoja = campos[0];
        String nomeLoja = campos[1];
        float lon =  Float.parseFloat(campos[2]);
        float lat =  Float.parseFloat(campos[3]);

        GPS loc = new GPS(lat, lon);
        Collection<IEncomenda> encs = new ArrayList<>();
        Collection<IProduto> listProds = new ArrayList<>();
        // Criar os produtos.
        // Cada loja vai ter uma lista de produtos predefinidos para o bom funcionamento da app.
        // Isto só vai acontecer para a leitura dos logs. Nos outros casos é possivel ter lojas sem produtos.
        IProduto p1 = new Produto("Água", "p1", 0.2, 0.1f, false);
        IProduto p2 = new Produto("T-Shirt", "p2", 0.01, 10, false);
        IProduto p3 = new Produto("Telemóvel", "p3", 1, 250, false);
        IProduto p4 = new Produto("Saco de batatas", "p4", 10, 13, false);
        IProduto p5 = new Produto("Ben-u-ron", "p5", 0.05, 4.5f, true);
        listProds.add(p1); listProds.add(p2); listProds.add(p3); listProds.add(p4); listProds.add(p5);

        return new Loja(idLoja, idLoja, nomeLoja, loc, encs, false, 0, 0, listProds);
     }

     public IEncomenda parseEncomenda(String input){
        String[] campos = input.split(",");

        String idEnc = campos[0];
        String idUser = campos[1];
        String idLoja = campos[2];
        float peso = Float.parseFloat(campos[3]);
        Collection<LinhaDeEncomenda> list = new ArrayList<>();

        for (int i = 4; i < campos.length; i += 4) {
             String codProd = campos[i];
             String nome = campos[i+1];
             int quantidade = (int)Float.parseFloat(campos[i+2]);
             float preco = Float.parseFloat(campos[i+3]);
             Produto prod = new Produto(nome, codProd, peso, preco, false);
             LinhaDeEncomenda l = new LinhaDeEncomenda(prod, quantidade);
             list.add(l);
        }
        return new Encomenda(idEnc, idLoja, "", idUser, peso, list);
     }

     public Collection<String> lerFicheiro(String logs) {
         Collection<String> lines = new ArrayList<>();
         try {
             lines = Files.readAllLines(Paths.get(logs), StandardCharsets.UTF_8);
         } catch(IOException exc) {
             System.out.println(exc.getMessage());
         }
         return lines;
     }
}