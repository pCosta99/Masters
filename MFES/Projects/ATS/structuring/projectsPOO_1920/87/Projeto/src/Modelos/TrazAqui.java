package Modelos;

import Comparadores.QtdEncomendasReverseComparator;
import Comparadores.TotalKMsComparator;
import Exceptions.AlreadyEvaluatedException;
import Exceptions.ProdutoInexistenteException;
import Exceptions.UserInexistenteException;
import Geral.GPS;
import Readers.Parser;
import Stock.Encomenda;
import Stock.EncomendaRealizadaUtilizador;
import Stock.InfoProduto;
import Stock.LinhaEncomenda;
import Users.*;

import java.io.*;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class TrazAqui implements TrazAquiModel, Serializable {
    boolean logged;
    private Map<String, User> users; //Map de utilizadores, voluntários, lojas e transportadoras
    private List<Encomenda> encomendas;
    private User logged_user;

    /**
     * Construtor padrão
     */
    public TrazAqui() {
        this.users = new TreeMap<>();
        this.encomendas = new ArrayList<>();
        this.logged = false;
        this.logged_user = null;
    }

    /**
     * Método que interpreta os pedidos do utilizador, servindo de intermedio entre o modelo e o controlador.
     * @param num Caso a averiguar.
     * @param l Lista com parâmetros necessários.
     * @return Lista com os resultados.
     * @throws IOException Exceção.
     */
    public Collection<Object> interpreta(int num, Collection<Object> l) throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        List<Object> lista = new ArrayList<>(l);
        switch (num) {
            case 1:
                addUser(lista);
                break;
            case 2:
                novaEncomenda(lista);
                break;
            case 3:
                return encomendasFeitasUtilizador();
            case 4:
                return encomendasEmEsperaUtilizador();
            case 5:
                return lojasDoSistema();
            case 6:
                return produtosDaLoja((String) lista.get(0));
            case 7:
                return encomendasDisponiveis();
            case 8:
                return entregarEncomenda((String) lista.get(0));
            case 9:
                alterarDisponibilidade((Integer) lista.get(0));
                break;
            case 10:
                return classifcacaoVoluntario();
            case 11:
                return produtosDaLoja();
            case 12:
                return encomendasDaLoja();
            case 13:
                alterarInformarLoja();
                break;
            case 14:
                adicionaProdutoAoStock((String) lista.get(0), (String) lista.get(1), (Double) lista.get(2));
                break;
            case 15:
                return entregarEncomendaTransportadora((String) lista.get(0));
            case 16:
                return encomendasDisponiveisTransportadora();
            case 17:
                return encomendasParaAceitarUtilizador();
            case 18:
                return aceitarEncomendaDaTransportadora((String) lista.get(0), (String) lista.get(1));
            case 19:
                alterarDisponibilidadeTransportadora((Integer) lista.get(0));
                break;
            case 20:
                return calculaFaturadoEntreDatas((Integer) lista.get(0), (Integer) lista.get(1), (Integer) lista.get(2), (Integer) lista.get(3), (Integer) lista.get(4), (Integer) lista.get(5));
            case 21:
                return entregadores();
            case 22:
                classificaEntregador((String) lista.get(0), (Integer) lista.get(1));
                break;
            case 23:
                gravarTrazAqui((String) lista.get(0));
                break;
            case 24:
                return classificacaoTransportadora();
            case 25:
                return top10users();
            case 26:
                return top10transportadoras();
            case 27:
                return lojaInforma();
            case 28:
                alteraTempoAtendimento((Double) lista.get(0));
                break;
            default:
        }
        return null;
    }

    /*-------------------------------------------FUNCIONALIDADES UTILIZADOR-------------------------------------------*/

    /**
     * Método que adiciona uma nova encomenda ao sistema e a loja respetiva.
     *  @param lista Lista com os dados relativos à encomenda.
     * @throws UserInexistenteException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    public void novaEncomenda(List<Object> lista) throws UserInexistenteException, ProdutoInexistenteException {
        List<LinhaEncomenda> aux = new ArrayList<>();
        int i = 0;
        double peso = 0;
        String codL = (String)lista.get(i++);
        if(!this.users.containsKey(codL)) throw new UserInexistenteException(codL);
        Loja l = (Loja) this.users.get(codL);
        do {
            if (lista.size() == i) break;
            String codP = (String) lista.get(i++);
            if (!l.getProdutos().containsKey(codP)) throw new ProdutoInexistenteException(codP);
            InfoProduto info = l.getProdutos().get(codP);
            peso += info.getPeso();
            int qtd = (int) lista.get(i++);
            aux.add(new LinhaEncomenda(codP, info.getNome(), qtd, info.getPreco()));
        }while(true);
        String codEnc = "e" + randomNumber();
        this.encomendas.add(new Encomenda(codEnc, this.logged_user.getCode(), codL, peso, aux));
        l.addEncomendaOnHold(new Encomenda(codEnc, this.logged_user.getCode(), codL, peso, aux));
        Utilizador user = (Utilizador) this.logged_user;
        user.addEncomendaOnHold(new Encomenda(codEnc, this.logged_user.getCode(), codL, peso, aux));
        this.users.put(l.getCodigo(),l);
        this.users.put(user.getCodigo(),user);
    }

    /**
     * Método que devolve uma lista com encomendas feitas e entregues ao utilizador.
     * @return Lista com as encomendas.
     */
    public List<Object> encomendasFeitasUtilizador() {
        Utilizador user = (Utilizador) getUser(this.logged_user.getCode());
        List<Object> aux = new ArrayList<>();
        for (EncomendaRealizadaUtilizador e : user.getEncomendasFeitas())
            aux.add(e.getData_entrega().toString() + "\n" + e.toString());
        return aux;
    }

    /**
     * Método que devolve uma lista com as encomendas à espera de serem entregues.
     * @return Lista com as encomendas.
     */
    public List<Object> encomendasEmEsperaUtilizador() {
        Utilizador user = (Utilizador) this.logged_user;
        List<Object> aux = new ArrayList<>();
        for (Encomenda e : user.getOnHold())
            aux.add(e.toString());
        return aux;
    }

    /**
     * Método que devolve uma lista com todas as lojas do sistema.
     * @return Lista com as lojas.
     */
    public List<Object> lojasDoSistema() {
        List<Object> aux = new ArrayList<>();
        for (User u : this.users.values()) {
            if (u instanceof Loja)
                aux.add(u.getCode() + " -> " + ((Loja) u).getNome());
        }
        return aux;
    }

    /**
     * Método que devolve uma lista com os produtos de uma dada loja.
     * @param codLoja Código da loja a averiguar.
     * @return Lista com os produtos e informação respetiva.
     * @throws UserInexistenteException Exceção.
     */
    public List<Object> produtosDaLoja(String codLoja) throws UserInexistenteException {
        if(!this.users.containsKey(codLoja)) throw new UserInexistenteException(codLoja);
        Loja l = (Loja) getUser(codLoja);
        List<Object> aux = new ArrayList<>();
        for (Map.Entry<String, InfoProduto> prod : l.getProdutos().entrySet()) {
            aux.add(prod.getKey() + " -> " + prod.getValue().getNome() + " - " + prod.getValue().getPreco()+ "€");
        }
        return aux;
    }

    /**
     * Método que devolve uma lista com encomendas à espera de serem aceites pelo utilizador.
     * @return Lista com as encomendas.
     */
    public List<Object> encomendasParaAceitarUtilizador() {
        Utilizador u = (Utilizador) this.logged_user;
        List<Object> aux = new ArrayList<>();
        for (Map.Entry<String, Encomenda> e : u.getPorAceitar().entrySet())
            aux.add(e.getValue().getCodEncomenda() + " -> " + e.getKey());
        return aux;
    }

    /**
     * Método que aceita ou rejeita uma encomenda por parte do utilizador e atualiza o sistema.
     * @param code Código da encomenda a averiguar.
     * @param answer String que diz se aceita ou não aceita.
     * @return Caso tenha sido aceite, insere um boolean na lista para podermos confirmar ao utilizador que a encomenda foi feita, caso contrário devolve uma lista vazia.
     */
    public List<Object> aceitarEncomendaDaTransportadora(String code, String answer) {
        Utilizador u = (Utilizador) this.logged_user;
        double ptotal = 0;
        double clima = generateClima();
        List<Object> aux = new ArrayList<>();
        for (Map.Entry<String, Encomenda> e : u.getPorAceitar().entrySet()) {
            if (e.getValue().getCodEncomenda().compareTo(code) == 0) {
                if (answer.compareTo("S") == 0) {
                    Loja l = (Loja) getUser(e.getValue().getCodLoja());
                    Transportadora t = (Transportadora) getUser(e.getKey());
                    e.getValue().setDataEntrega(LocalDateTime.now());
                    for (LinhaEncomenda le : e.getValue().getProdutos())
                        ptotal += le.getValorUnitario();
                    t.addEncomendaRealizada(e.getValue().getCodEncomenda(), e.getValue().getCodUtilizador(), e.getValue().getCodLoja(), (Duration.between(e.getValue().getDataEncomenda(), e.getValue().getDataEntrega()).toMinutes() + l.getTempo_médio_atendimento()*l.getQtd_pessoas_fila()) * clima, e.getValue().getPeso(), ptotal, t.getGPS().distanceTo(l.getGPS()) + l.getGPS().distanceTo(u.getGps()));
                    u.addEncomendaRealizada(e.getValue(), (Duration.between(e.getValue().getDataEncomenda(), e.getValue().getDataEntrega()).toMinutes() + l.getTempo_médio_atendimento()*l.getQtd_pessoas_fila())*clima, e.getKey());
                    u.removePorAceitar(e.getKey());
                    u.addCodeParaClassificar(e.getKey());
                    l.removeEncomenda(e.getValue());
                    this.users.put(t.getCode(), t);
                    this.users.put(u.getCodigo(), u);
                    this.users.put(l.getCodigo(), l);
                    aux.add(true);
                    return aux;
                }else{
                    u.removePorAceitar(e.getKey());
                    this.encomendas.add(e.getValue().clone());
                    this.users.put(u.getCodigo(),u);
                }
            }
        }
        return aux;
    }

    /**
     * Método que devolve um Set com os códigos dos Voluntários e/ou Transportadoras que um utilizador tem por avaliar.
     * @return Set com os códigos.
     */
    public Set<Object> entregadores() {
        Utilizador u = (Utilizador) this.logged_user;
        Set<Object> aux = new TreeSet<>();
        for (String e : u.getPorClassificar()) {
            aux.add(e);
        }
        return aux;
    }

    /**
     * Método que classifica um entregador.
     * @param codEntregador Código do entregador.
     * @param classifica Classificação dada pelo utilizador.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     */
    public void classificaEntregador(String codEntregador, int classifica) throws UserInexistenteException, AlreadyEvaluatedException {
        Utilizador u = (Utilizador) this.logged_user;
        if(!u.getPorClassificar().contains(codEntregador)) throw new AlreadyEvaluatedException(codEntregador);
        if (codEntregador.charAt(0) == 'v') {
            if(!this.users.containsKey(codEntregador)) throw new UserInexistenteException(codEntregador);
            Voluntario v = (Voluntario) getUser(codEntregador);
            v.updateClassificacao(classifica);
            u.removeCodePorAceitar(codEntregador);
            this.users.put(v.getCodigo(), v);
            this.users.put(u.getCodigo(), u);
        }
        if (codEntregador.charAt(0) == 't') {
            if(!this.users.containsKey(codEntregador)) throw new UserInexistenteException(codEntregador);
            Transportadora t = (Transportadora) getUser(codEntregador);
            t.updateClassificacao(classifica);
            u.removeCodePorAceitar(codEntregador);
            this.users.put(t.getCode(), t);
            this.users.put(u.getCodigo(), u);
        }
    }

    /**
     * Método que devolve uma lista com os 10 utilizadores que mais encomendas realizadas possuem.
     * @return LIsta com os utilizadores e informação respetiva.
     */
    public List<Object> top10users(){
        Set<Utilizador> aux = new TreeSet<>(new QtdEncomendasReverseComparator());
        List<Object> res = new ArrayList<>();
        int i = 0;
        for (Map.Entry<String,User> e : this.users.entrySet()){
            if(e.getValue() instanceof Utilizador){
                aux.add((Utilizador) e.getValue());
            }
        }
        for (Utilizador u : aux){
            if(i<10){
                res.add(u.getNome() + " -> " + u.getEncomendasFeitas().size());
                i++;
            }
        }
        return res;
    }

    /*----------------------------------------------------------------------------------------------------------------*/

    /*-------------------------------------------FUNCIONALIDADES VOLUNTÁRIO-------------------------------------------*/

    /**
     * Método que devolve uma lista com as encomendas que estejam no raio de ação de um voluntário.
     * @return Lista com as encomendas.
     */
    public List<Object> encomendasDisponiveis() {
        Voluntario v = (Voluntario) this.logged_user;
        List<Object> aux = new ArrayList<>();
        for (Encomenda e : this.encomendas) {
            Utilizador u = (Utilizador) getUser(e.getCodUtilizador());
            Loja l = (Loja) getUser(e.getCodLoja());
            if (l.getGPS().distanceTo(v.getGps()) <= v.getRaio() && l.getGPS().distanceTo(u.getGps()) <= v.getRaio())
                aux.add(e.toString());
        }
        return aux;
    }

    /**
     * Método que permite ao voluntário efetuar uma encomenda.
     * @param codEnc Código da encomenda a entregar.
     * @return List na qual se insere true caso a encomenda exista e seja passível de ser entregue, caso contrário devolve uma lista vazia.
     */
    public List<Object> entregarEncomenda(String codEnc) {
        Voluntario v = (Voluntario) this.logged_user;
        List<Object> aux = new ArrayList<>();
        double clima = generateClima();
        if (v.isDisponivel()) {
            for (Encomenda e : this.encomendas) {
                if (e.getCodEncomenda().compareTo(codEnc) == 0) {
                    Loja l = (Loja) getUser(e.getCodLoja());
                    Utilizador u = (Utilizador) getUser(e.getCodUtilizador());
                    if (l.getGPS().distanceTo(v.getGps()) <= v.getRaio() && l.getGPS().distanceTo(u.getGps()) <= v.getRaio()) {
                        e.setDataEntrega(LocalDateTime.now());
                        v.addEncomendaRealizada(codEnc, e.getCodUtilizador(), e.getCodLoja(), (Duration.between(e.getDataEncomenda(), e.getDataEntrega()).toMinutes() + l.getTempo_médio_atendimento() * l.getQtd_pessoas_fila()) * clima);
                        u.addEncomendaRealizada(e, Duration.between(e.getDataEncomenda(), e.getDataEntrega()).toMinutes() + l.getTempo_médio_atendimento() * l.getQtd_pessoas_fila(), v.getCodigo());
                        l.removeEncomenda(e);
                        u.addCodeParaClassificar(v.getCode());
                        u.removeOnHold(e);
                        this.encomendas.remove(e);
                        this.users.put(u.getCodigo(),u);
                        this.users.put(l.getCodigo(), l);
                        aux.add(true);
                        return aux;
                    }
                }
            }
        }
        return aux;
    }

    /**
     * Método que permite alterar a disponibilidade de um voluntário para fazer entregas.
     * @param e Que é a opção de como pretende mostrar-se.
     */
    public void alterarDisponibilidade(int e) {
        Voluntario v = (Voluntario) this.logged_user;
        v.setDisponivel(e == 1);
    }

    /**
     * Método que devolve a classificação de um voluntário.
     * @return Classificação.
     */
    public List<Object> classifcacaoVoluntario() {
        Voluntario v = (Voluntario) this.logged_user;
        List<Object> aux = new ArrayList<>();
        aux.add(v.getClassificação());
        return aux;
    }

    /*----------------------------------------------------------------------------------------------------------------*/

    /*----------------------------------------------FUNCIONALIDADES LOJA----------------------------------------------*/

    /**
     * Método que devolve um Set com os produtos da  loja.
     * @return Set com os produtos.
     */
    public Set<Object> produtosDaLoja() {
        Set<Object> aux = new TreeSet<>();
        Loja l = (Loja) this.logged_user;
        for (Map.Entry<String, InfoProduto> e : l.getProdutos().entrySet())
            aux.add(e.getValue().getNome() + " -> " + e.getKey() + " -> " + e.getValue().getPreco() + "€");
        return aux;
    }

    /**
     * Método de que devolve uma lista com as encomendas à espera de ser entregues nessa loja.
     * @return List com as encomendas.
     */
    public List<Object> encomendasDaLoja() {
        List<Object> aux = new ArrayList<>();
        Loja l = (Loja) this.logged_user;
        for (Encomenda e : l.getEncomendas())
            aux.add(e.toString());
        return aux;
    }

    /**
     * Método que altera se a Loja informa sobre a sua fila de espera ou não.
     */
    public void alterarInformarLoja() {
        Loja l = (Loja) this.logged_user;
        if (l.isInforma_sobre_loja()) l.setInforma_sobre_loja(false);
        else l.setInforma_sobre_loja(true);
    }

    /**
     * Método que adiciona um produto ao stock.
     * @param code Código do produto a adicionar.
     * @param desc Nome do produto a adicionar.
     * @param preco Preço a que o produto vai ser vendido.
     */
    public void adicionaProdutoAoStock(String code, String desc, double preco) {
        Loja l = (Loja) this.logged_user;
        l.addProdLoja(code, desc, preco, randomPeso());
    }

    /**
     * Método que retorna uma lista com algo se a Loja informar sobre a sua fila de espera.
     * @return List.
     */
    public List<Object> lojaInforma(){
        List<Object> aux = new ArrayList<>();
        Loja l = (Loja) this.logged_user;
        if(l.isInforma_sobre_loja()) aux.add(true);
        return aux;
    }

    /**
     * Método que altera o tempo de atendimento de uma loja.
     * @param tempo Tempo de atendimento.
     */
    public void alteraTempoAtendimento(double tempo){
        Loja l = (Loja) this.logged_user;
        l.setTempo_médio_atendimento(tempo);
    }

    /*----------------------------------------------------------------------------------------------------------------*/

    /*---------------------------------------------FUNCIONALIDADES EMPRESA--------------------------------------------*/

    /**
     * Método que devolve uma lista com encomendas no raio de ação da transportadora disponíveis para entrega.
     * @return Lista com encomendas.
     */
    public List<Object> encomendasDisponiveisTransportadora() {
        List<Object> aux = new ArrayList<>();
        Transportadora t = (Transportadora) this.logged_user;
        for (Encomenda e : this.encomendas) {
            Utilizador u = (Utilizador) getUser(e.getCodUtilizador());
            Loja l = (Loja) getUser(e.getCodLoja());
            if (l.getGPS().distanceTo(t.getGPS()) <= t.getRaio() && l.getGPS().distanceTo(u.getGps()) <= t.getRaio())
                aux.add(e.toString());
        }
        return aux;
    }

    /**
     * Método que envia ao utilizador a intenção de entregar a sua encomenda.
     * @param codEnc Código da encomenda a entregar.
     * @return List com true caso seja possível realizar a entrega.
     */
    public List<Object> entregarEncomendaTransportadora(String codEnc) {
        Transportadora t = (Transportadora) this.logged_user;
        List<Object> aux = new ArrayList<>();
        if (t.getDisponibilidade()) {
            for (Encomenda e : this.encomendas) {
                if (e.getCodEncomenda().compareTo(codEnc) == 0 && t.getOnHold().size() < (t.getNMaximo() + 1)) {
                    Utilizador u = (Utilizador) getUser(e.getCodUtilizador());
                    Loja l = (Loja) getUser(e.getCodLoja());
                    if (l.getGPS().distanceTo(t.getGPS()) <= t.getRaio() && l.getGPS().distanceTo(u.getGps()) <= t.getRaio()) {
                        t.addEncomendaParaAceitar(e);
                        u.addEncomendaParaAceitar(t.getCode(), e);
                        this.users.put(u.getCodigo(), u);
                        this.encomendas.remove(e);
                        aux.add(true);
                        return aux;
                    }
                }
            }
        }
        return aux;
    }

    /**
     * Método que permite alterar a disponibilidade de uma Transportadora para efetuar entregas.
     * @param e Modo como se pretende mostrar.
     */
    public void alterarDisponibilidadeTransportadora(int e) {
        Transportadora v = (Transportadora) this.logged_user;
        v.setDisponivel(e == 1);
    }

    /**
     * Método que calcula o total faturado em custos de transporte entre duas datas.
     * @param y1 Ano de inicio.
     * @param m1 Mes de inicio.
     * @param d1 Dia de inicio.
     * @param y2 Ano de fim.
     * @param m2 Mes de fim.
     * @param d2 Dia de fim.
     * @return Valor calculado.
     */
    public List<Object> calculaFaturadoEntreDatas(int y1, int m1, int d1, int y2, int m2, int d2) {
        LocalDateTime i = LocalDateTime.of(y1, m1, d1, 0, 0);
        LocalDateTime f = LocalDateTime.of(y2, m2, d2, 0, 0);
        List<Object> aux = new ArrayList<>();
        Transportadora t = (Transportadora) this.logged_user;
        double fat = t.calculaFat(i, f);
        aux.add(fat);
        return aux;
    }

    /**
     * Método que devolve a classficação de uma dada Transportadora.
     * @return Classificação.
     */
    public List<Object> classificacaoTransportadora() {
        Transportadora t = (Transportadora) this.logged_user;
        List<Object> aux = new ArrayList<>();
        aux.add(t.getClassificação());
        return aux;
    }

    /**
     * Método que calcula o top10 de Transportadoras em termos de quilómetros percorridos.
     * @return List com o top10 de Transportadoras e a sua informação.
     */
    public List<Object> top10transportadoras(){
        Set<Transportadora> aux = new TreeSet<>(new TotalKMsComparator());
        List<Object> res = new ArrayList<>();
        int i = 0;
        for (Map.Entry<String,User> e : this.users.entrySet()){
            if(e.getValue() instanceof Transportadora){
                aux.add((Transportadora) e.getValue());
            }
        }
        for (Transportadora u : aux){
            if(i<10){
                res.add(u.getNome() + " -> " + u.getTotalKM());
                i++;
            }
        }
        return res;
    }

    /*----------------------------------------------------------------------------------------------------------------*/

    /**
     * Método que adiciona uma encomenda à lista de encomendas em espera de um utilizador.
     *
     * @param e Encomenda feita.
     */
    public void addEncomendaToUtilizador(Encomenda e) {
        Utilizador u = (Utilizador) this.users.get(e.getCodUtilizador());
        u.addEncomendaOnHold(e.clone());
    }

    /**
     * Método que adiciona a uma da loja informação sobre um produto comprado na mesma.
     *
     * @param e Encomenda feita.
     */
    public void addProdutoALoja(Encomenda e) {
        Loja l = (Loja) this.users.get(e.getCodLoja());
        for (LinhaEncomenda le : e.getProdutos()) {
            l.addProdLoja(le.getCodProduto(), le.getDescricao(), le.getValorUnitario(), (e.getPeso() / e.getProdutos().size()));
        }
    }

    /**
     * Método que adiciona uma encomenda à fila de espera de uma loja.
     *
     * @param e Encomenda realizada.
     */
    public void addEncomendaALoja(Encomenda e) {
        Loja l = (Loja) this.users.get(e.getCodLoja());
        l.addEncomendaOnHold(e.clone());
        l.incrementaFilaEspera();
    }

    /**
     * Método que retorna um User com um dado código.
     * @param codigo Código a averiguar.
     * @return User correspondente ao código.
     */
    public User getUser(String codigo) {
        return this.users.get(codigo).clone();
    }

    /**
     * Obtém a lista de users
     * @return List
     */
    public List<User> getListaUsers() {
        return this.users.values().stream().map(User::clone).collect(Collectors.toList());
    }

    /**
     * Método que gera um numero aleatório.
     * @return Número gerado.
     */
    public int randomNumber() {
        Random random = new Random();
        return random.nextInt(9999);
    }

    /**
     * Método que gera um peso aleatório até 50.
     * @return Peso gerado.
     */
    public double randomPeso(){
        Random random = new Random();
        return random.nextDouble() * random.nextInt(500);
    }

    /**
     * Adiciona um Utilizador
     * @param u Utilizador
     */
    public void addUtilizador(User u) {
        users.put(u.getCode(), u);
    }

    /**
     * Método que adiciona um User com os devidos parâmetros.
     * @param l que contém a informação sobre o User.
     */
    public void addUser(List<Object> l) {
        switch (((String) l.get(0)).charAt(0)) {
            case 'u':
                users.put((String) l.get(0), new Utilizador((String) l.get(0), (String) l.get(2), (String) l.get(1), new GPS((Double) l.get(3), (Double) l.get(4))));
                break;
            case 'v':
                users.put((String) l.get(0), new Voluntario((String) l.get(0), (String) l.get(2), (String) l.get(1), new GPS((Double) l.get(3), (Double) l.get(4)), (Boolean) l.get(5), (Double) l.get(6)));
                break;
            case 't':
                users.put((String) l.get(0), new Transportadora((String) l.get(0), (String) l.get(2), (String) l.get(1), new GPS((Double) l.get(3), (Double) l.get(4)), (String) l.get(5), (Double) l.get(6), (Double) l.get(7), (Integer) l.get(8)));
                break;
            case 'l':
                users.put((String) l.get(0), new Loja((String) l.get(0), (String) l.get(2), (String) l.get(1), new GPS((Double) l.get(3), (Double) l.get(4)), (Boolean) l.get(5), (Double) l.get(6)));
                break;
        }
    }

    /**
     * Verifica se existe user válido
     * @param username Username inserido pelo Users.Utilizador
     * @param password Password inserida pelo Users.Utilizador
     * @return boolean
     */
    public boolean checkLoggin(String username, String password) throws UserInexistenteException {
        if(!this.users.containsKey(username)) throw new UserInexistenteException(username);
        User u = this.users.get(username);
        if (u == null) return false;
        else if (u.getPassword().equals(password)) {
            setLogged(true, this.users.get(username).clone());
            return true;
        } else return false;
    }

    /**
     * Método que adiciona uma encomenda ao Sistema.
     * @param e Encomenda lida.
     */
    public void addEncomenda(Encomenda e) {
        this.encomendas.add(e);
    }

    /**
     * Método que devolve a lista de encomendas no sistema.
     * @return Lista de encomendas.
     */
    public List<Encomenda> getEncomendas() {
        ArrayList<Encomenda> aux = new ArrayList<>();
        for (Encomenda e : this.encomendas)
            aux.add(e.clone());
        return aux;
    }

    /**
     * Obtém o logged user
     * @return int
     */
    public User getLogged() {
        return logged_user;
    }

    /**
     * Faz o logout do user
     */
    public void logout() {
        this.logged_user = null;
    }

    /**
     * Define o logged number
     * @param logged Logged number
     */
    public void setLogged(boolean logged, User u) {
        this.logged = logged;
        this.logged_user = u;
    }

    /**
     * Método que grava o estado atual do programa num ficheiro objeto.
     * @param filename Nome com que vai ser gravado o ficheiro.
     * @throws IOException Exceção.
     */
    public void gravarTrazAqui(String filename) throws IOException {
        ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream(filename));
        o.writeObject(this);
        o.flush();
        o.close();
        Parser aux = new Parser();
        aux.wparser(this);
    }

    /**
     * Método que permite carrega o estado do programa a partir de um ficheiro objeto.
     * @param filename que é o nome do ficheiro
     * @return Programa com um estado.
     * @throws IOException            Exceção.
     * @throws ClassNotFoundException Exceção.
     */
    public static TrazAqui loadTrazAqui(String filename) throws IOException, ClassNotFoundException {
        ObjectInputStream o = new ObjectInputStream(new FileInputStream(filename));
        TrazAqui d = (TrazAqui) o.readObject();
        o.close();
        return d;
    }


    /**
     * Método que gera um valor correspondente a um clima aleatório.
     * @return Valor que afeta o tempo de entrega consoante o clima.
     */
    public double generateClima(){
        Random random = new Random();
        int clima = random.nextInt(10); // 0-2 -> Chuva ; 3-5 -> Nevoeiro ; 6-8 -> Neve ; 9-10 -> Limpo
        if(clima>=0 && clima<=2) return 1.5;
        else if(clima>=3 && clima<=5) return 1.25;
        else if(clima>=6 && clima<=8) return 2;
        else return 1;
    }


    /**
     * Método que define a password de um dado User.
     * @param codUser Código do User.
     * @param pw Password a associar.
     */
    public void setPassword(String codUser, String pw) {
        this.users.get(codUser).setPassword(pw);
    }

}
