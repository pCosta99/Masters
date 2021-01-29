package Models;

import Models.Comparators.EmpresasComparatorByDistance;
import Models.Comparators.UserComparatorByOrders;
import View.View;

import java.io.Serializable;
import java.security.InvalidParameterException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static View.View.MenuOption.*;


/**
 * Models.Sistema principal do progama TrazAqui!.
 *
 * @author (Benjamim Coelho)
 * @author (Henrique Neto)
 * @author (Sara Marques)
 * @version (0)
 */
public class Sistema implements Serializable {
    private final Map<String, Encomenda> pedidos_encomenda;//encomendas solicitadas pelos utilizadores que ainda nao foram aceites por um Models.Agente
    private final Map<String, Utilizador> users; // lista dos utilizadores , keys sao os emails
    private final Map<String, Agente> agentes; // lista dos agentes , keys sao os codigos
    private final Map<String, Loja> lojas; // lista das lojas, keys sao os codigos

    /**
     * Constroi um Sistema Vazio
     */
    public Sistema() {
        this.pedidos_encomenda = new HashMap<>();
        this.users = new HashMap<>();
        this.agentes = new HashMap<>();
        this.lojas = new HashMap<>();
    }

    /**
     * Retorna uma mensagem de boas vindas usando apenas o primeiro nome de um nome completo
     *
     * @param name String do qual se vai retirar o primeiro nome
     */
    static void welcomeName(String name) {
        try {
            View.welcome(name.substring(0, name.indexOf(' ')));
        } catch (StringIndexOutOfBoundsException e) { //caso seja um unico nome (sem espaços)
            View.welcome(name);
        }
    }

    public List<String> getLojas() {
        return this.lojas.values().stream()
                .map(a -> a.getNome() + " - " + a.getCod())
                .collect(Collectors.toList());
    }

    public List<Produto> getProductsFromLoja(String loja) {
        return this.lojas.get(loja).getProductList();
    }
    public List<String> getProductsStringFromLoja(String loja) {
        return this.lojas.get(loja).getProductList()
                .stream().map(Produto::toString).collect(Collectors.toList());
    }

    /**
     * Dado um email e um código, testa se corresponde a um Utilizador já registado no sistema
     *
     * @param mail String com o email
     * @param cod  String com o código
     * @return True se os dados correspondem a um Utilizador já registado, False caso contrário
     */
    public boolean existeUser(String mail, String cod) {
        if (this.users.containsKey(mail)) {
            for (Map.Entry<String, Utilizador> e : this.users.entrySet()) {
                if (e.getValue().getCod().equals(cod)) return true;
            }
        }
        return false;
    }

    /**
     * Dado um código, testa se corresponde a uma Empresa já registada no sistema
     *
     * @param cod String com o código
     * @return True se o código corresponde a uma Empresa já registada, False caso contrário
     */
    public boolean existeEmp(String cod) {
        if (this.agentes.containsKey(cod)) {
            return this.agentes.get(cod) instanceof EmpresaTransportadora;
        }
        return false;
    }

    /**
     * Dado um código, testa se corresponde a um Voluntário já registado no sistema
     *
     * @param cod String com o código
     * @return True se o código corresponde a um Voluntário já registado, False caso contrário
     */
    public boolean existeVol(String cod) {
        if (this.agentes.containsKey(cod)) {
            return this.agentes.get(cod) instanceof Voluntario;
        }
        return false;
    }

    /**
     * Dado um código, testa se corresponde a uma Loja já registada no sistema
     *
     * @param cod String com o código
     * @return True se o código corresponde a uma Loja já registada, False caso contrário
     */
    public boolean existeLoja(String cod) {
        return this.lojas.containsKey(cod);
    }
 /*
    public boolean existeLoja_by_name(String nome) {
        for (Loja l : this.lojas.values()) {
            if (l.getNome().equals(nome)) return true;
        }
        return false;
    } */

    /**
     * Dado o código de um utilizador, devolve o seu email associado
     *
     * @param cod String do código de utilizador
     * @return String do email do utilizador. Devolve null se não existir nenhum utilizador com o código dado.
     */
    private String getUserEmail(String cod) {
        String email = null;
        for (Map.Entry<String, Utilizador> e : this.users.entrySet()) {
            if (e.getValue().getCod().equals(cod)) {
                email = e.getKey();
                break;
            }
        }
        return email;
    }

    /**
     * Lista as Encomendas pendentes (já aceites, ainda não transportadas)
     */
    public void availableEncs() {
        if (this.pedidos_encomenda.isEmpty()) View.show(Nada);
        else
            for (Map.Entry<String, Encomenda> e : this.pedidos_encomenda.entrySet()) {
                View.simpleshow(e.getValue().toString());
            }
    }

    /**
     * Solicita o transporte de uma encomenda pendente por parte de um agente
     *
     * @param agente String do código do agente a realizar o transporte
     * @param codenc String do código da encomenda pendente a aceitar
     */
    public boolean agenteAceitarEnc(String agente, String codenc) throws InvalidParameterException {

        try {
            Encomenda enc = this.pedidos_encomenda.get(codenc);
            String user = getUserEmail(enc.getUser());
            String loja = enc.getLoja();

            if (this.agentes.get(agente).solicitar_transporte(enc, this.users.get(user), this.lojas.get(loja)))
            {
                this.users.get(user).setEstado(true);
                return true;
            }
            else return false;
        } catch (NullPointerException e) {
            e.printStackTrace(System.out);
            throw new InvalidParameterException("Código de encomenda inválido");
        }
    }

    public void setUserStatus(Boolean estado,String email) {
        this.users.get(email).setEstado(estado);
    }

    public boolean getUserStatus(String email) {
        return this.users.get(email).getEstado();
    }


    public void userPedirEnc(String email, String loja, Set<Produto> res) {

        String user = this.users.get(email).getCod();
        Encomenda enc = new Encomenda(loja, user, user + " encomenda", res);

        this.setUserStatus(true,email); // user espera enc

        this.pedidos_encomenda.put(enc.getEnc(), enc); // enc guardada em pedidos pendentes

        this.lojas.get(loja).request(enc.getEnc()); // enc pedida à loja

    }

    /**
     * Cria uma chave nova que nao esteja registada no dado set de chaves
     *
     * @param prefix Prefixo da chave
     * @param keys   Sets de chaves a consultar
     * @return Nova chave que nao esteja registada no dado set de chaves
     */
    private String generateKey(String prefix, Set<String> keys) {
        int i = 0;
        while (keys.contains(prefix + i))
            i++;
        return prefix + i;
    }

    /**
     * Regista um utilizador
     *
     * @param cod      Codigo do Utilizador
     * @param email    Email do Utilizador
     * @param password Password do Utilizador
     * @param Nome     Nome do Utilizador
     * @param x        Latitude do Utilizador
     * @param y        Longitude do Utilizador
     */
    public void registarUtilizador(String cod, String email, String password, String Nome, double x, double y) throws InvalidParameterException {
        if (existeUser(email, cod)) {
            throw new InvalidParameterException("Já existe um utilizador com este código");
        } else {
            if (cod == null) cod = generateKey("u", this.users.keySet());

            Utilizador res = new Utilizador(email, password, cod, Nome, x, y);
            this.users.put(res.getEmail(), res);
        }
    }

    /**
     * Regista um voluntário
     *
     * @param cod  Codigo do Voluntario
     * @param nome Nome do Voluntario
     * @param x    Latitude do Voluntario
     * @param y    Longitude do Voluntario
     * @param r    Raio de ação do Voluntario
     */
    public void registarVoluntario(String cod, String nome, double x, double y, double r, boolean med, boolean d) throws InvalidParameterException {
        if (existeVol(cod)) {
            throw new InvalidParameterException("Já existe um voluntário com este código.");
        } else {
            if (cod == null) cod = generateKey("v", this.agentes.keySet());

            Voluntario res = new Voluntario(nome, cod, 0, 0, x, y, r, med, d);
            this.agentes.put(res.getCodigo(), res);
        }
    }

    /**
     * Regista uma empresa transportadora
     *
     * @param cod  Codigo da Empresa
     * @param nome Nome da Empresa
     * @param x    Latitude da Empresa
     * @param y    Longitude da Empresa
     * @param r    Raio de ação da Empresa
     * @param taxa Preço por Kilometro
     * @param cap  Capacidade de Transporte
     * @param NIF  Numero de Identificação Fiscal da Empresa
     */
    public void registarEmpresa(String cod, String nome, double x, double y, double r, double taxa, int cap, String NIF, boolean med, boolean d)  throws InvalidParameterException{
        if (existeEmp(cod)) {
            throw new InvalidParameterException("Já existe uma empresa com este código.");
        } else {
            if (cod == null) cod = generateKey("t", this.agentes.keySet());

            EmpresaTransportadora res = new EmpresaTransportadora(nome, cod, 0, 0, x, y, r, taxa, cap, 0, NIF, med, d);
            this.agentes.put(res.getCodigo(), res);
        }
    }

    /**
     * Regista uma dada Loja
     *
     * @param cod   Codigo da Loja
     * @param n     Nome da Loja
     * @param x     Latitude da Loja
     * @param y     Longitude da Loja
     * @param Queue True se a Loja permite ver o tamanho das suas filas de espera, False caso contrario
     */
    public void registarLoja(String cod, String n, double x, double y, Boolean Queue, String tipo) throws InvalidParameterException {
        if (existeLoja(cod)) {
            throw new InvalidParameterException("Já existe uma loja com este código.");
        } else {
            if (cod == null) cod = generateKey("l", this.lojas.keySet());

            Loja res;
            if (Queue) res = new Loja_Queue(cod, n, x, y, tipo);
            else res = new Loja(cod, n, x, y, tipo);
            this.lojas.put(res.getCod(), res);
        }
    }

    /**
     * Insere um pedido de Encomenda no Sistema
     *
     * @param x Encomenda a ser registada
     */
    public void inserirPedidoEncomenda(Encomenda x) throws InvalidParameterException {
        if (!existeLoja(x.getLoja())) throw new InvalidParameterException("Loja inválida.");
        else {
            String email = getUserEmail(x.getUser());

            if (email == null) throw new InvalidParameterException("Utilizador inválido.");
            else try {
                this.lojas.get(x.getLoja()).request(x.getEnc());
                this.users.get(email).inc_Encomendas();
                this.pedidos_encomenda.put(x.getEnc(), x.clone());
            } catch (NullPointerException e) {
                e.printStackTrace(); //debug
            }
        }
    }

    /**
     * devolve o codigo do agente responsavel pela encomenda de um utilizador
     * @param user_code codigo do utilizador que fez a encomenda
     */

    public String getAgente_Encomenda(String user_code) {
        for(Agente a:this.agentes.values()) {
            if (a.has_order_in_name(user_code)) return a.getCodigo();
        }
        return null;
    }

    /**
     * Remove uma encomenda que chegou ao cliente das encomendas em processo de entrega
     */

    public void remove_encomenda(String user_email, LocalDateTime hora_chegada) {
        Utilizador usr = this.users.get(user_email);
        usr.receive_Enc();
        String user = usr.getCod();

        String agent = getAgente_Encomenda(user);
        if (agent!=null){
            this.agentes.get(agent).remove_encomenda(user,hora_chegada);
        }
    }

    /**
     * Valida as credenciais de um utilizador
     *
     * @param email    Email do Utilizador
     * @param password Password do Utilizador
     * @return True se as credenciais são Válidas, False caso contrário
     */
    public boolean loginUser(String email, String password) throws InvalidParameterException {

        if (this.users.containsKey(email)) {
            Utilizador aux = this.users.get(email);

            if (aux.getPassword().equals(password)) {

                welcomeName(aux.getNome());
                return true;
            } else {
                throw new InvalidParameterException("Password errada");
            }
        } else {
            throw new InvalidParameterException("Email inexistente.");
        }
    }

    /**
     * Valida as credenciais de um agente
     *
     * @param cod código de identificação do agente
     * @return True se as credenciais são Válidas, False caso contrário
     */
    public boolean loginAgent(String cod) throws InvalidParameterException {

        if (existeVol(cod)) {
            welcomeName(this.agentes.get(cod).getNome());
            return true;
        } else if (existeEmp(cod)) {
            View.welcome(this.agentes.get(cod).getNome());
            return true;
        } else {
            throw new InvalidParameterException("Agente inválido.");
        }
    }

    /**
     * Mostra a classificaçao de um dado agente
     *
     * @param cod String do codigo do agente
     */
    public void verClassif(String cod) {
        View.showClassif(this.agentes.get(cod).getClassificacao());
    }

    public boolean getDispAgente(String cod) {
        return this.agentes.get(cod).getDisponibilidade();
    }

    public void setDispAgente(String cod, boolean d) {
        this.agentes.get(cod).setDisponibilidade(d);
    }

    public void historicoEncs(String cod, String mail) {

        Map<Encomenda, StatsEntrega> reg = this.agentes.get(cod).getRegisto_encomendas();
        boolean t = false;
        String user = null;

        if (reg.isEmpty()) View.show(Nada);
        else {
            if (mail != null) user = this.users.get(mail).getCod();

            for (Map.Entry<Encomenda, StatsEntrega> e : reg.entrySet()) { //needs testing

                if (mail != null) t = e.getKey().getUser().equals(user);

                if (mail == null || t) {
                    View.showEnc(e.getKey().getEnc());
                    View.simpleshow(e.getValue().toString());
                }
            }
        }
    }

    /**
     * Dados relativos às encomendas prontas a entregar em cada loja
     */
    public void encomendasProntas() {

        List<String> list;
        boolean stuff = false;

        for (Loja l : this.lojas.values()) {
            list = l.readyEncs();

            if (!list.isEmpty()) {
                View.showLoja(l.getNome(), list);
                stuff = true;
            }
        }
        if (!stuff) View.show(Nada);
    }

    /**
     * Indica quantos utilizadores utilizam o Sistema
     *
     * @return Nº de utilizadores do Sistema
     */
    public int quantosUtilizadores() {
        return this.users.size();
    }

    /**
     * Atribui uma classificação de 0 a 10 a um dado agente
     *
     * @param rating     Classificação a atribuir ao agente
     * @param cod_agente Codigo do agente
     * @throws InvalidParameterException Se a classificação dada for invalida
     */
    public void rate_agent(double rating, String cod_agente) throws InvalidParameterException {
        if (rating > 10 || rating < 0) throw new InvalidParameterException(rating + " não é uma classificação válida.");
        try {
            this.agentes.get(cod_agente).rate(rating);
        } catch (NullPointerException e) {
            throw new InvalidParameterException("Agente inválido.");
        }

    }

    /**
     * Retorna a faturação total de uma dada empresa num certo período de tempo
     *
     * @param cod_emp Código da empresa a considerar
     * @param inicio  Data inicial
     * @param fim     Data final
     */
    public void fatemp(String cod_emp, LocalDateTime inicio, LocalDateTime fim) {
        try {
            double res = ((EmpresaTransportadora) this.agentes.get(cod_emp)).total_faturado(inicio, fim);
            View.showFat(res);
        } catch (NullPointerException e) {
            throw new InvalidParameterException("Agente inválido.");
        }
    }

    /**
     * Devolve os nomes de X Utilizadors por ordem decrescente do nº de transações
     *
     * @param x Nº de utilizadores a listar
     */
    public void TopXUsers(int x) {
        TreeSet<Utilizador> aux = new TreeSet<>(new UserComparatorByOrders());

        for (Utilizador u : this.users.values()) {
            aux.add(u.clone());
        }

        List<String> l = aux.stream().map(Utilizador::getNome).limit(x).collect(Collectors.toList());
        View.showList(l);
    }

    /**
     * Devolve os nomes de X Agentes por ordem decrescente do Nº de Kilometros percorridos
     *
     * @param x Nº de Agentes a listar
     */
    public void TopXEmpresas(int x) {
        TreeSet<EmpresaTransportadora> aux = new TreeSet<>(new EmpresasComparatorByDistance());

        this.agentes.values()
                .stream()
                .filter(a -> a instanceof EmpresaTransportadora)
                .forEach(s -> aux.add(((EmpresaTransportadora) s).clone()));

        List<String> l = aux.stream().map(Agente::getNome).limit(x).collect(Collectors.toList());
        View.showList(l);
    }

}
