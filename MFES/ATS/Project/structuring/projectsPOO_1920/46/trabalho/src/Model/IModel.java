package Model;
import java.io.*;
import java.util.ArrayList;
import java.util.*;

/**
 * Interface do Model
 * */
public interface IModel {

    /**
     * Guarda o estado do model ou seja de todas as classes de entidades num ficheiro usando o ObjectOutputStream
     * @throws IOException erro
     */
    void guardaEstado () throws IOException;

    /**
     *
     * Carrega o estado do model ou seja de todas as classes de entidades num ficheiro usando o ObjectInputStream
     * @throws IOException erro
     * @throws ClassNotFoundException erro
     * @return Estrutura Model
     */
    Model loadEstado () throws IOException, ClassNotFoundException;

    /**
     * Função que realiza certas operações para o registo de dados
     * @return o numero para acrescentar no cod da encomenda para q nunca se repitam
     */
    int contaNCodEnc ();

    /**
     * Função que realiza certas operações para o registo de dados
     * @return o numero para acrescentar no cod da encomenda para q nunca se repitam
     */
    int contaNCodProd ();

    /**
     * Função que realiza certas operações para o registo de dados
     * @return o numero para acrescentar no cod do produto para q nunca se repitam
     */
    int contaNCodUser ();

    /**
     * Função que realiza certas operações para o registo de dados
     * @return o numero para acrescentar no cod da transportadora para q nunca se repitam
     */
    int contaNCodTrans ();

    /**
     * Função que realiza certas operações para o registo de dados
     * @return o numero para acrescentar no cod da loja para q nunca se repitam
     */
    int contaNCodLoja ();

    /**
     * Função que realiza certas operações para o registo de dados
     * @return o numero para acrescentar no cod do voluntario para q nunca se repitam
     */
    int contaNCodVol ();

    /**
     * @return Mapa das Transportadoras
     * */
    HashMap<String, ITransportadora> getTransMap();

    /**
     * @return Mapa das Lojas
     * */
    HashMap<String, ILoja> getLojaMap();

    /**
     * @return Mapa dos utilizadores
     * */
    TreeMap<String, IUtilizador> getUserMap();

    /**
     * @return Mapa dos Voluntarios
     * */
    HashMap<String, IVoluntario> getVolMap();

    /**
     * @return Mapa de encomendas com o seu ID como key
     */
    HashMap<String, IEncomenda> getEncMap();

    /**
     * Função que regista uma encomenda na estrutura
     * @param id id da encomenda
     * @param userId id do comprador
     * @param lojaId id da loja
     * @param peso peso dos produtos
     * @param produtos produtos
     * */
    void registaEncomenda(String id, String userId, String lojaId, double peso, ArrayList<LinhaEncomenda> produtos);

    /**
     * Adiciona uma encomenda à estrutura
     * @param e encomenda a adicionar
     * */
    void addEncomenda(IEncomenda e);

    /**
     * Adiciona um voluntário à estrutura
     * @param v voluntário a adicionar
     * */
    void addVoluntario(IVoluntario v);

    /**
     * Adiciona um utilizador à estrutura
     * @param u utilizador a adicionar
     * */
    void addUtilizador(IUtilizador u);

    /**
     * Adiciona uma loja à estrutura
     * @param l loja a adicionar
     * */
    void addLoja(ILoja l);

    /**
     * Adiciona uma transportadora à estrutura
     * @param t transportadora a adicionar
     * */
    void addTransportadora(ITransportadora t);

    /**
     *
     * */
    void removeEncomenda(String id);

    /**
     *
     * */
    List<String> precisa_recolha(ILoja l);

    /**
     *
     * */
    ILoja loja_nome(String nome);

    /**
     *
     * */
    IEncomenda encomendas_u(IUtilizador u);

    /**
     *
     * */
    boolean validaLogInUser(String email, String pwd);

    /**
     *
     * */
    boolean validaLogInVol(String email, String pwd);

    /**
     *
     * */
    boolean validaLogInTrans(String email, String pwd);

    /**
     *
     * */
    boolean validaLogInLoja(String email, String pwd);

    /**
     *
     * */
    IUtilizador getUser(String email);

    /**
     *
     * */
    IVoluntario getVol(String email);

    /**
     *
     * */
    ITransportadora getTrans(String email);

    /**
     *
     * */
    ILoja getLoja(String email);

    /**
     * Função que
     * */
    ILoja loja(String id);

    /**
     * Função que
     * */
    ITransportadora transportadora(String id);

    /**
     * Função que
     * */
    IUtilizador user(String id);

    /**
     *
     * */
    IVoluntario voluntario(String id);

    /**
     *
     * */
    IEncomenda encomenda(String value);

    /**
     *
     * */
    boolean validaRegistoUser(String email);

    /**
     *
     * */
    boolean validaRegistoVol(String email);

    /**
     *
     * */
    boolean validaRegistoTrans(String email);

    /**
     *
     * */
    boolean validaRegistoLoja(String email);

    /**
     *
     * */
    List<String> top10Acessos();

    /**
     *
     * */
    List<String> top10Distancias();

    /**
     *
     * */
    List<String> encomendas_ativas();

    /**
     *
     * */
    List<String> lojas();

    /**
     * Metodo provavelmente temporario que adiciona as transporadoras existentes fazendo load de um ficheiro txt
     *  @throws IOException erro
     */
    void fileToTrans() throws IOException;

    /**
     * Metodo provavelmente temporario que adiciona as lojas existentes fazendo load de um ficheiro txt
     * @throws IOException erro
     */
    void filetoLoja() throws IOException;

    /**
     * Metodo provavelmente temporario que adiciona os Utilizadores existentes fazendo load de um ficheiro txt
     * @throws IOException erro
     */
    void fileToUser() throws IOException;

    /**
     * Metodo provavelmente temporario que adiciona os Voluntarios existentes fazendo load de um ficheiro txt
     * @throws IOException erro
     */
    void fileToVol() throws IOException;

    /**
     * Metodo provavelmente temporario que adiciona as Encomendas existentes fazendo load de um ficheiro txt
     * @throws IOException erro
     */
    void fileToEnc() throws IOException;

    /**
     * Metodo provavelmente temporario que adiciona um inventario as lojas existentes fazendo load de um ficheiro txt
     * @throws IOException erro
     */
    void loadInventLoja() throws IOException;
}