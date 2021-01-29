import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;

public interface TrazAquiModelI {

    /**
     * Metodos gets e sets,
     * clone, equlas e toString
     */
    Map<String, UtilizadorI> getUtilizadores();
    void setUtilizadores(Map<String, UtilizadorI> utilizadores);
    Map<String, VoluntarioI> getVoluntarios();
    void setVoluntarios(Map<String, VoluntarioI> voluntarios);
    Map<String, Empresa_TransportadoraI> getEmpresas();
    void setEmpresas(Map<String,Empresa_TransportadoraI> empresas);
    Map<String, LojaI> getLojas();
    void setLojas(Map<String, LojaI> lojas);
    Map<String, EncomendaI> getEncomendas();
    void setEncomendas(Map<String, EncomendaI> encomendas);
    Set<String> getEnc_aceites();
    void setEnc_aceites(Set<String> enc_aceites);
    Map<String, LoginI> getLogins();
    void setLogins(Map<String, LoginI> lgs);
    TrazAquiModel clone();
    boolean equals(Object o);

    // -------------------------------Metodos de leitura e escrita----------------------------------------------------------

    /**
     * Metodo que le os logs que estao por linhas num array de strings
     * e cria os dados da aplicacao com todos os utilizadores, voluntarios
     * transportadoras, lojas, logins e encomendas aceites.
     */
    String[] lerS() throws IOException;
    // Metodo caso a funçao de escrever e ler em binario nao funcione
    void lerTA(String[] logs);

    /**
     * Metodo que grava em ficheiro de objetos(binario) o estado total da turma.
     */
    void guardaEstado() throws IOException;
    /**
     * Metodo que recupera uma instancia de turma gravada em ficheiro de objetos.
     */
    TrazAquiModel carregaEstado() throws IOException, ClassNotFoundException;
    // -------------------------------Login-------------------------------------------------------
    /**
     * Metodo que valida o login
     * Caso login n seja valido imprimir uma mensagem a dizer login invalido
     */
    boolean loginValido(String codigo, String password);

    // -------------------------------Metodos relativos ao Utilizador-------------------------------------------------------
    /**
     * Metodos que adiciona um utilizador.
     */
    // Cria um codigo de utilizador novo
    String criaUtilizador();

    /**
     * Metodo que adiciona e regista um utilizador
     */
    void addUtilizador(String code, String nome, String password, double[] coordenadas);

    /**
     * Metodo que remove um utilizador
     */
    void removeUtilizador(String codUtilizador);

    /**
     * Metodo que cria a encomenda solicitada pelo utilizador
     * @param list Recebe uma lista de strings ja com os valores direitos
     */
    String solicitaEncomenda(String codUt, String codLoja, List<String> list);

    /**
     * Metodo que consulta a informacao de um transportador
     */
    String consultaInformacao(String codUtilizador, String codTr, LocalDateTime inicio, LocalDateTime fim);

    // -------------------------------Metodos relativos ao Voluntário-------------------------------------------------------
    /**
     * Metodo que cria codigo de voluntario
     */
    String criaVoluntario();

    /**
     * Metodo que cria um novo voluntario e regista
     */
    void newVoluntarioMedico(String code, String nome, String password, double[] coordenadas, char medico, double raio);

    /**
     * Metodo que remove um voluntario
     */
    void removeVoluntario(String codVoluntario);

    /**
     *  Metodo que determina a listagens dos 10 voluntarios
     *  que mais utilizam o sistema (em número de encomendas transportadas);
     */
    List<String> voluntariosMaisUtilizam();

    /**
     * Metodo que determina o voluntario que vai buscar a encomenda
     * Devolve null caso nenhum voluntario possa ir buscar
     * @return "" caso nao haja nenhum voluntario
     */
    String qualVoluntario(EncomendaI e);

    /**
     * Metodo que um voluntario vai buscar uma encomenda
     */
    LocalDateTime vaiBuscarEncVl(String codVl, String codEnc);

    /**
     * Metodo que entrega uma encomenda para voluntario
     */
    void entregaEncVl(String vl, String codEnc, LocalDateTime inicio, int fila);

    /**
     * Metodo que retorna uma string com o codigo e o nome do voluntario
     */
    String codeNomeVl();

    /**
     * Metodo que diz se é voluntario medico
     */
    boolean eMedicoVl(String codVl);

    /**
     * Metodo disponibiliza voluntario medico
     */
    void disponivelMedicoVl(boolean status, String codVl);

    /**
     * Atualizar classificacao e depois tornar vl disponivel outra vez
     */
    void atualizavl(String codVl, double cl);

    /**
     * Atualiza disponibilidade para um Voluntario
     */
    void disponibilidadeVl(String codVl, boolean b);

    // -------------------------------Metodos relativos a Encomendas-------------------------------------------------------
    /**
     * Gerar encomendas
     */
    String geraEnc(String codTransportador, boolean medico);

    /**
     * Metodo que remove uma encomenda
     */
    void removeEncomenda(String enc);

    // -------------------------------Metodos relativos a Empresas-------------------------------------------------------
    /**
     * Metodo que cria o codigo de uma transportadora
     */
    String criaEmpresa_Transportadora();

    /**
     * Metodo que cria uma nova empresa
     */
    void newEmpresa_Transportadora(String code, String nome, String password, double[] coordenadas,
                               char b, String codET, int nif, double r, double taxa, int lotacao);

    /**
     * Metodo que remove uma empresa transportadora
     */
    void removeEmpresas(String codEmpresa);

    /**
     * Metodo que determina a listagens dos 10 empresas transportadoras que mais utilizam
     * o sistema (em número de kms percorridos)
     */
    List<String> transportadorasMaisUtilizam();

    /**
     * Metodo que determina a empresa transportadora que vai buscar a encomenda
     * Devolve null quando nenhuma das empresas pode ir buscar
     */
    String qualTransportadora(EncomendaI encomenda, List<String> codTr);

    /**
     * Metodo que retorna uma string com o nome e o codigo de todas as transportadoras
     */
    String codeNomeTr();

    /**
     * Calcula o preco do transporte
     */
    double precoTr(String codTr, String codEnc);

    /**
     * metodo que vai buscar uma encomenda para transportadora
     */
    LocalDateTime vaiBuscarEncTr(String codTr, String codEnc);

    /**
     * Metodo que entrega encomenda para transportadora
     */
    void entregaEncTr(String tr, String codEnc, LocalDateTime inicio, int fila);

    /**
     * Atualiza Transportadora
     */
    void atualizaTr(String codTr, double cl);

    /**
     * Metodo que calcula a distancia a uma loja
     */
    double distLojaEt(String codTr, String codLoja);

    /**
     * Loja mais perto
     */
    String lojaMaisPerto(String codTr, List<String> codEncs);

    /**
     * Metodo que calcula distancias entre lojas
     */
    double distEntreLojas(String codL1, String codL2);

    /**
     * Metodo que ordena as lojas por distancia
     */
    List<String> ordenaLojas(String codL1, List<String> codLojas);

    /**
     * Metodo que calcula distancia entre loja e ut mais perto
     */
    String utMaisPerto(String codLoja, List<String> uts);

    /**
     * Metodo que ordena as Utilizadores por distancia
     */
    List<String> ordenaUts(String codUt1, List<String> codUts);

    /**
     * Metodo que vai entrega uma encomenda para tr de varias encomendas
     */
    void entregaEncTrEncs(String codTr, String codEnc, double kms, LocalDateTime inicio, int nFilaLojas);

    /**
     * Metodo que diz se é Empresa medico
     */
    boolean eMedicoTr(String codTr);

    /**
     * Metodo que torna disponivel uma empresa medica
     */
    void disponivelMedicoTr(boolean status, String codTr);

    /**
     * Atualiza disponibilidade para uma transportadora
     */
    void disponibilidadeTr(String codTr, boolean b);

    /**
     * Distancia entre do atual ao ultimo visitado
     */
    double distUlt(String ult, String at);

    /**
     * Metodo que atualiza kms de uma transportadora
     */
    void atualizaKms(String codTr, double kms);

    /**
     * Devolve a distancia total percorrida
     */
    double distTotal(String codTr, String codEnc);
    // -------------------------------Metodos relativos a Lojas-------------------------------------------------------
    /**
     * Metodo que cria o codigo de uma loja
     */
    String criaLoja();

    /**
     * Metodo que cria uma nova loja
     */
    void newLoja(String code, String nome, String password, double[] coordenadas);

    /**
     * Metodo que remove uma loja
     */
    void removeLoja(String codLoja);

    /**
     * Metodo que adiciona o peso de uma encomenda
     */
    void adicionaPesoEnc(String codLoja, String codEnc, double peso);

    /**
     * Metodo que devolve uma string com o codigo e o nome de todas as lojas
     */
    String codeNomeLojas();

    /**
     * Metodo que gera encomendas para as lojas
     */
    String encLoja(String codLoja);

    // ------------------ decisao entre voluntario e transportador ------------------------

    /**
     * Metodo que decide entre voluntario e transportadora (qual deles vai buscar a encomenda)
     * criterio, e o que esta mais perto
     */
    String vlOuTr(String codTr, String codVl, String codLj);



}
