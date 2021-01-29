import java.time.LocalDateTime;
import java.util.List;

public interface TrazAquiViewI {
    String inputNome();

    String inputPassword();

    double[] lerCoordenadas();

    String inputCode();

    double inputRaioAcao();

    char inputMedico();

    int inputNif();

    double inputTaxa();

    int inputLotacao();

    void imprime(String s);

    char disponivelNovamente();

    char disponivelParaMedico();

    void aindaNaoDisponivel();

    void erroIO();

    //-------------------- Utilizador --------------------
    void criaUtilizador(String cod);

    /**
    * Metodos view relativos ao pedido de uma encomenda nova
    */
    // Codigo da loja
    String qualCodLoja(String lojas);

    // Linha da encomenda
    String codProduto();

    String descricaoProduto();

    int quantidadeProduto();

    char adicionarMaisProdutos();

    void semTransportadores();

    /**
    * Metodos de registos de Transportes
    */
    String codTransportadores(String transportadores);

    LocalDateTime qualDataInicio();

    LocalDateTime qualDataFim();

    /**
    * Mensagem de encomenda em inicio de transporte
    */
    void inicioTransporte(String nome, String tipo);

    char aceitaPreco(String preco);

    double classificaTransporte();

    //-------------------- Voluntário --------------------
    void criaVoluntario(String cod);

    void voluntariosMaisUtilizam(List<String> top10);

    void estadoVoluntario();

    //-------------------- Empresa --------------------
    void criaEmpresa(String cod);

    void transportadorasMaisUtilizam(List<String> top10);

    void totalFaturado(double n);

    void encEntregue(String enc);

    //-------------------- Loja --------------------
    void criaLoja(String cod);

    double pesoEnc();

    void printNFilaEspera(int n);

    void printNovaEnc();

    void encRecolhida(String codEnc);

    //-------------------- Login --------------------
    void login();

    void erroLogin();

    char registo();

    char login_registo();
    //---------------- Encomendas -------------------------
    char pretendeTransportar(String enc);

    void erroEnc();

    void bemVindo(String s);

    //---------------- Menu Utilizador ---------------------
    int menuUtilizador();
    //------------------------------ Menu Voluntario ------------------------------
    int menuVoluntario();
    //------------------------------ Menu Transportadora ------------------------------
    int menuTransportadora();
    //------------------------------ Menu Loja ------------------------------
    int menuLoja();
}
