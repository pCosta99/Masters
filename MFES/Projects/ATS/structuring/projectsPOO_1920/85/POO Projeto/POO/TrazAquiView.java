import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

import static java.lang.Double.MAX_EXPONENT;
import static java.lang.Double.MAX_VALUE;
import static java.time.LocalDateTime.MAX;

public class TrazAquiView implements TrazAquiViewI{
    //------------------------------ inputs --------------------------------------
    private String lerLinha(){
        Scanner sc = new Scanner(System.in);
        return sc.nextLine();
    }

    private String lerCodigo(){
        Scanner sc = new Scanner(System.in);
        return sc.next();
    }

    private char lerChar(){
        Scanner sc = new Scanner(System.in);
        return Character.toLowerCase(sc.next().charAt(0));
    }

    private int lerInt(){
        Scanner sc = new Scanner(System.in);
        return sc.nextInt();
    }

    private int tryLerInt(){
        int n;
        try {
            n = lerInt();
        } catch (InputMismatchException e) {
            erroIO();
            n = MAX_EXPONENT;
            }
        return n;
    }

    private double lerDouble() {
        Scanner sc = new Scanner(System.in);
        return sc.nextDouble();
    }

    private double tryLerDouble(){
        double d;
        try {
            d = lerDouble();
        } catch (InputMismatchException e) {
            d = MAX_VALUE;
            erroIO();
        }
        return d;
    }

    private LocalDateTime lerData(){
        Scanner sc = new Scanner(System.in);
        String data = sc.next() + "T00:00:00";
        return LocalDateTime.parse(data);
    }

    private LocalDateTime tryLerData(){
        LocalDateTime lt;
        try {
            lt = lerData();

        } catch (DateTimeParseException e) {
            erroIO();
            lt = MAX;
        }
        return lt;
    }

    private double[] lerCoordenadasAux(){
        double[] gps;
        gps = new double[2];

        double y = tryParseDouble("lat");
        double x = tryParseDouble("lon");
        gps[0] = x;
        gps[1] = y;

        return gps; // Retorna as coordenadas gps
    }

    private double tryParseDouble(String c){
        Scanner sc = new Scanner(System.in);
        double d;
        do {
            try {
                if(c.equals("lat")){
                    System.out.print("Latitude: ");
                }
                else System.out.print("Longitude: ");
                d = Double.parseDouble(sc.next());
            } catch (NumberFormatException e) {
                erroIO();
                d = MAX_VALUE;
            }
        }while (d == MAX_VALUE);
        return d;
    }

    //-------------------- Outputs --------------------
    public String inputNome() {
        System.out.print("Nome: ");
        return lerLinha();
    }

    public String inputPassword() {
        System.out.print("Password: ");
        return lerLinha();
    }

    public double[] lerCoordenadas() {
        System.out.println("Coordenadas gps");
        return lerCoordenadasAux();
    }

    public String inputCode() {
        System.out.print("Code: ");
        return lerCodigo();
    }

    public double inputRaioAcao() {
        double r;
        do {
            System.out.print("Raio de ação: ");
            r = tryLerDouble();
        } while (r == MAX_VALUE);
        return r;
    }

    public char inputMedico() {
        System.out.println("Tem certificado para entrega de medicamentos?");
        System.out.print("Para sim precione [S], caso contrario [N]: ");
        return lerChar();
    }

    public int inputNif() {
        int nif;
        do {
            System.out.println("Nif: ");
            nif = tryLerInt();
        } while (nif == MAX_EXPONENT);
        return nif;
    }

    public double inputTaxa(){
        double t;
        do {
            System.out.print("Taxa: ");
            t = tryLerInt();
        } while (t == MAX_EXPONENT);
        return t;
    }

    public int inputLotacao() {
        int l;
        do {
            System.out.println("Lotacao de encomendas: ");
            l = tryLerInt();
        } while (l == MAX_EXPONENT);
        return l;
    }

    public void imprime(String s){
        System.out.println(s);
    }

    public char disponivelNovamente(){
        System.out.println("Pretende ficar disponivel novamente?");
        System.out.print("[S] para sim, [N] caso nao: ");
        return lerChar();
    }

    public char disponivelParaMedico(){
        System.out.println("Pretende ficar disponivel tambem para transporte de medicamentos?");
        System.out.print("[S] para sim, [N] caso nao: ");
        return lerChar();
    }

    public void aindaNaoDisponivel(){
        System.out.println("Primeiro tem de se mostrar disponivel utilizando '1' no menu");
    }

    public void erroIO(){
        System.out.println("Valor mal introduzido, tente outra vez");
    }

    //-------------------- Utilizador --------------------
    public void criaUtilizador(String cod) {
        System.out.println("Registar como utilizador");
        System.out.println("Codigo de utilizador: " + cod);
    }

    /**
     * Metodos view relativos ao pedido de uma encomenda nova
     */
    // Codigo da loja
    public String qualCodLoja(String lojas){
        System.out.println("Solicitar encomenda");
        System.out.println(lojas);
        System.out.print("Codigo da Loja: ");
        return lerCodigo();
    }

    // Linha da encomenda
    public String codProduto(){
        System.out.print("Codigo do produto: ");
        return lerCodigo();
    }

    public String descricaoProduto(){
        System.out.print("Descricao do produto: ");
        return lerLinha();
    }

    public int quantidadeProduto(){
        int n;
        do {
            System.out.print("Quantidade: ");
            n = tryLerInt();
        } while (n == MAX_EXPONENT);
        return n;
    }

    public char adicionarMaisProdutos(){ // Retorna um s ou um n minusculo
        System.out.print("Deseja adicionar mais produtos? [S] para sim [N] para nao: ");
        return lerChar();
    }

    public void semTransportadores(){
        System.out.println("De momento nao temos transportadores disponiveis, pedimos desculpa pelo incomodo");
    }

    /**
     * Metodos de registos de Transportes
     */
    public String codTransportadores(String transportadores){
        imprime(transportadores);
        System.out.println("Codigo do transportador a consultar: ");
        return lerCodigo();
    }

    public LocalDateTime qualDataInicio(){
        LocalDateTime lt;
        do{
            System.out.print("Data de inicio que pretende ver (no formato 2020-01-01): ");
            lt = tryLerData();
        }while (lt == MAX);

        return lt;
    }

    public LocalDateTime qualDataFim(){
        LocalDateTime lt;
        do{
            System.out.print("Data de fim que pretende ver (no formato 2020-01-01): ");
            lt = tryLerData();
        }while (lt == MAX);

        return lt;
    }

    /**
     * Mensagem de encomenda em inicio de transporte
     */
    public void inicioTransporte(String nome, String tipo){
        System.out.println(tipo + nome + " vai buscar a sua encomenda.");
    }

    public char aceitaPreco(String preco){
        System.out.println("Aceita o preco do transporte " + preco + "?");
        System.out.println("[S] caso sim, [N] caso contrario: ");
        return lerChar();
    }

    public double classificaTransporte(){
        double d;
        do {
            System.out.print("Classificar transporte (0 a 10): ");
            d = tryLerDouble();
        } while (d == MAX_VALUE);
        return d;
    }

    //-------------------- Voluntário --------------------
    public void criaVoluntario(String cod) {
        System.out.println("Registar como voluntario");
        System.out.println("Codigo associado ao voluntarios: " + cod);
    }

    public void voluntariosMaisUtilizam(List<String> top10){
        System.out.println("Voluntarios mais utilizam a aplicacao");
        for(String s: top10)
            imprime(s);
    }

    public void estadoVoluntario(){
        System.out.println("Esta agora disponivel para entregas.\n");
    }

    //-------------------- Empresa --------------------
    public void criaEmpresa(String cod) {
        System.out.println("Registar como empresa transportadora");
        System.out.println("Codigo associado: " + cod);
    }

    public void transportadorasMaisUtilizam(List<String> top10){
        System.out.println("Transportoadoras mais utilizam a aplicacao");
        for(String s: top10)
            imprime(s);
    }

    public void totalFaturado(double n){
        System.out.println("O total faturado pela sua empresa foi: " + n);
    }

    public void encEntregue(String enc){
        System.out.println("A encomenda " + enc + " foi entregue");
    }

    //-------------------- Loja --------------------
    public void criaLoja(String cod) {
        System.out.println("Codigo de Loja: " + cod);
    }

    public double pesoEnc(){
        double p;
        do {
            System.out.print("Qual o peso da encomenda: ");
            p = tryLerDouble();
        } while (p == MAX_VALUE);
        return p;
    }

    public void printNFilaEspera(int n){
        System.out.println("Está " + n + " pessoa em fila de espera");
    }

    public void printNovaEnc(){
        System.out.println("Nova encomenda");
    }

    public void encRecolhida(String codEnc){
        System.out.println("A encomenda " + codEnc + " foi recolhida");
    }

    //-------------------- Login --------------------
    public void login(){
        System.out.println("Login");
    }
    public void erroLogin(){
        System.out.println("Codigo ou passward invalidos, tente algo diferente");
    }

    private void registoAux(){
        System.out.println("Registo no Traz Aqui");
        System.out.println("Selecione a utilidade do registo");
        System.out.println("Utilizador - u");
        System.out.println("Voluntario - v");
        System.out.println("Transportadora - t");
        System.out.println("Loja - l");
        System.out.println("Sair - q");
    }
    public char registo(){
        char c;
        registoAux();
        c = lerChar();
        while (c != 'u' && c != 'v' && c != 't' && c != 'l' && c != 'q'){
            erro_registo();
            registoAux();
            c = lerChar();
        }
        return c;
    }


    private void erro_registo(){
        System.out.println("Registo invalido tente outra vez!!");
    }

    private void login_registoAux(){
        System.out.println("Login - l");
        System.out.println("Registar - r");
        System.out.println("Sair - q");
    }

    public char login_registo(){
        login_registoAux();
        char c;
        c = lerChar();
        while (c != 'l' && c != 'r' && c != 'q'){
            System.out.println("Carater mal introduzido! Tente outra vez");
            login_registoAux();
            c = lerChar();
        }
        return c;
    }
    //---------------- Encomendas -------------------------
    public char pretendeTransportar(String enc){
        System.out.println("Pretende transportar a seguinte encomenda?");
        System.out.println(enc);
        System.out.print("[S] para sim [N] para nao o fazer: ");
        return lerChar();
    }

    public void erroEnc(){
        System.out.println("De momento nao ha encomendas disponiveis");
    }

    public void bemVindo(String s){
        System.out.println("Olá " + s);
    }

    //---------------- Menu Utilizador ---------------------
    public int menuUtilizador(){
        int n;
        do {
        System.out.println("Pedido de encomenda - 1");
        System.out.println("Consultar informaçoes de transportadores- 2");
        System.out.println("Voluntarios que mais utilizam aplicacao - 3");
        System.out.println("Transportadoras que mais utilizam aplicacao - 4");
        System.out.println("Remover a conta - 5");
        System.out.println("Sair - 0");
            n = tryLerInt();
        } while (n < 0 || n > 5);
        return n;
    }
//------------------------------ Menu Voluntario ------------------------------
    public int menuVoluntario(){
        int n;
        do {
            System.out.println("Sinalizar que esta disponivel - 1");
            System.out.println("Ver encomendas das lojas e decidir qual ir buscar - 2");
            System.out.println("Voluntarios que mais utilizam aplicacao - 3");
            System.out.println("Transportadoras que mais utilizam aplicacao - 4");
            System.out.println("Remover a conta - 5");
            System.out.println("Sair - 0");
            n = tryLerInt();
        } while (n < 0 || n > 5);
        return n;
    }
//------------------------------ Menu Transportadora ------------------------------
    public int menuTransportadora() {
        int n;
        do {
            System.out.println("Sinalizar que esta disponivel - 1");
            System.out.println("Saber quanto fatorou em vendas num determinado periodo - 2");
            System.out.println("Encomendas disponiveis - 3");
            System.out.println("Voluntarios que mais utilizam aplicacao - 4");
            System.out.println("Transportadoras que mais utilizam aplicacao - 5");
            System.out.println("Remover a conta - 6");
            System.out.println("Sair - 0");
            n = tryLerInt();
        } while (n < 0 || n > 6);
        return n;
    }
//------------------------------ Menu Loja ------------------------------
    public int menuLoja(){
        int n;
        do {
            System.out.println("Sinalizar que existe uma encomenda - 1");
            System.out.println("Pessoas em fila de espera - 2");
            System.out.println("Voluntarios que mais utilizam aplicacao - 3");
            System.out.println("Transportadoras que mais utilizam aplicacao - 4");
            System.out.println("Remover a conta - 5");
            System.out.println("Sair - 0");
            n = tryLerInt();
        }while (n < 0 || n > 5);
        return n;
    }
}
