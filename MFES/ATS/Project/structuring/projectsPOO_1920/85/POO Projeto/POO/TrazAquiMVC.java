import java.io.IOException;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class TrazAquiMVC implements TrazAquiMVCI{
    TrazAquiModelI model;
    TrazAquiViewI view;

    public TrazAquiMVC(){
        this.model = new TrazAquiModel();
        this.view = new TrazAquiView();
    }

    //------------------------------ Ler e Escrever ------------------------------
    // Esta funçao é usada caso a função de ler em binario nao funcione
    public void read_TA() {
        try {
            this.model.lerTA(this.model.lerS().clone());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Funçao que carrega o estado guardado em binario
     */
    public void carregaEstado() throws IOException, ClassNotFoundException {
        this.model = new TrazAquiModel(this.model.carregaEstado());
    }

    /**
     * Funçao que guarda o estado guardado em binario
     */
    public void guardaEstado() throws IOException {
        this.model.guardaEstado();
    }

//------------------------------ Utilizador ------------------------------
    /**
     * Adicionar um utilizador, faz IOs com o utilizador
     */
    private void addUser() {
        String code = this.model.criaUtilizador();
        this.view.criaUtilizador(code);

        String nome = this.view.inputNome();

        String password = this.view.inputPassword();

        double[] coordenadas = this.view.lerCoordenadas();

        this.model.addUtilizador(code, nome, password, coordenadas);
    }

    /**
     * Solicitar encomenda
     */
    // String com todos os produtos encomendados
    private String prodEnc(String codUtilizador) {
        List<String> lista = new ArrayList<>();
        String codLoja;
        do {
            String lojas = this.model.codeNomeLojas();
            codLoja = this.view.qualCodLoja(lojas);
        } while (!this.model.getLojas().containsKey(codLoja));
        char c;
        do {
            String codProduto = this.view.codProduto();
            lista.add(codProduto);

            String descricao = this.view.descricaoProduto();
            lista.add(descricao);

            double quantidadeProduto = this.view.quantidadeProduto();
            lista.add(String.valueOf(quantidadeProduto));

            c = this.view.adicionarMaisProdutos();
        } while (c == 's');
        return this.model.solicitaEncomenda(codUtilizador, codLoja, lista);
    }

    // Solicitar encomenda
    private void solicitaEnc(String codUt){
        String codEnc = prodEnc(codUt);
        String codVl = this.model.qualVoluntario(this.model.getEncomendas().get(codEnc));
        String codTr = this.model.qualTransportadora(this.model.getEncomendas().get(codEnc), new ArrayList<>());
        String codLj = this.model.getEncomendas().get(codEnc).getCodLoja();

        if(codVl.equals("") && codTr.equals("")){
            this.view.semTransportadores();
        }else if(codVl.equals("")){
            codTr = aceitaTr(codTr, codEnc);

            if(codTr.equals("")) {
                this.view.semTransportadores();
                this.model.removeEncomenda(codEnc);
            }
            else solicitaEncTr(codTr, codEnc);
        }
        else if(codTr.equals("")){
            solicitaEncVl(codVl, codEnc);
        }
        else {
            String qual = this.model.vlOuTr(codTr, codVl, codLj);

            if (qual.equals("tr")) {
                qualTransporta(codVl,codTr,codEnc,codLj);
            } else {
                solicitaEncVl(codVl, codEnc);
            }
        }
    }
    // Escolha entre Tr e Vl
    private void qualTransporta(String codVl, String codTr, String codEnc, String codLj){
        String qual;
        char s;
        do {
            qual = this.model.vlOuTr(codTr, codVl, codLj);
            List<String> codsTrs = new ArrayList<>();
            double preco = this.model.precoTr(codTr, codEnc);

            s = this.view.aceitaPreco(preco + "");

            codsTrs.add(codTr);
            codTr = this.model.qualTransportadora(this.model.getEncomendas().get(codEnc), codsTrs);
        } while ((qual.equals("tr") && codTr.equals("")) || s == 's');
        if(qual.equals("tr")){
            solicitaEncTr(codTr, codEnc);
        }
        else if(qual.equals("vl"))
            solicitaEncVl(codVl, codEnc);
        else this.view.semTransportadores();
    }

    // Caso nao aceita transportadora
    private String aceitaTr(String codTr, String codEnc){
        List<String> codsTrs = new ArrayList<>();
        double preco = this.model.precoTr(codTr, codEnc);
        char s = this.view.aceitaPreco(preco + "");
        while (s != 's' && !codTr.equals("")){
            codsTrs.add(codTr);
            codTr = this.model.qualTransportadora(this.model.getEncomendas().get(codEnc), codsTrs);
            if(!codTr.equals("")) {
                preco = this.model.precoTr(codTr, codEnc);
                s = this.view.aceitaPreco(preco + "");
            }
        }
        return codTr;
    }

    // Caso o transportador seja uma Transportadora
    private void solicitaEncTr(String codTr, String codEnc){
        String codLoja = this.model.getEncomendas().get(codEnc).getCodLoja();
        int feLoja = this.model.getLojas().get(codLoja).filaEspera();

        this.view.inicioTransporte(this.model.getEmpresas().get(codTr).getNome(), "A Transportadora ");// Transportadora tem de se procurar por outra
        LocalDateTime inicio = this.model.vaiBuscarEncTr(codTr, codEnc);
        inicio = reformulaData(inicio);
        this.model.entregaEncTr(codTr, codEnc, inicio, feLoja);

        double cl = this.view.classificaTransporte();
        this.model.atualizaTr(codTr, cl);

        double kms = this.model.distTotal(codTr, codEnc);
        this.model.atualizaKms(codTr, kms);
    }

    // Caso o transportador seja um voluntario
    private void solicitaEncVl(String codVl, String codEnc){
        String codLoja = this.model.getEncomendas().get(codEnc).getCodLoja();
        int feLoja = this.model.getLojas().get(codLoja).filaEspera();

        this.view.inicioTransporte(this.model.getVoluntarios().get(codVl).getNome(), "O Voluntario ");
        LocalDateTime inicio = this.model.vaiBuscarEncVl(codVl, codEnc);
        inicio = reformulaData(inicio);
        this.model.entregaEncVl(codVl, codEnc, inicio, feLoja);

        this.model.getVoluntarios().get(codVl).disponivel(true);

        double cl = this.view.classificaTransporte();
        this.model.atualizavl(codVl, cl);
    }

    /**
     * Consultar os registos de um transportador
     */
    private void consultaRegistos(String codUtilizador){
        String codTr;
        codTr = this.model.codeNomeVl() + this.model.codeNomeTr();
        codTr = this.view.codTransportadores(codTr);

        LocalDateTime inicio = this.view.qualDataInicio();
        LocalDateTime fim = this.view.qualDataFim();

        String inf = this.model.consultaInformacao(codUtilizador, codTr, inicio, fim);
        this.view.imprime(inf);
    }

//------------------------------ Voluntário ------------------------------
    /**
     * Adicionar um voluntario
     */
    private void addVoluntary() {
        String code = this.model.criaVoluntario();
        this.view.criaVoluntario(code);

        String nome = this.view.inputNome();

        String password = this.view.inputPassword();

        double[] coordenadas = this.view.lerCoordenadas();

        double raio = this.view.inputRaioAcao();

        char medico = this.view.inputMedico();

        this.model.newVoluntarioMedico(code, nome, password, coordenadas, medico, raio);
    }

    /**
     * Voluntarios que mais utilizam aplicacao
     */
    private void voluntariosMaisUtilizam(){
        List<String> top10 = this.model.voluntariosMaisUtilizam();
        this.view.voluntariosMaisUtilizam(top10);
    }

    /**
     * Transportar encomendas
     */
    private void transporteEncomendasVl(String codVl, boolean medico){
        String codEnc;
        codEnc = this.model.geraEnc(codVl, medico);

        if(codEnc.equals(""))
            this.view.erroEnc();
        else {
            char c = this.view.pretendeTransportar(this.model.getEncomendas().get(codEnc).toString());
            LocalDateTime inicio;

            if (c == 's') {
                String codLoja = this.model.getEncomendas().get(codEnc).getCodLoja();
                int feLoja = this.model.getLojas().get(codLoja).filaEspera();
                inicio = this.model.vaiBuscarEncVl(codVl, codEnc);
                this.model.entregaEncVl(codVl, codEnc, inicio, feLoja);
                c = this.view.disponivelNovamente();

                if (c == 's') {
                    this.model.disponibilidadeVl(codVl, true);
                    if(medico)
                        this.model.disponivelMedicoVl(true, codVl);
                }
            }
        }
    }


//------------------------------ Empresa ------------------------------
    /**
     * Adicionar uma empresa
     */
    private void addCompany(){

        String code = this.model.criaEmpresa_Transportadora();
        this.view.criaEmpresa(code);

        String nome = this.view.inputNome();

        String password = this.view.inputPassword();

        double[] coordenadas = this.view.lerCoordenadas();

        double raio = this.view.inputRaioAcao();

        double taxa = this.view.inputTaxa();

        char medico = this.view.inputMedico();

        int nif = this.view.inputNif();

        int lotacao = this.view.inputLotacao();

        this.model.newEmpresa_Transportadora(code, nome, password, coordenadas, medico, code, nif, raio, taxa, lotacao);
    }

    /**
     * Transportadoras que mais utilizam aplicacao
     */
    private void transportadorasMaisUtilizam(){
        List<String> top10 = this.model.transportadorasMaisUtilizam();
        this.view.transportadorasMaisUtilizam(top10);
    }

    /**
     * Transporte de uma transportadora
     */
    private void transporteEncomendasTr(String codTr, boolean medico){
        int lotacao = this.model.getEmpresas().get(codTr).getLotacaoEnc();
        List<String> encs = new ArrayList<>();
        String enc;
        LocalDateTime inicio = null;
        double kms = 0;
        int filas = 0;
        for(int i = 0; i < lotacao; i++){
            enc = this.model.geraEnc(codTr, medico);
            encs.add(enc);
            inicio = this.model.vaiBuscarEncTr(codTr, enc);
        }


        String lojaMaisPerto = this.model.lojaMaisPerto(codTr, encs);
        kms += this.model.distLojaEt(codTr, lojaMaisPerto);
        filas += this.model.getLojas().get(lojaMaisPerto).filaEspera();

        List<String> loja =  encs.stream()                                             // Percorre a lista das encomendas geradas
                             .map(e -> this.model.getEncomendas().get(e).getCodLoja()) // Vai buscar o codigo das lojas
                             .collect(Collectors.toList());                            // Junta os elementos numa lista
        loja = this.model.ordenaLojas(lojaMaisPerto, loja);

        String lojaAnterior = lojaMaisPerto;
        for(String l: loja){
            kms += this.model.distEntreLojas(lojaAnterior, l);
            filas += this.model.getLojas().get(l).filaEspera() + 1;
            lojaAnterior = l;
        }

        List<String> uts = encs.stream()                                                   // Percorre a lista das encomendas geradas
                           .map(e -> this.model.getEncomendas().get(e).getCodUtilizador()) // Vai buscar o codigo dos utilizadores
                           .collect(Collectors.toList());                                  // Junta os elementos numa lista

        String utMaisPerto = this.model.utMaisPerto(loja.get(loja.size()-1), uts);
        uts = this.model.ordenaUts(utMaisPerto, uts);

        String ultimoVisitado = lojaAnterior;

        List<String> encsEntregues = new ArrayList<>();
        for(String s: uts){
            for (String e: encs)
                if(s.equals(this.model.getEncomendas().get(e).getCodUtilizador()) && !encsEntregues.contains(e)) {
                    String codEnc = this.model.getEncomendas().get(e).getCodEncomenda();
                    kms += this.model.distUlt(ultimoVisitado, s);
                    this.model.entregaEncTrEncs(codTr, codEnc, kms, inicio, filas);
                    encsEntregues.add(e);
                    this.view.encEntregue(e);
                }
        }
        this.model.atualizaKms(codTr, kms);
        char c = this.view.disponivelNovamente();

        if (c == 's') {
            this.model.disponibilidadeTr(codTr, true);
            if(medico)
                this.model.disponivelMedicoTr(true, codTr);
        }
    }

//------------------------------ Lojas ------------------------------
    /**
     * Adicionar uma loja
     */
    private void addStore(){

        String code = this.model.criaLoja();
        this.view.criaLoja(code);

        String nome = this.view.inputNome();

        String password = this.view.inputPassword();

        double[] coordenadas = this.view.lerCoordenadas();

        this.model.newLoja(code, nome, password, coordenadas);
    }

    /**
     * Adicionar o peso a uma encomenda recem chegada
     */
    private void addPesoEnc(String codLoja, String codEnc){
        String enc = this.model.getEncomendas().get(codEnc).toString();
        this.view.imprime(enc);

        double peso = this.view.pesoEnc();
        this.model.adicionaPesoEnc(codLoja, codEnc, peso);
    }

//------------------------------ Login -----------------------------------
    /**
     * Metodo que faz login na aplicacao
     */
    private String login(){
        this.view.login();

        String codigo = this.view.inputCode();

        String password = this.view.inputPassword();

        boolean b = this.model.loginValido(codigo, password);
        while (!b){
            this.view.erroLogin();

            codigo = this.view.inputCode();

            password = this.view.inputPassword();

            b = this.model.loginValido(codigo, password);
        }
        return codigo;
    }

    /**
     * Metodo que faz o regitos na aplicacao
     */
    private void registo(){
        char r = this.view.registo();

        switch (r) {
            case 'u':
                addUser();
                break;
            case 'v':
                addVoluntary();
                break;
            case 't':
                addCompany();
                break;
            case 'l':
                addStore();
                break;
            case 'q':
                break;
            default:
        }
    }

//------------------------------ Funcoes comuns a varios ----------------------
    private LocalDateTime reformulaData(LocalDateTime d){
        int minInicio = d.getMinute();
        int horaInicio = d.getHour();
        return LocalDateTime.of(d.toLocalDate(), LocalTime.of(horaInicio, minInicio));

    }
//------------------------------ Menu Utilizador ------------------------------
    private void menuUtilizador(String codUt){
        this.view.bemVindo(this.model.getUtilizadores().get(codUt).getNome());
        int n = this.view.menuUtilizador();

        while (n != 0 && n !=5){
            switch (n){
                case 1:
                    solicitaEnc(codUt);
                    break;
                case 2:
                    consultaRegistos(codUt);
                    break;
                case 3:
                    voluntariosMaisUtilizam();
                    break;
                case 4:
                    transportadorasMaisUtilizam();
                    break;
                default:
            }
            n = this.view.menuUtilizador();
        }

        if(n == 5)
            this.model.removeUtilizador(codUt);

    }
//------------------------------ Menu Voluntario ------------------------------
    private void menuVoluntario(String codVl){
        this.view.bemVindo(this.model.getVoluntarios().get(codVl).getNome());
        int n = this.view.menuVoluntario();
        boolean medico = this.model.eMedicoVl(codVl);

        while (n != 0 && n != 5){
            switch (n){
                case 1:
                    this.model.disponibilidadeVl(codVl, true);
                    this.view.estadoVoluntario();
                    if(medico && this.view.disponivelParaMedico() == 's')
                        this.model.disponivelMedicoVl(true, codVl);
                    else medico = false;
                    break;
                case 2:
                    if(this.model.getVoluntarios().get(codVl).isDisponivel())
                        transporteEncomendasVl(codVl, medico);
                    else this.view.aindaNaoDisponivel();
                    break;
                case 3:
                    voluntariosMaisUtilizam();
                    break;
                case 4:
                    transportadorasMaisUtilizam();
                    break;
                default:
            }
            n = this.view.menuVoluntario();
        }
        if(n == 5)
            this.model.removeVoluntario(codVl);
        else this.model.disponibilidadeVl(codVl, false);
    }
//------------------------------ Menu Transportadora ------------------------------
    private void menuTransportadora(String codTr){
        this.view.bemVindo(this.model.getEmpresas().get(codTr).getNome());
        int n = this.view.menuTransportadora();
        boolean medico = this.model.eMedicoTr(codTr);

        while (n != 0 && n != 6){
            switch (n){
                case 1:
                    this.model.disponibilidadeTr(codTr, true);
                    if(medico && this.view.disponivelParaMedico() == 's')
                        this.model.disponivelMedicoTr(true, codTr);
                    else medico = false;
                    break;
                case 2:
                    LocalDateTime inicio = this.view.qualDataInicio();
                    LocalDateTime fim = this.view.qualDataFim();
                    double total = this.model.getEmpresas().get(codTr).totalFaturado(inicio, fim);
                    this.view.totalFaturado(total);
                    break;
                case 3:
                    if(this.model.getEmpresas().get(codTr).isDisponivel())
                        transporteEncomendasTr(codTr, medico);
                    else this.view.aindaNaoDisponivel();
                    break;
                case 4:
                    voluntariosMaisUtilizam();
                    break;
                case 5:
                    transportadorasMaisUtilizam();
                    break;
                default:
            }
            n = this.view.menuTransportadora();
        }
        if(n == 6)
            this.model.removeEmpresas(codTr);
        else this.model.disponibilidadeTr(codTr, false);
    }
//------------------------------ Menu Loja ------------------------------
    private void menuLoja(String codLoja){
        this.view.bemVindo(this.model.getLojas().get(codLoja).getNome());
        int nfe;
        String codEnc;
        int n = this.view.menuLoja();

        while (n != 5 && n != 0){
            switch (n){
                case 1:
                    codEnc = this.model.encLoja(codLoja);
                    this.view.printNovaEnc();
                    addPesoEnc(codLoja, codEnc);
                    break;
                case 2:
                    nfe = this.model.getLojas().get(codLoja).filaEspera();
                    this.view.printNFilaEspera(nfe);
                    break;
                case 3:
                    voluntariosMaisUtilizam();
                    break;
                case 4:
                    transportadorasMaisUtilizam();
                    break;
                default:
            }
            String codFstEnc;
            if(LocalDateTime.now().getNano() % 4 == 0 && this.model.getLojas().get(codLoja).getFe().size() > 0){
                codFstEnc = this.model.getLojas().get(codLoja).getFe().get(0);
                String codTransporte = this.model.qualTransportadora(this.model.getEncomendas().get(codFstEnc), new ArrayList<>());
                if(!codTransporte.equals("")) {
                    LocalDateTime inicio = this.model.vaiBuscarEncTr(codTransporte, codFstEnc);
                    inicio = reformulaData(inicio);

                    this.model.entregaEncTr(codTransporte, codFstEnc, inicio, 0);
                    this.view.encRecolhida(codFstEnc);

                    double kms = this.model.distTotal(codTransporte, codFstEnc);
                    this.model.atualizaKms(codTransporte, kms);
                }
            }

            n = this.view.menuLoja();
        }
        if(n == 5)
            this.model.removeLoja(codLoja);
    }
// ------------------------------- Menu ---------------------------------------------------
    public void menu() {
        char lr = this.view.login_registo();

        if (lr == 'r')
            registo();
        if (lr == 'q');
        else {
            String codigo = login();
            switch (codigo.charAt(0)) {
                case 'u':
                    menuUtilizador(codigo);
                    break;
                case 'v':
                    menuVoluntario(codigo);
                    break;
                case 't':
                    menuTransportadora(codigo);
                    break;
                case 'l':
                    menuLoja(codigo);
                    break;
                default:
                    break;
            }
        }
    }

}