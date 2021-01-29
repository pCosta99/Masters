import Enums.Estado;
import Exception.*;

import java.io.Serializable;
import java.time.DateTimeException;
import java.time.LocalDateTime;
import static java.time.format.DateTimeFormatter.ofPattern;

import java.util.*;
import java.io.*;

/**
 * Classe que irá estabelcer a ponte entre todas as classes que visam apresentar mensagens/resultados ao utilizador e a BaseDeDados.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class Controller implements Serializable {
    private BaseDeDados    model;
    private final View     view;
    private final Menu     menuPrincipal;
    private final MenuTipo menuTipo;

    /**
     * Construtor do Controller.
     * @param bd, BaseDeDados do Controller a construir.
     * @param view, View do Controller a construir.
     * @param menuPrincipal, Menu do Controller a construir.
     * @param menuTipo, MenuTipo do Controller a construir.
     */
    public Controller(BaseDeDados bd, View view, Menu menuPrincipal, MenuTipo menuTipo){
        this.model = bd;
        this.view = view;
        this.menuPrincipal = menuPrincipal;
        this.menuTipo = menuTipo;
    }

    /**
     * Função responsável por chamar as funções que apresentam as opções ao utilizador e chamar as funções que respondem ao selecionado pelo mesmo.
     */
    public void run(){
        int escolha;
        do {
            View.printBlankLine();
            menuPrincipal.executa();
            escolha = menuPrincipal.getOption();
            switch(escolha){
                case 0: View.printSpacedMessage("Fechando!"); break;
                case 1: this.login();                              break;
                case 2: this.registo();                            break;
                case 3: this.lerEstado();                          break;
                case 4: this.gravaEstado();                        break;
                case 5: this.totalFaturado();                      break;
                case 6: this.top10Utilizadores();                  break;
                case 7: this.top10Empresas();                      break;
                case 8: this.classificacaoTransportador();         break;
                default: View.printOpcaoInvalida();
            }
        }while(escolha != 0);
    }

    /**
     * Função responsável por chamar as funções que apresentam todas as formas de login possíveis e chamar as funções que respondem ao selecionado pelo utilizador.
     */
    private void login(){
        View.printBlankLine();
        menuTipo.executa();
        String mail = Input.lerString("Mail: ","Não foi possível ler o mail indicado!");
        String pass = Input.lerString("Pass: ","Não foi possível ler a password indicada!");
        try {
            View.printBlankLine();
            String codigo = null;
            switch (this.menuTipo.getOption()) {
                case UTILIZADOR: codigo = this.model.getUtilizador(mail,pass); break;
                case VOLUNTARIO: codigo = this.model.getVoluntario(mail,pass); break;
                case EMPRESA:    codigo = this.model.getEmpresa(mail,pass);    break;
                case LOJA:       codigo = this.model.getLoja(mail,pass);       break;
            }
            this.view.setCodigo(codigo);
            this.logedin();
        } catch(CredenciaisErradasException e){System.out.println(e.getMessage());}
    }

    /**
     * Função responsável por chamar as funções que apresentam todas formas de Registo possíveis e chamar as funções que respondem ao selecionado pelo utilizador.
     */
    private void registo(){
        View.printBlankLine();
        menuTipo.executa();
        String nome = Input.lerString("Nome: ","Não foi possível ler o nome indicado!");
        Localizacao gps = Input.lerLocalizacao();
        String mail = Input.lerString("Mail: ","Não foi possível ler o mail indicado!");
        String pass = Input.lerString("Pass: ","Não foi possível ler a password indicada!");
        try {
            String s = null;
            switch (this.menuTipo.getOption()){
                case UTILIZADOR:
                    s = this.model.registaUtilizador(nome,gps,mail,pass);
                    break;
                case VOLUNTARIO:
                    s = this.model.registaVoluntario(nome,gps,mail,pass);
                    break;
                case EMPRESA:
                    s = this.model.registaEmpresa(nome,gps,mail,pass);
                    break;
                case LOJA:
                    s = this.model.registaLoja(nome,gps,mail,pass);
                    break;
            }
            this.view.setCodigo(s);
            this.view.sucessoRegisto(this.menuTipo.getOption());
        } catch (MailAlreadyRegisteredException e){
            System.out.println(e.getMessage());
        }
    }

    /**
     * Função responsável por chamar as funções de login disponíveis.
     */
    private void logedin(){
        switch(this.menuTipo.getOption()){
            case UTILIZADOR: logedinUtilizador(); break;
            case VOLUNTARIO: logedinVoluntario(); break;
            case EMPRESA:    logedinEmpresa();    break;
            case LOJA:       logedinLoja();       break;
        }
    }

    //////////////////////////////////////////    UTILIZADOR    ////////////////////////////////////////////////////////
    /**
     * Função responsável por chamar as funções disponíveis dos utilizadores.
     */
    private void logedinUtilizador(){
        int escolha;
        Menu user = new Menu(new String[]{"Consultar Dados",
                                          "Solicitar a entrega de uma encomenda",
                                          "Aceder à informação das entregas efectuadas num determinado período e por Transportador",
                                          "Classificar Transportador",
                                          "Encomendas à espera de decisão",
                                          "Ver classificação de um Transportador"});
        do {
            View.printBlankLine();
            this.view.printAtual(this.menuTipo.getOption());
            user.executa("Logout.");
            escolha = user.getOption();
            switch(escolha){
                case 0: View.printLogOut();                break;
                case 1: this.consultarDadosUtilizador();   break;
                case 2: this.encomendar();                 break;
                case 3: this.infoEncomendasTransportador();break;
                case 4: this.classificar();                break;
                case 5: this.encomendasDecisao();          break;
                case 6: this.classificacaoTransportador(); break;
                default: View.printOpcaoInvalida();
            }
        }while(escolha != 0);
    }

    /**
     * Função responsável por chamar as funções que permitem ao utilizador tomar uma decisão quanto à entrega de uma encomenda.
     */
    private void encomendasDecisao(){
        try {
            Map<String,Double> pendentes = this.model.getListPendentes(this.view.getCodigo());
            if(pendentes.isEmpty()){
                View.printSpacedMessage("Sem encomendas pendentes de aceitação!");
                return;
            }
            new Navegador(pendentes,"Encomenda: "," - Custo: ").show();
            String encomenda = Input.lerString("Código da Encomenda: ","Não foi possível ler o código da encomenda");
            Double preco = pendentes.get(encomenda);
            if(preco == null) throw new EncomendaNotFoundException(encomenda);
            char r = Input.lerChar(String.format("Aceita pagar %.2f (S|N)? ",preco),"Digite S ou N!","SN");
            this.model.responder(r == 'S' || r == 's',this.view.getCodigo(),encomenda);
        }
        catch (UtilizadorNotFoundException | TransportadorNotFoundException | EncomendaNotFoundException e){
            View.printSpacedMessage("A encomenda não se encontra nas encomendas que necessitam de resposta!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem ao utilizador fazer uma nova encomenda.
     */
    private void encomendar(){
        char mostrarLoja, medica;
        mostrarLoja = Input.lerChar("Deseja ver todas as lojas (S|N)? ", "Digite S ou N!","SN");
        if(mostrarLoja == 's' || mostrarLoja == 'S')
            new Navegador(this.model.getAllCodigoLoja()).show();

        String loja = Input.lerString("Código da loja: ","Não foi possível ler o código da loja.");
        double peso = Input.lerDouble("Peso da Encomenda: ", "Digite um Valor Positivo.");

        medica = Input.lerChar("É uma encomenda médica (S|N)? ","Caractér Inválido.","SN");
        boolean certificado = medica == 's' || medica == 'S';

        try{
            List<LinhaEncomenda> l = Controller.preencherEncomenda();
            String codEncomenda = this.model.add(this.view.getCodigo(),loja,peso,l,certificado);
            View.printSpacedMessage("Adicionada a encomenda com o código " + codEncomenda);
        }
        catch (EncomendaInvalidaException | LojaNotFoundException e) {
            View.printSpacedMessage(e.getMessage() + "\nNão foi adicionada nenhuma encomenda!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem preencher encomendas.
     */
    private static List<LinhaEncomenda> preencherEncomenda() {
        List<LinhaEncomenda> l = new ArrayList<>();
        char adicionar;
        do {
            String produto   = Input.lerString("Código do produto: ", "Não foi possível ler o código do produto");
            String descricao = Input.lerString("Descrição: ", "Não foi possível ler a descrição do produto");
            double quant     = Input.lerDouble("Quantidade: ", "Insira um valor positivo.");
            double valorUnit = Input.lerDouble("Preço Unitário: ", "Insira um valor positivo");
            l.add(new LinhaEncomenda(produto, descricao, quant, valorUnit));
            adicionar = Input.lerChar("Deseja adicionar outra linha de Encomenda (S|N)? ", "Caractér Inválido","SN");
        } while (adicionar == 's' || adicionar == 'S');
        return l;
    }

    /**
     * Função responsável por chamar as funções que permitem a um utilizador consultar os seus dados.
     */
    private void consultarDadosUtilizador(){
        int escolha;
        Menu user = new Menu(new String[]{"Ver dados",
                                          "Alterar password",
                                          "Alterar mail"});
        do {
            View.printBlankLine();
            this.view.printAtual(this.menuTipo.getOption());
            user.executa("Retroceder.");
            escolha = user.getOption();
            switch(escolha){
                case 0: View.printMenuAnterior();         break;
                case 1: this.getDadosUtilizador();        break;
                case 2: this.alterarPasswordUtilizador(); break;
                case 3: this.alterarMailUtilizador();     break;
                default: View.printOpcaoInvalida();       break;
            }
        }while(escolha != 0);
    }

    /**
     * Imprime os dados do utilizador.
     */
    private void getDadosUtilizador(){
        try{
            View.printSpacedMessage(this.model.getDadosUtilizador(this.view.getCodigo()));
        } catch (UtilizadorNotFoundException e) {
            View.printSpacedMessage(e.getMessage());
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a um utilizador alterar o mail.
     */
    private void alterarMailUtilizador(){
        String mail = Input.lerString("Introduza o novo mail: ","Não foi possível ler o mail indicado!");
        try{
            this.model.alterarMailUtilizador(this.view.getCodigo(),mail);
            View.printSpacedMessage("Mail alterado com sucesso");
        }
        catch (UtilizadorNotFoundException e){
            View.printSpacedMessage(e.getMessage() + "\nMail inalterado!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a um utilizador alterar a password.
     */
    private void alterarPasswordUtilizador(){
        String pass = Input.lerString("Introduza a nova password: ","Não foi possível ler a password indicada!");
        try{
            this.model.alterarPasswordUtilizador(this.view.getCodigo(),pass);
            View.printSpacedMessage("Password alterada com sucesso");
        }
        catch (UtilizadorNotFoundException e){
            View.printSpacedMessage(e.getMessage() + "\nPassword inalterada!");
        }
    }

    //////////////////////////////////////////    VOLUNTARIO    ////////////////////////////////////////////////////////
    /**
     * Função responsável por chamar as funções disponíveis dos voluntários.
     */
    private void logedinVoluntario() {
        int escolha;
        Menu voluntario = new Menu(new String[]{"Consultar dados",
                                                "Alterar disponibilidade",
                                                "Marcar Encomenda como entregue",
                                                "Rejeitar entrega da encomenda",
                                                "Atualizar estado de certificado de transporte médico",
                                                "Ver encomendas transportadas"});
        do{
            View.printBlankLine();
            this.view.printAtual(this.menuTipo.getOption());
            this.mensagemEntregaAtual();
            voluntario.executa("Logout.");
            escolha = voluntario.getOption();
            switch(escolha){
                case 0: View.printLogOut();              break;
                case 1: this.consultarDadosVoluntario(); break;
                case 2: this.alterarDisponibilidade();   break;
                case 3: this.entregar();                 break;
                case 4: this.recusarTransportador();     break;
                case 5: this.alterarCertificado();       break;
                case 6: this.encomendasTransportadas();  break;
                default: View.printLogOut();
            }
        }while(escolha != 0);
    }

    /**
     * Função responsável por chamar as funções que permitem a um voluntario consultar os seus dados.
     */
    private void consultarDadosVoluntario(){
        int escolha;
        Menu user = new Menu(new String[]{"Ver dados",
                                          "Alterar password",
                                          "Alterar raio",
                                          "Alterar mail"});
        do {
            View.printBlankLine();
            user.executa("Retroceder.");
            escolha = user.getOption();
            switch(escolha){
                case 0: View.printLogOut();               break;
                case 1: this.getDadosTransportador();     break;
                case 2: this.alterarPasswordVoluntario(); break;
                case 3: this.alterarRaioVoluntario();     break;
                case 4: this.alterarMailVoluntario();     break;
                default: View.printOpcaoInvalida();
            }
        }while(escolha != 0);
    }

    /**
     * Função responsável por chamar as funções que permitem a um voluntario alterar o mail.
     */
    private void alterarMailVoluntario(){
        String mail = Input.lerString("Introduza o novo Mail: ","Não foi possível ler o mail indicado!");
        try{
            this.model.alterarMailVoluntario(this.view.getCodigo(),mail);
            View.printSpacedMessage("Mail alterado com sucesso!");
        }
        catch (VoluntarioNotFoundException e){
            View.printSpacedMessage(e.getMessage() + "\nMail inalterado!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a um voluntario alterar a password.
     */
    private void alterarPasswordVoluntario(){
        String pass = Input.lerString("Introduza a nova Password: ","Não foi possível ler a password indicada!");
        try{
            this.model.alterarPasswordVoluntario(this.view.getCodigo(),pass);
            View.printSpacedMessage("Password alterada com sucesso!");
        }
        catch (VoluntarioNotFoundException e){
            View.printSpacedMessage(e.getMessage() + "\nPassword inalterada!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a um voluntario alterar o raio.
     */
    private void alterarRaioVoluntario(){
        double raio = Input.lerDouble("Introduza o raio: ", "Valor inválido");
        try{
            this.model.alterarRaioVoluntario(this.view.getCodigo(),raio);
            View.printSpacedMessage("Raio alterado com sucesso!");
        }
        catch (VoluntarioNotFoundException | NotPositiveNumberException e) {
            View.printSpacedMessage(e.getMessage() + "\nRaio inalterado!");
        }
    }

    ///////////////////////////////////////////    EMPRESA    //////////////////////////////////////////////////////////
    /**
     * Função responsável por chamar as funções disponíveis das empresas.
     */
    private void logedinEmpresa() {
        int escolha;
        Menu empresa = new Menu(new String[]{"Consultar Dados",
                                             "Alterar disponibilidade",
                                             "Marcar Encomenda como entregue",
                                             "Rejeitar entrega da encomenda",
                                             "Atualizar estado de certificado de transporte médico",
                                             "Ver encomendas Transportadas"});
        do{
            View.printBlankLine();
            this.view.printAtual(this.menuTipo.getOption());
            this.mensagemEntregaAtual();
            empresa.executa("Logout.");
            escolha = empresa.getOption();
            switch(escolha){
                case 0: View.printLogOut();            break;
                case 1: this.consultarDadosEmpresa();  break;
                case 2: this.alterarDisponibilidade(); break;
                case 3: this.entregar();               break;
                case 4: this.recusarTransportador();   break;
                case 5: this.alterarCertificado();     break;
                case 6: this.encomendasTransportadas();break;
                default: View.printOpcaoInvalida();
            }
        }while(escolha != 0);
    }

    /**
     * Função responsável por chamar as funções que permitem apresentar a entrega e o seu estado.
     */
    private void mensagemEntregaAtual() {
        StringBuilder sb = new StringBuilder();
        try{
            boolean disp = this.model.getDisponibilidade(this.view.getCodigo());
            sb.append("Disponibilidade: ").append(disp ? "Disponível\n" : "Indisponível\n");
            if(!disp) {
                Estado e = this.model.getEstadoEntrega(this.view.getCodigo());
                String enc = this.model.getCodigoEncomendaTransportador(this.view.getCodigo());
                sb.append("Estado: ").append(enc).append(" - ").append(e).append('\n');
            }
        }
        catch (TransportadorNotFoundException | EncomendaNotFoundException e) {
            sb.append("Estado: --sem encomenda--");
        }
        View.printSpacedMessage(sb.toString());
    }

    /**
     * Função responsável por chamar as funções que permitem a uma empresa consultar os seus dados.
     */
    private void consultarDadosEmpresa(){
        int escolha;
        Menu user = new Menu(new String[]{"Ver dados",
                                          "Alterar password",
                                          "Alterar nif",
                                          "Alterar raio",
                                          "Alterar preco por km",
                                          "Alterar preco por peso",
                                          "Alterar preco por hora",
                                          "Alterar mail"});
        do {
            View.printBlankLine();
            user.executa("Retroceder.");
            escolha = user.getOption();
            switch(escolha){
                case 0: View.printMenuAnterior();          break;
                case 1: this.getDadosTransportador();      break;
                case 2: this.alterarPasswordEmpresa();     break;
                case 3: this.alterarNifEmpresa();          break;
                case 4: this.alterarRaioEmpresa();         break;
                case 5: this.alterarPrecoPorKmEmpresa();   break;
                case 6: this.alterarPrecoPorPesoEmpresa(); break;
                case 7: this.alterarPrecoPorHoraEmpresa(); break;
                case 8: this.alterarMailEmpresa();         break;
                default: View.printOpcaoInvalida();
            }
        }while(escolha != 0);
    }

    /**
     * Imprime os dados do Transportador.
     */
    private void getDadosTransportador(){
        try{
            View.printSpacedMessage(this.model.getDadosTransportador(this.view.getCodigo()));
        } catch (TransportadorNotFoundException e) {
            View.printSpacedMessage(e.getMessage());
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma empresa alterar o mail.
     */
    private void alterarMailEmpresa(){
        String mail = Input.lerString("Introduza o novo mail: ","Não foi possível ler o mail indicado!");
        try{
            this.model.alterarMailEmpresa(this.view.getCodigo(),mail);
            View.printSpacedMessage("Mail alterado com sucesso");
        }
        catch (EmpresaNotFoundException e){
            View.printSpacedMessage(e.getMessage() + "\nMail inalterado!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma empresa alterar a password.
     */
    private void alterarPasswordEmpresa(){
        String pass = Input.lerString("Introduza a nova password: ","Não foi possível ler a password indicada!");
        try{
            this.model.alterarPasswordEmpresa(this.view.getCodigo(),pass);
            View.printSpacedMessage("Password alterada com sucesso");
        }
        catch (EmpresaNotFoundException e){
            View.printSpacedMessage(e.getMessage() + "\nPassword inalterada!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma empresa alterar o nif.
     */
    private void alterarNifEmpresa(){
        int nif = Input.lerInt("Introduza o NIF: ", "Valor inválido");
        try{
            this.model.alterarNifEmpresa(this.view.getCodigo(),nif);
            View.printSpacedMessage("NIF alterado com sucesso");
        }
        catch (EmpresaNotFoundException | NotPositiveNumberException e) {
            View.printSpacedMessage(e.getMessage() + "\nNIF inalterado!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma empresa alterar o raio.
     */
    private void alterarRaioEmpresa(){
        double raio = Input.lerDouble("Introduza o raio: ", "Valor inválido");
        try{
            this.model.alterarRaioEmpresa(this.view.getCodigo(),raio);
            View.printSpacedMessage("Raio alterado com sucesso");
        }
        catch (EmpresaNotFoundException | NotPositiveNumberException e) {
            View.printSpacedMessage(e.getMessage() + "\nRaio inalterado!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma empresa alterar a sua disponibilidade.
     */
    private void alterarDisponibilidade(){
        try {
            try {
                this.model.getEstadoEntrega(this.view.getCodigo());
                View.printSpacedMessage("Entregue primeiro a encomenda atual!");
            }
            catch(EncomendaNotFoundException e){
                char disp = Input.lerChar("Disponível (S|N): ","Digite S ou N!","SN");
                boolean d = disp == 's' || disp == 'S';
                this.model.alterarDisponibilidade(d,this.view.getCodigo());
                View.printSpacedMessage("Está agora " + (d ? "Disponível!" : "Indisponível!"));
            }
        } catch (TransportadorNotFoundException ignored) {} //Não acontece
    }

    /**
     * Função responsável por chamar as funções que permitem a uma empresa fazer uma entrega.
     */
    private void entregar(){
        String codEncomenda;
        try { codEncomenda = this.model.getCodigoEncomendaTransportador(this.view.getCodigo()); }
        catch (TransportadorNotFoundException | EncomendaNotFoundException e){
            View.printSpacedMessage("Sem encomenda a entregar!");
            return;
        }
        try {
            double tempo = Input.lerDouble("Tempo necessário para entregar " +
                                                    codEncomenda +
                                                    "\n> ","Digite um número positivo!");
            this.model.entregue(this.view.getCodigo(), codEncomenda, tempo);
            View.printSpacedMessage(codEncomenda + " foi entregue!");
        }
        catch (TransportadorNotFoundException | EncomendaNotFoundException | UtilizadorNotFoundException e){
            View.printSpacedMessage(e.getMessage());
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma empresa alterar o preço por km.
     */
    private void alterarPrecoPorKmEmpresa(){
        double precoPorKm = Input.lerDouble("Introduza o preco por km: ", "Valor invalido");
        try{
            this.model.alterarPrecoPorKmEmpresa(this.view.getCodigo(),precoPorKm);
            View.printSpacedMessage("Preco por km alterado com sucesso");
        }
        catch (EmpresaNotFoundException | NotPositiveNumberException e) {
            View.printSpacedMessage(e.getMessage() + "\nPreço Inalterado!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma empresa alterar o preço por peso.
     */
    private void alterarPrecoPorPesoEmpresa(){
        double precoPorPeso = Input.lerDouble("Introduza o preco por peso: ", "Valor invalido");
        try{
            this.model.alterarPrecoPorPesoEmpresa(this.view.getCodigo(),precoPorPeso);
            View.printSpacedMessage("Preco por peso alterado com sucesso");
        }
        catch (EmpresaNotFoundException | NotPositiveNumberException e) {
            View.printSpacedMessage(e.getMessage() + "\nPreço inalterado!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma empresa alterar o preço por hora.
     */
    private void alterarPrecoPorHoraEmpresa(){
        double precoPorHora = Input.lerDouble("Introduza o preco por hora: ", "Valor invalido");
        try{
            this.model.alterarPrecoPorHoraEmpresa(this.view.getCodigo(),precoPorHora);
            View.printSpacedMessage("Preco por hora alterado com sucesso");
        }
        catch (EmpresaNotFoundException | NotPositiveNumberException e) {
            View.printSpacedMessage(e.getMessage() + "\nPreço inalterado!");
        }
    }

    /////////////////////////////////////////////    LOJA    ///////////////////////////////////////////////////////////

    /**
     * Função responsável por chamar as funções disponíveis das lojas.
     */
    private void logedinLoja(){
        int escolha;
        Menu loja = new Menu((new String[]{"Consultar dados",
                                           "Aceitar encomenda"}));
        do{
            View.printBlankLine();
            this.view.printAtual(this.menuTipo.getOption());
            loja.executa("Logout.");
            escolha = loja.getOption();
            switch(escolha){
                case 0: View.printLogOut(); break;
                case 1: this.alterarDadosLoja(); break;
                case 2: this.aceitar(); break;
                default: View.printOpcaoInvalida();
            }
        }while (escolha!=0);
    }

    /**
     * Função responsável por chamar as funções que permitem a uma loja alterar os seus dados.
     */
    private void alterarDadosLoja(){
        int escolha;
        Menu user = new Menu(new String[]{"Ver dados",
                                          "Alterar password",
                                          "Alterar tempo médio de atendimento",
                                          "Alterar mail"});
        do {
            View.printBlankLine();
            user.executa("Retroceder.");
            escolha = user.getOption();
            switch(escolha){
                case 0: View.printMenuAnterior();          break;
                case 1: this.getDadosLoja();               break;
                case 2: this.alterarPasswordLoja();        break;
                case 3: this.alterarTempoMedioTempoLoja(); break;
                case 4: this.alterarMailLoja();            break;
                default: View.printOpcaoInvalida();
            }
        }while(escolha != 0);
    }

    /**
     * Imprime os dados da loja.
     */
    private void getDadosLoja(){
        try{
            View.printSpacedMessage(this.model.getDadosLoja(this.view.getCodigo()));
        } catch (UtilizadorNotFoundException e) {
            View.printSpacedMessage(e.getMessage());
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma loja alterar o mail.
     */
    private void alterarMailLoja(){
        String mail = Input.lerString("Introduza o novo mail: ","Não foi possível ler o mail indicado!");
        try{
            this.model.alterarMailLoja(this.view.getCodigo(),mail);
            View.printSpacedMessage("Mail alterado com sucesso");
        }
        catch (LojaNotFoundException e){
            View.printSpacedMessage(e.getMessage() + "\nMail inalterado!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma loja alterar a password.
     */
    private void alterarPasswordLoja(){
        String pass = Input.lerString("Introduza a nova password: ","Não foi possível ler a password indicada!");
        try{
            this.model.alterarPasswordLoja(this.view.getCodigo(),pass);
            View.printSpacedMessage("Password alterada com sucesso");
        }
        catch (LojaNotFoundException e){
            View.printSpacedMessage(e.getMessage() + "\nPassword inalterada!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma loja alterar o tempo médio.
     */
    private void alterarTempoMedioTempoLoja(){
        double tempoMedio = Input.lerDouble("Introduza o tempo medio: ", "Valor inválido");
        try{
            this.model.alterarTempoMedioLoja(this.view.getCodigo(),tempoMedio);
            View.printSpacedMessage("Tempo medio alterado com sucesso");
        }
        catch (LojaNotFoundException | NotPositiveNumberException e) {
            View.printSpacedMessage(e.getMessage() + "\nTempo Médio inalterado!");
        }
    }

    /**
     * Função responsável por chamar as funções que permitem a uma loja aceitar uma entrega.
     */
    private void aceitar(){
        List<String> entregas = null;
        try{
            entregas = this.model.getEntregasLoja(this.view.getCodigo());
            if(entregas.isEmpty()){
                View.printSpacedMessage("Sem encomendas para aceitar!");
                return;
            }
        } catch (LojaNotFoundException ignored) {} //Não acontece
        assert(entregas!=null);
        char mostrarEnc = Input.lerChar("Deseja ver as entregas disponíveis (S|N)? ","Digite S ou N!","SN");
        if(mostrarEnc == 's' || mostrarEnc == 'S')
            new Navegador(entregas).show();
        String encomenda = Input.lerString("Código da Encomenda: ","Não foi possível ler o código da Encomenda!");
        try {
            this.model.entregarEncomenda(this.view.getCodigo(),encomenda);
            View.printSpacedMessage("A encomenda " + encomenda + " já pode ser entregue!");
        }
        catch (LojaNotFoundException | UtilizadorNotFoundException | EncomendaNotFoundException e) {
            View.printSpacedMessage(e.getMessage() + "\nNão foram feitas alterações!");
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Função responsável por chamar as funções que permitem gravar um estado.
     */
    private void gravaEstado(){
        String ficheiro = Input.lerString("Nome do ficheiro: ","Não foi possível ler o nome do ficheiro.");
        try{
            this.model.gravarEstado(ficheiro);
            View.printSpacedMessage("Escrita em " + ficheiro + " com sucesso!");
        } catch (FileNotFoundException e) {
            View.printSpacedMessage("Não existe o ficheiro " + ficheiro);
        } catch (IOException e) {
            View.printSpacedMessage("Não foi possivel escrever no ficheiro " + ficheiro);
        }
    }

    /**
     * Função responsável por chamar as funções que permitem ler um estado.
     */
    private void lerEstado(){
        String ficheiro = Input.lerString("Nome do ficheiro: ","Não foi possível ler o nome do ficheiro.");
        try{
            this.model = BaseDeDados.carregarEstado(ficheiro);
            View.printSpacedMessage("Carregados os dados do ficheiro " + ficheiro + " com sucesso!");
        } catch (IOException | ClassNotFoundException | ClassCastException e) {
            View.printSpacedMessage("Não foi possível ler do ficheiro " + ficheiro);
        }
    }

    /**
     * Função responsável por chamar as funções que determinam o Top 10 de utilizadores que mais utilizaram o sistema em número de encomendas.
     */
    private void top10Utilizadores(){
        int i = 1;
        View.printBlankLine();
        for(String utilizador : this.model.listagemTop10Utilizadores())
            System.out.printf("#%d - %s\n",i++,utilizador);
        View.printBlankLine();
    }

    /**
     * Função responsável por chamar as funções que determinam o Top 10 de empresas que mais utilizaram o sistema em kms percorridos.
     */
    private void top10Empresas(){
        int i = 1;
        View.printBlankLine();
        for(String empresa : this.model.listagemTop10Empresas())
            System.out.printf("#%d - %s\n",i++,empresa);
        View.printBlankLine();
    }

    /**
     * Função responsável por chamar as funções que determinam o total faturado num período.
     */
    private void totalFaturado() {
        try {
            String codEmpresa = Input.lerString("Nome da Empresa: " , "Não foi possível ler o nome da empresa.");
            View.printSpacedMessage("Limite Inferior:");
            LocalDateTime inf = LocalDateTime.of(Input.lerInt("\tAno: ","\tAno inválido. "),
                                                 Input.lerInt("\tMês: ","\tMês Inválido.",1,12),
                                                 Input.lerInt("\tDia: ","\tDia Inválido.",1,31),
                                            0,0,0);
            View.printSpacedMessage("Limite Superior:");
            LocalDateTime sup = LocalDateTime.of(Input.lerInt("\tAno: ","\tAno inválido. "),
                                                 Input.lerInt("\tMês: ","\tMês Inválido.",1,12),
                                                 Input.lerInt("\tDia: ","\tDia Inválido.",1,31),
                                            23,59,59);
            double d = this.model.getFaturadoIntervalo(codEmpresa,inf,sup);
            View.printSpacedMessage(String.format("Total faturado pela empresa %s entre %s e %s é de %.2f",codEmpresa,
                                                                                                           inf.format(ofPattern("dd-LLL-yyyy")),
                                                                                                           sup.format(ofPattern("dd-LLL-yyyy")),
                                                                                                           d));
        } catch(EmpresaNotFoundException | DateTimeException | OrdemCronologicaErradaException e) {
            View.printSpacedMessage(e.getMessage());
        }
    }

    /**
     * Função responsável por chamar as funções que classificam o transportador.
     */
    private void classificar(){
        List<String> naoClassificados = null;
        try { naoClassificados = this.model.getListNaoClassificados(this.view.getCodigo()); }
        catch (UtilizadorNotFoundException ignored) {}
        assert naoClassificados != null;
        if (naoClassificados.isEmpty()){
            View.printSpacedMessage("Sem encomendas para classificar!");
            return;
        }
        else new Navegador(naoClassificados).show();
        String encomenda = Input.lerString("Código da Encomenda: ","Não foi possível ler o código da Encomenda!");
        int classificacao = Input.lerInt("Classificação a atribuir: ","Classificação inválida",1,10);
        try {
            this.model.classificar(this.view.getCodigo(),encomenda,classificacao);
            View.printSpacedMessage("Atribuiu com sucesso " + classificacao +
                                    " ao serviço de entrega da encomenda " + encomenda + "!");
        } catch (EncomendaNotFoundException | ClassificacaoInvalidaException | TransportadorNotFoundException | UtilizadorNotFoundException e) {
            View.printSpacedMessage(e.getMessage());
        }
    }

    /**
     * Função responsável por chamar as funções que permitem ao transportador rejeitar o transporte de uma encomenda.
     */
    private void recusarTransportador(){
        try {
            this.model.recusarTransportador(this.view.getCodigo());
            View.printSpacedMessage("Já não está a entregar a encomenda!");
        }
        catch (TransportadorNotFoundException | EncomendaNotFoundException | UtilizadorNotFoundException e){
            View.printSpacedMessage("Sem encomenda a entregar!");
        }
    }

    private void infoEncomendasTransportador(){
        String codTransportador = Input.lerString("Código do transportador: ","Não foi possível ler o código do transportador.");
        try{
            View.printSpacedMessage("Limite Inferior:");
            LocalDateTime inf = LocalDateTime.of(Input.lerInt("\tAno: ","\tAno inválido. "),
                                                 Input.lerInt("\tMês: ","\tMês Inválido.",1,12),
                                                 Input.lerInt("\tDia: ","\tDia Inválido.",1,31),
                                                 0,0,0);
            View.printSpacedMessage("Limite Superior:");
            LocalDateTime sup = LocalDateTime.of(Input.lerInt("\tAno: ","\tAno inválido. "),
                                                 Input.lerInt("\tMês: ","\tMês Inválido.",1,12),
                                                 Input.lerInt("\tDia: ","\tDia Inválido.",1,31),
                                                 23,59,59);
            Collection<String> t = this.model.getInfoUtilizadorTransportador(this.view.getCodigo(),codTransportador,inf,sup);
            if (t.isEmpty())
                View.printSpacedMessage("Sem informação sobre encomendas transportadas.");
            else new Navegador(t).show();
        } catch (OrdemCronologicaErradaException | UtilizadorNotFoundException | DateTimeException e) {
            View.printSpacedMessage(e.getMessage());
        }
    }

    /**
     * Função responsável por chamar as funções que determinam a classificação média de um transportador.
     */
    private void classificacaoTransportador(){
        String codTransportador = Input.lerString("Código do transportador: ","Não foi possível ler o código do transportador.");
        try {
            double od = this.model.classificacaoTransportador(codTransportador);
            if (od == 0)
                View.printSpacedMessage("Ainda não foi atribuida uma classificação ao transportador " + codTransportador + ".");
            else
                View.printSpacedMessage("O transportador " + codTransportador + " possui uma classificacão média de " + od + "!");
        } catch (TransportadorNotFoundException e) {
            View.printSpacedMessage(e.getMessage());
        }
    }

    /**
     * Função que é responsável por chamar as funções que alteram o certificado de um transportador.
     */
    private void alterarCertificado(){
        char r = Input.lerChar("Tem certificado para transporte de encomendas médicas (S|N)? ","Digite S ou N!","SN");
        boolean state = r == 's' || r == 'S';
        try{
            this.model.alterarCertificado(this.view.getCodigo(),state);
            View.printSpacedMessage((state ? "Está agora" : "Não está") + " apto para fazer transporte de encomendas médicas!");
        }
        catch (TransportadorNotFoundException e) {
            View.printSpacedMessage(e.getMessage() + "\nCertificado inalterado!");
        }
    }

    /**
     * Imprime no ecrã um navegador sobre as encomendas transportados pelo tranportador atual;
     */
    private void encomendasTransportadas(){
        try { new Navegador1Elemento(this.model.encomendasTransportadas(this.view.getCodigo())).show(); }
        catch (TransportadorNotFoundException e) {
            View.printSpacedMessage(e.getMessage());
        }
    }
}
