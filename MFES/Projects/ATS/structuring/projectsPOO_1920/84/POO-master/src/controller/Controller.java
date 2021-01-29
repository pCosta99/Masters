package controller;

import exceptions.*;
import model.*;
import view.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Controller {
    private final Menu menu;
    private final TrazAqui model;
    private User user;

    public Controller(TrazAqui model){
        this.menu = new Menu();
        this.model = model;

    }

    public void run() throws Exception {
        String titulo = "";
        while (this.menu.getRun()) {
            switch (menu.getMenu()) {
                case Login: //deve estar
                    try {
                        NovoLogin log = menu.novoLogin(titulo);
                        this.user = model.login(log.getUser(), log.getPassword());
                        if (user instanceof Cliente) menu.selecionaOpcao(Menu.MenuIndicador.MenuCliente);

                        else {
                            menu.selecionaOpcao(Menu.MenuIndicador.MenuVoluntario);
                            this.model.getVolD().add((Voluntario) this.user);
                        }
                        titulo = "";


                    } catch (ExcecaoLogin e) {
                        System.out.println("Password ou User errados!");
                    }
                    break;

                case RegistoCliente:
                    try {
                        RegistoUser user = menu.registoUser(titulo);
                        List<Encomenda> listavaziaRegisto = new ArrayList<>();
                        List<Encomenda> listaPending = new ArrayList<>();
                        Cliente cliente = new Cliente(user.getId(), user.getNome(),
                                user.getEmail(), user.getPassword(),
                                user.getMorada(), user.getCoor(),
                                listavaziaRegisto, listaPending);
                        this.model.addUser(cliente);
                        menu.retrocesso();
                        titulo = "";


                    } catch (ExcecaoRegisto | ExcecaoCoordenadasInvalidas | ExcecaoUserExistente excecaoRegisto) {
                        excecaoRegisto.printStackTrace();
                    }
                    break;

                case RegistoVoluntario: //deve estar
                    try {
                        RegistoUser user = menu.registoUser(titulo);
                        List<Viagem> viagens = new ArrayList<>();
                        List<Encomenda> listVaziaRegisto = new ArrayList<>();
                        List<Encomenda> pendingLista = new ArrayList<>();
                        Voluntario voluntario = new Voluntario(user.getId(), user.getNome(),
                                user.getEmail(), user.getPassword(),
                                user.getMorada(), user.getCoor(),
                                user.getRaio(), user.isSpecial(),
                                user.isStatus(), viagens, 0, 0, 0, listVaziaRegisto, pendingLista);
                        //listVaziaRegisto,pendingLista);
                        this.model.addUser(voluntario);
                        this.model.desocupado(voluntario);

                        this.menu.selecionaOpcao(Menu.MenuIndicador.Inicio);


                    } catch (Exception excecaoCoordenadasInvalidas) {
                        excecaoCoordenadasInvalidas.printStackTrace();
                    }
                    break;


                case Lojas: //deve estar
                    try {
                        this.menu.showLojas(titulo, this.model.getLojas()
                                .stream()
                                .map(l -> Arrays.asList(l.toString().split("\n")))
                                .collect(Collectors.toList()));


                        EscolheLoja loja = this.menu.escolheLoja(titulo);
                        Loja escolhida = this.model.getLojaEspecifica(loja.getLoja());
                        switch (loja.getAceita()) {
                            case "sim":
                                this.menu.showProdutos(titulo, this.model.getLojaEspecifica(escolhida.getCod()).getprodutos()
                                        .stream()
                                        .map(l -> Arrays.asList(l.toString().split("\n")))
                                        .collect(Collectors.toList()));
                                this.menu.retrocesso();
                                break;

                            case "nao":
                                this.menu.selecionaOpcao(Menu.MenuIndicador.Lojas);
                                break;

                            default:
                                this.menu.selecionaOpcao(Menu.MenuIndicador.Admin);
                                break;

                        }

                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                    break;

                case ShowEmpresasTransporte:
                    try {
                        this.menu.showEmpresasTransporte(titulo, this.model.getListaEmpresa().stream()
                                .map(l -> Arrays.asList(l.toString().split("\n")))
                                .collect(Collectors.toList()));
                        this.menu.selecionaOpcao(Menu.MenuIndicador.EmpresasTransportadoras);
                        titulo = "";
                        break;
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                    break;

                case FacturacaoEmpresa:
                    try {
                        String nomeEmp = this.menu.escolheEmpresaTransporte(titulo);
                        EmpresaTransporte e = this.model.getEmpresaEspecifica(nomeEmp);
                        PeriodoTempo periodo = this.menu.getPeriododeString(titulo);
                        this.menu.showS("Facturacao: " + this.model.facturacaoPeriodo(e, periodo.getInicio(), periodo.getFin()));
                        this.menu.selecionaOpcao(Menu.MenuIndicador.EmpresasTransportadoras);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }





                case ConsultaRegistodeEncomendasEmpresas:
                    try {
                        String nomeEmpresa = this.menu.escolheEmpresaTransporte(titulo);
                        EmpresaTransporte empresa = this.model.getEmpresaEspecifica(nomeEmpresa);

                        this.menu.showEncomenda(titulo, this.model.getListEncomendasEmpresa(empresa)
                                .stream()
                                .map(Encomenda::toParsableEmpresaEncomenda)
                                .map(enc -> Arrays.asList(enc.split("\n")))
                                .collect(Collectors.toList()));
                        this.menu.selecionaOpcao(Menu.MenuIndicador.Admin);
                        titulo = "";

                    } catch (ExcecaoRegistoDeEncomendasNull | ExcecaoEmpresaInexistente m) {
                        titulo = "Encomenda Inexistente";
                    }
                    break;

                case ConsultaRegistodeEncomendasVoluntarioAdmin:
                    try {
                        this.menu.showVoluntarios(titulo, this.model.getUtilizadoresVoluntarios()
                                .stream()
                                .map(l -> Arrays.asList(l.toString()
                                        .split("\n")))
                                .collect(Collectors.toList()));


                        String voluntarioid = this.menu.escolheVoluntario(titulo);

                        //Voluntario vol = (Voluntario) this.model.getUser(voluntarioid);
                        this.menu.showEncomenda(titulo, ((Voluntario) this.model.getUser(voluntarioid)).getRegistoEncomendasHistorico()
                                .stream()
                                .map(Encomenda::toParsableVoluntarioEncomenda)
                                .map(enc -> Arrays.asList(enc.split("\n")))
                                .collect(Collectors.toList()));


                    } catch (Exception m) {
                        m.printStackTrace();
                    }
                    break;

                case Top10Clientes:
                    try {
                        this.menu.melhoresClientes(this.model.lista10ClientesMaisAtivos()
                                .stream()
                                .map(c -> Arrays.asList(c.getId(), String.format("%d", c.numeroEncomendas())))
                                .collect(Collectors.toList()));
                        this.menu.retrocesso();
                        break;
                    }catch (Exception exception) {
                        exception.printStackTrace();
                    }


                case Top10Empresas: //Falta comparador para empresas
                    try {
                        this.menu.melhoresClientes(this.model.lista10EmpresasMaisAtivas()
                                .stream()
                                .map(c -> Arrays.asList(c.getId(), String.format("%d", c.numeroEncomendas())))
                                .collect(Collectors.toList()));

                        this.menu.selecionaOpcao(Menu.MenuIndicador.Admin);
                    } catch (Exception exception) {
                        exception.printStackTrace();
                    }


                case ConsultaRegistodeEncomendasVoluntario: //deve estar
                    try {
                        Voluntario voluntario = (Voluntario) this.user;
                        List<Encomenda> registo = this.model.getHistoricoVoluntario(voluntario);
                        if (registo == null) {
                            System.out.println("Registo sem encomendas");
                            break;
                        } else {
                            this.menu.consultarRegistodeEncomendasVoluntario(registo
                                    .stream()
                                    .map(Encomenda::toParsableVoluntarioEncomenda)
                                    .map(enc -> Arrays.asList(enc.split("\n")))
                                    .collect(Collectors.toList()));

                            this.menu.selecionaOpcao(Menu.MenuIndicador.MenuVoluntario);
                            titulo = "";
                        }


                    } catch (Exception me) {
                        me.printStackTrace();
                    }
                    break;


                case ConsultaPedidosCliente:
                    try {
                        Cliente client = (Cliente) this.user;
                        PeriodoTempo period = this.menu.getPeriododeString(titulo);
                        List<Encomenda> lista = this.model.getListaEncomendaporPeriodoCliente(client,period.getInicio(),period.getFin());
                        this.menu.consultaRegistodeEncomendasCliente(period, lista.stream()
                                .map(Encomenda::toParsableClienteEncomenda)
                                .map(enc -> Arrays.asList(enc.split("\n")))
                                .collect(Collectors.toList()));
                        this.menu.retrocesso();
                        titulo = "";

                    } catch (ExcecaoPeriododeTempo excecaoPeriododeTempo) {
                        excecaoPeriododeTempo.printStackTrace();
                    }
                    break;

                case Carrinho:
                    try {

                        String escolha = this.menu.perguntaPagar(titulo); //escolhe loja e decide se paga pagamento
                        switch (escolha) {
                            case "sim":
                                this.menu.selecionaOpcao(Menu.MenuIndicador.FinalizaComEmpresa);

                                break;

                            case "nao":
                                this.menu.selecionaOpcao(Menu.MenuIndicador.FinalizaComVoluntario);
                                break;

                            case "b":
                                this.menu.selecionaOpcao(Menu.MenuIndicador.MenuCliente);


                            default:
                                break;

                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }


                case FinalizaComEmpresa:
                    try {
                        Cliente cliente = (Cliente) this.user;
                        boolean special = menu.perguntaEstatutoLoja();
                        if (special) {
                            this.menu.showLojas(titulo, this.model.getLojascomEspecial()
                                    .stream()
                                    .map(l -> Arrays.asList(l.toString().split("\n")))
                                    .collect(Collectors.toList()));


                            EscolheLoja escolhida = this.menu.escolheLojaConfirma(titulo);
                            Loja loja = model.getLojaEspecifica(escolhida.getLoja()); //vai buscar loja

                            switch (escolhida.getAceita()) {
                                case "sim":
                                    this.menu.showProdutos(titulo, loja.getprodutos()
                                            .stream()
                                            .map(l -> Arrays.asList(l.toString().split("\n")))
                                            .collect(Collectors.toList()));


                                    List<EscolheProduto> listaProdutos = new ArrayList<>();
                                    List<Produto> listaOficial = new ArrayList<>();
                                    while (menu.adicionaProdutos(titulo).charAt(0) == 's') {
                                        switch (menu.adicionaProdutos(titulo)) {
                                            case "sim":
                                                EscolheProduto produto = menu.adicionaProduto();
                                                //System.out.println(loja.getCod());
                                                Produto prod = model.procuraProdutoLoja(loja,produto.getProduto());//erro
                                                listaOficial.add(prod);
                                                break;


                                            case "nao":
                                                this.menu.aceitaProdutos(listaOficial.stream()
                                                        .map(l -> Arrays.asList(l.toString().split("\n")))
                                                        .collect(Collectors.toList()));
                                                break;

                                            default:
                                                this.menu.retrocesso();
                                                break;
                                        }

                                    }


                                    //List<Produto> listaOficial = new ArrayList<>();


                                    this.menu.showEmpresasTransporte(titulo, this.model.getListaEmpresa().stream()
                                            .map(l -> Arrays.asList(l.toString().split("\n")))
                                            .collect(Collectors.toList()));


                                    String idEmpresa = this.menu.escolheEmpresaTransporte(titulo);
                                    EmpresaTransporte empresa = this.model.getEmpresaEspecifica(idEmpresa);

                                    Encomenda ultima = this.model.pedidoEncomendaParaEmpresa(cliente, empresa, cliente.getCoordenadas(), true, loja, listaOficial);
                                    this.menu.showS(ultima.toString());
                                    this.menu.selecionaOpcao(Menu.MenuIndicador.MenuCliente);

                                    break;
                                case "nao":
                                    this.menu.retrocesso();
                                    break;


                                default:
                                    this.menu.selecionaOpcao(Menu.MenuIndicador.MenuCliente);
                                    break;
                            }


                        } else {
                            this.menu.showLojas(titulo, this.model.getLojas()
                                    .stream()
                                    .map(l -> Arrays.asList(l.toString().split("\n")))
                                    .collect(Collectors.toList()));

                            EscolheLoja escolhida = this.menu.escolheLojaConfirma(titulo);
                            Loja loja = model.getLojaEspecifica(escolhida.getLoja());

                            switch (escolhida.getAceita()) {
                                case "sim":
                                    this.menu.showProdutos(titulo, loja.getprodutos()
                                            .stream()
                                            .map(l -> Arrays.asList(l.toString().split("\n")))
                                            .collect(Collectors.toList()));


                                    List<EscolheProduto> listaProdutos = new ArrayList<>();

                                    while (menu.adicionaProdutos(titulo).charAt(0) == 's') {
                                        switch (menu.adicionaProdutos(titulo)) {
                                            case "sim":
                                                EscolheProduto produto = menu.adicionaProduto();
                                                listaProdutos.add(produto);

                                                break;

                                            case "nao":
                                                break;

                                            default:
                                                this.menu.retrocesso();
                                                break;
                                        }

                                    }


                                    List<Produto> listaOficial = new ArrayList<>();
                                    for (EscolheProduto e : listaProdutos) {
                                        listaOficial.add(loja.getProduto(e.getProduto()));
                                    }


                                    this.menu.showEmpresasTransporte(titulo, this.model.getListaEmpresa().stream()
                                            .map(l -> Arrays.asList(l.toString().split("\n")))
                                            .collect(Collectors.toList()));


                                    String idEmpresa = this.menu.escolheEmpresaTransporte(titulo);
                                    EmpresaTransporte empresa = this.model.getEmpresaEspecifica(idEmpresa);

                                    Encomenda ultima = model.pedidoEncomendaParaEmpresa(cliente, empresa, cliente.getCoordenadas(), true, loja, listaOficial);
                                    this.menu.showS(ultima.toString());




                                    break;
                                case "nao":
                                    this.menu.retrocesso();
                                    break;


                                default:
                                    this.menu.selecionaOpcao(Menu.MenuIndicador.MenuCliente);
                                    break;
                            }
                        }

                        this.menu.selecionaOpcao(Menu.MenuIndicador.MenuCliente);
                        break;
                    } catch (ExcecaoLojaInexistente | ExcecaoProdutoInexistente | ExcecaoEmpresaInexistente excecaoLojaInexistente) {
                        excecaoLojaInexistente.printStackTrace();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }

                case FinalizaComVoluntario:
                    Cliente cliente = (Cliente) this.user;
                    boolean special = menu.perguntaEstatutoLoja();
                    try {

                        if (special){
                            this.menu.showLojas(titulo, this.model.getLojascomEspecial()
                                    .stream()
                                    .map(l -> Arrays.asList(l.toString().split("\n")))
                                    .collect(Collectors.toList()));


                            EscolheLoja escolhida = this.menu.escolheLojaConfirma(titulo);
                            Loja loja = model.getLojaEspecifica(escolhida.getLoja());

                            switch (escolhida.getAceita()) {
                                case "sim":
                                    this.menu.showProdutos(titulo, loja.getprodutos()
                                            .stream()
                                            .map(l -> Arrays.asList(l.toString().split("\n")))
                                            .collect(Collectors.toList()));


                                    List<EscolheProduto> listaProdutos = new ArrayList<>();

                                    while (menu.adicionaProdutos(titulo).charAt(0) == 's') {
                                        switch (menu.adicionaProdutos(titulo)) {
                                            case "sim":
                                                EscolheProduto produto = menu.adicionaProduto();
                                                listaProdutos.add(produto);

                                                break;

                                            case "nao":
                                                break;

                                            default:
                                                this.menu.selecionaOpcao(Menu.MenuIndicador.MenuCliente);
                                                break;
                                        }

                                    }


                                    List<Produto> listaOficial = new ArrayList<>();
                                    for (EscolheProduto e : listaProdutos) {
                                        listaOficial.add(loja.getProduto(e.getProduto()));
                                    }

                                    //voluntario mais perto
                                    Voluntario escolhido = this.model.voluntarioEscolhido(cliente.getCoordenadas(), loja);
                                    Encomenda encomendaFinal = this.model.pedidoEncomendaParaVoluntario(cliente, escolhido, cliente.getCoordenadas(), true, loja, listaOficial);


                                    this.menu.showS(encomendaFinal.toString());

                                    break;
                                case "nao":
                                    break;


                                default:
                                    this.menu.selecionaOpcao(Menu.MenuIndicador.MenuCliente);
                                    break;
                            }


                        } else {

                                this.menu.showLojas(titulo, this.model.getLojas()
                                        .stream()
                                        .map(l -> Arrays.asList(l.toString().split("\n")))
                                        .collect(Collectors.toList()));

                                EscolheLoja escolhida = this.menu.escolheLojaConfirma(titulo);
                                Loja loja = model.getLojaEspecifica(escolhida.getLoja());

                                switch (escolhida.getAceita()) {
                                    case "sim":
                                        this.menu.showProdutos(titulo, loja.getprodutos()
                                                .stream()
                                                .map(l -> Arrays.asList(l.toString().split("\n")))
                                                .collect(Collectors.toList()));


                                        List<EscolheProduto> listaProdutos = new ArrayList<>();

                                        while (menu.adicionaProdutos(titulo).charAt(0) == 's') {
                                            switch (menu.adicionaProdutos(titulo)) {
                                                case "sim":
                                                    EscolheProduto produto = menu.adicionaProduto();
                                                    listaProdutos.add(produto);

                                                    break;

                                                case "nao":
                                                    break;

                                                default:
                                                    this.menu.retrocesso();
                                                    break;
                                            }

                                        }


                                        List<Produto> listaOficial = new ArrayList<>();
                                        for (EscolheProduto e : listaProdutos) {
                                            listaOficial.add(loja.getProduto(e.getProduto()));
                                        }

                                        //Escolhe voluntario mais perto
                                        Voluntario escolhido = this.model.voluntarioEscolhido(cliente.getCoordenadas(), loja);
                                        Encomenda encomendaFinal = this.model.pedidoEncomendaParaVoluntario(cliente, escolhido, cliente.getCoordenadas(), false, loja, listaOficial);



                                        this.menu.showS(encomendaFinal.toString());
                                        System.out.println("Encomenda Feita");
                                        break;


                                    case "nao":
                                        break;


                                    default:
                                        this.menu.selecionaOpcao(Menu.MenuIndicador.MenuCliente);
                                        break;
                                }

                            }

                    } catch (Exception e) {
                        e.printStackTrace();
                    }


                case PendingVoluntario:
                    Voluntario vol = (Voluntario) this.user;
                    String resposta = this.menu.respostaEncomendaAoPendingPrint(titulo,this.model.getEncomendaPending(vol).stream()
                            .map(Encomenda::toParsableVoluntarioEncomenda)
                            .map(l -> Arrays.asList(l.toString().split("\n")))
                            .collect(Collectors.toList()));

                    switch(resposta){
                        case "nao":
                            Encomenda encomenda = this.model.getEncomendaPending(vol).get(0);
                            this.model.removeEncomendaPendingVoluntario(vol,encomenda);
                            this.menu.retrocesso();

                            break;

                        case "sim":
                            Encomenda encomenda1 = this.model.getEncomendaPending(vol).get(0);
                            Cliente c = encomenda1.getCliente();
                            this.model.removeEncomendaPendingVoluntario(vol,encomenda1);
                            this.model.addEncomendaHistoricoAVoluntario(vol,encomenda1);
                            this.model.removeEncomendaPendingCliente(c,encomenda1);
                            this.model.addEncomendaHistoricoACliente(c, encomenda1);
                            System.out.println("Encomenda Aceite");
                            break;

                        default:
                            this.menu.selecionaOpcao(Menu.MenuIndicador.PendingVoluntario);
                            break;

                    }

                    this.menu.selecionaOpcao(Menu.MenuIndicador.MenuVoluntario);
                    break;

                default:
                    this.menu.deFault();
                    break;

            }
        }
    }
}



/*



                case ConsultaRegistodeEncomendasEmpresas: //pede o periodo para a empresa designada
                       try {
                           EmpresaTransporte empresa = this.model.getEmpresaEspecifica(this.menu.escolheEmpresaTransporte(titulo));
                           PeriodoTempo periodo = this.menu.getPeriododeString(titulo); //vai perguntar primeiro o periodo
                           this.menu.consultaRegistodeEncomendasEmpresas(periodo,this.model.getListaEncomendaporPeriodo(empresa,periodo.getInicio(),periodo.getFin())
                                   .stream()
                                   .map(Encomenda::toParsableEmpresaEncomenda)
                                   .map(x -> Arrays.asList(x.split("\n")))
                                   .collect(Collectors.toList()));
                           this.menu.retrocesso();

                       } catch (Exception e) {
                           e.printStackTrace();
                       }
                       break;








                default:
                    this.menu.deFault();
                    break;
            }
        }
    }
}

                     */


