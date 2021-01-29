package Controladores;

import Exceptions.AlreadyEvaluatedException;
import Exceptions.ProdutoInexistenteException;
import Exceptions.UserInexistenteException;
import Modelos.TrazAqui;
import Modelos.TrazAquiModel;
import Readers.Input;
import Users.Loja;
import Users.Transportadora;
import Users.User;
import Users.Voluntario;
import Views.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class MainController implements TrazAquiController {
    private TrazAquiView view;
    private TrazAquiModel model;

    /**
     * Método que define o modelo a utilizar no programa.
     * @param m Modelo.
     */
    @Override
    public void setModel(TrazAquiModel m) {
        this.model = m;
    }

    /**
     * Método que define a view a utilizar no programa.
     * @param v View.
     */
    @Override
    public void setView(TrazAquiView v) {
        this.view = v;
    }

    /**
     * Controlador principal do programa.
     * @throws IOException Exceção.
     * @throws ClassNotFoundException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    @Override
    public void start() throws IOException, ClassNotFoundException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        String opcao;
        boolean logged = false;
        do {
            view.show();
            opcao = Input.lerString();
            opcao = opcao.toUpperCase();
            switch (opcao) {
                case "1":
                    if (!(logged = login())) {
                        view = new LoginView();
                        break;
                    }
                    User u = model.getLogged();
                    switch (u.getCode().charAt(0)) {
                        case 'u':
                            controladorAuxiliarUtilizador();
                            break;
                        case 'v':
                            controladorAuxiliarVoluntario();
                            break;
                        case 'l':
                            controladorAuxiliarLoja();
                            break;
                        case 't':
                            controladorAuxiliarTransportadora();
                            break;
                    }
                    break;
                case "2":
                    register();
                    view = new LoginView();
                    break;
                case "L":
                    loadFile();
                    break;
                case "S":
                    break;
            }
        } while (!opcao.equals("S") && !logged);
    }

    /*--------------------------------------------------UTILIZADOR--------------------------------------------------*/

    /**
     * Controlador auxiliar relativo ao utilizador.
     * @throws IOException Exceção.
     * @throws ClassNotFoundException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     * @throws ClassNotFoundException Exceção.
     */
    private void controladorAuxiliarUtilizador() throws IOException, ClassNotFoundException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        String opcao = "";
        do {
            this.view = new UtilizadorView();
            view.show();
            opcao = Input.lerString();
            opcao = opcao.toUpperCase();
            switch (opcao) {
                case "1":
                    novaEncomenda();
                    break;
                case "2":
                    encomendasFeitas();
                    view.show("Press Enter to exit");
                    Input.lerString();
                    break;
                case "3":
                    encomendasOnHold();
                    view.show("Press Enter to exit");
                    Input.lerString();
                    break;
                case "4":
                    encomendasToAccept();
                    break;
                case "5":
                    apresentarLojas();
                    view.show("Press Enter to exit");
                    Input.lerString();
                    break;
                case "6":
                    apresentarProdutosLoja();
                    break;
                case "7":
                    classificarEntregador();
                    break;
                case "8":
                    top10user();
                    break;
                case "G":
                    gravar();
                    break;
                case "S":
                    this.model.logout();
                    this.view = new LoginView();
                    start();
                    break;

            }

        } while (!(opcao.equals("S")));
    }

    /**
     * Método que permite efetuar nova encomenda.
     * @throws IOException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     */
    private void novaEncomenda() throws IOException, AlreadyEvaluatedException {
        List<Object> aux = new ArrayList<>();
        String loja, produto;
        int qtd;
        int qtLinhas = 0;
        try {
            view.show("Qual a Loja onde pretende encomendar?\n");
            apresentarLojas();
            loja = Input.lerString();
            aux.add(loja);
            if (loja.isEmpty()) return;
            do {
                view.show("Qual o produto que pretende comprar?\n");
                apresentarProdutosLoja(loja);
                produto = Input.lerString();
                if (produto.isEmpty()) break;
                aux.add(produto);
                do {
                    view.show("Indique a quantidade que pretende comprar: ");
                    qtd = Input.lerInt();
                    if (qtd < 1) view.show("Quantidade inválida\n");
                } while (qtd < 1);
                aux.add(qtd);
                qtLinhas++;
            }while(true);
            if(qtLinhas == 0) return;
            model.interpreta(2, aux);
            view.show("Encomenda efetuada com sucesso\n");
        } catch (UserInexistenteException m) {
            view.show(m.getMessage() + ": User inváldo\n");
        } catch (ProdutoInexistenteException m) {
            view.show(m.getMessage() + ": Produto inválido\n");
        }
    }

    /**
     * Método que apresenta as Lojas do sistema ao utilizador.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void apresentarLojas() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> aux = model.interpreta(5, new ArrayList<>());
        if (aux.size() == 0) view.show("Something's wrong");
        for (Object e : aux) {
            view.show(e + "\n");
        }
    }

    /**
     * Método que apresenta os produtos de uma dada loja ao utilizador.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void apresentarProdutosLoja() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        apresentarLojas();
        List<Object> aux = new ArrayList<>();
        String codLoja;
        view.show("Inserir código da loja que pretende consultar: ");
        codLoja = Input.lerString();
        if (codLoja.isEmpty()) return;
        aux.add(codLoja);
        try {
            Collection<Object> prods = model.interpreta(6, aux);
            if (prods.size() == 0) view.show("A Loja não possui produtos de momento!\n");
            for (Object e : prods)
                view.show(e + "\n");
        } catch (UserInexistenteException | ProdutoInexistenteException m) {
            view.show(m.getMessage() + " -> Código de loja inválido\n");
        }
        view.show("Press Enter to exit");
        Input.lerString();
    }
    /**
     * Método que apresenta os produtos de uma dada loja ao utilizador.
     * @param codLoja codigo de Loja.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void apresentarProdutosLoja(String codLoja) throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        List<Object> aux = new ArrayList<>();
        aux.add(codLoja);
        try {
            Collection<Object> prods = model.interpreta(6, aux);
            if (prods.size() == 0) view.show("A Loja não possui produtos de momento!\n");
            for (Object e : prods)
                view.show(e + "\n");
        } catch (UserInexistenteException | ProdutoInexistenteException m) {
            view.show(m.getMessage() + " -> Código de loja inválido\n");
        }
    }

    /**
     * Método que mostra ao utilizador as encomendas por ele feitas e entregues.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void encomendasFeitas() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> aux = model.interpreta(3, new ArrayList<>());
        if (aux.size() == 0) {
            view.show("Não possui encomendas feitas de momento!\n");
            return;
        } else {
            for (Object e : aux) {
                view.show(e + "\n");
            }
        }
    }

    /**
     * Método que mostra ao utilizador as encomendas à espera de serem entregues.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void encomendasOnHold() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> aux = model.interpreta(4, new ArrayList<>());
        if (aux.size() == 0) {
            view.show("Não possui encomendas feitas de momento!\n");
        } else {
            for (Object e : aux) {
                view.show(e + "\n");
            }
        }
    }

    /**
     * Método que mostra ao utilizador as encomendas que estão à espera de ser aceites.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void encomendasToAccept() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> aux = model.interpreta(17, new ArrayList<>());
        for (Object e : aux) {
            view.show(e + "\n");
        }
        if (aux.isEmpty()) view.show("Não possui encomendas por aceitar de momento.\n");
        else {
            String opcao;
            List<Object> args = new ArrayList<>();
            view.show("Indique o código da encomenda a aceitar/recusar ou pressione Enter para sair\n");
            opcao = Input.lerString();
            if (opcao.isEmpty()) return;
            args.add(opcao);
            view.show("Pretender aceitar(S) ou recusar(N) a encomenda?\n");
            opcao = Input.lerString();
            opcao = opcao.toUpperCase();
            args.add(opcao);
            Collection<Object> res = model.interpreta(18, args);
            if (res.isEmpty()) view.show("A encomenda não foi aceite ou não existe!\n");
            else view.show("Encomenda recebida com sucesso!\n");
        }
        view.show("Press Enter to exit");
        Input.lerString();
    }

    /**
     * Método que permite ao utilizador classificar um entregador.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void classificarEntregador() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> aux = model.interpreta(21, new ArrayList<>());
        List<Object> args = new ArrayList<>();
        String code;
        boolean valid = false;
        for (Object e : aux)
            view.show(e + "\n");
        do {
            view.show("Indique o entregador que pretende avaliar: ");
            code = Input.lerString();
            if (code.isEmpty()) return;
            else if (code.charAt(0) != 'v' && code.charAt(0) != 't')
                view.show("Código inválido\n");
            else {
                args.add(code);
                valid = true;
            }
        } while (!valid);
        view.show("Atribua uma classificação ao entregador ( 0 a 5 ): ");
        args.add(Input.lerInt());
        try {
            model.interpreta(22, args);

        } catch (UserInexistenteException m) {
            view.show(m.getMessage() + " -> Entregador inválido\n");
        } catch (AlreadyEvaluatedException m) {
            view.show(m.getMessage() + "já foi avaliado por si\n");
        }
    }

    /**
     * Método que permite ao utilizador visualizar o top10 de utilizadores em termos de encomendas feitas.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void top10user() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> res = model.interpreta(25, new ArrayList<>());
        view.show("------TOP10 UTILIZADORES------\n");
        for (Object e : res) {
            view.show(e + "\n");
        }
        view.show("-------------------------------\n");
        view.show("Press Enter to exit");
        Input.lerString();
    }

    /*--------------------------------------------------VOLUNTARIO--------------------------------------------------*/

    /**
     * Controlador auxiliar do voluntário.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     * @throws ClassNotFoundException Exceção.
     */
    private void controladorAuxiliarVoluntario() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException, ClassNotFoundException {
        String opcao;
        Voluntario v = (Voluntario) model.getLogged();
        do {
            this.view = new VoluntarioView();
            view.show(v.isDisponivel());
            opcao = Input.lerString();
            opcao = opcao.toUpperCase();
            switch (opcao) {
                case "1":
                    if (!v.isDisponivel()) break;
                    entregarEncomenda();
                    break;
                case "2":
                    consultarSistema();
                    break;
                case "3":
                    alterarDisponibilidade();
                    break;
                case "4":
                    mostrarClassificacao();
                    break;
                case "G":
                    gravar();
                    break;
                case "S":
                    this.model.logout();
                    this.view = new LoginView();
                    start();
                    break;

            }

        } while (!(opcao.equals("S")));
    }

    /**
     * Método que permite ao voluntário entregar uma encomenda.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void entregarEncomenda() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        List<Object> aux = new ArrayList<>();
        String codEnc;
        view.show("Indique o código de encomenda que pretende entregar:\n");
        if ((codEnc = Input.lerString()).isEmpty()) return;
        aux.add(codEnc);
        Collection<Object> res = model.interpreta(8, aux);
        if (res.isEmpty()) view.show("A encomenda não se encontra no Sistema ou não está no seu raio de ação!\n");
        else view.show("Entrega feita com sucesso!\n");
    }

    /**
     * Método que permite ao voluntário consultar as encomendas disponiveis para entregar.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void consultarSistema() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> aux = model.interpreta(7, new ArrayList<>());
        view.show("Encomendas Disponiveis para Entrega\n");
        if (aux.isEmpty()) view.show("-> Não existem encomendas disponiveis para poder entregar!\n");
        for (Object e : aux) {
            view.show(e + "\n");
        }
        view.show("Press Enter to exit");
        Input.lerString();
    }

    /**
     * Método que permite ao voluntário alterar a sua disponibilidade no sistema.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void alterarDisponibilidade() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        List<Object> aux = new ArrayList<>();
        view.show("Pretende mostrar-se disponivel (1) ou indisponível(0)?\n");
        int opcao = Input.lerInt();
        aux.add(opcao);
        model.interpreta(9, aux);
        view.show("Press Enter to exit");
        Input.lerString();
    }

    /**
     * Método que permite ao voluntário visualizar a sua classificação.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void mostrarClassificacao() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> aux = model.interpreta(10, new ArrayList<>());
        double c = 0;
        for (Object e : aux) c = (double) e;
        view.show("A sua classificação é de " + c + " em 5\n");
        view.show("Press Enter to exit\n");
        Input.lerString();
    }


    /*----------------------------------------------------LOJA----------------------------------------------------*/

    /**
     * Controlador auxiliar da loja.
     * @throws IOException
     * @throws UserInexistenteException
     * @throws AlreadyEvaluatedException
     * @throws ProdutoInexistenteException
     * @throws ClassNotFoundException Exceção.
     */
    private void controladorAuxiliarLoja() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException, ClassNotFoundException {
        String opcao;
        do {
            this.view = new LojaView();
            view.show();
            opcao = Input.lerString();
            opcao = opcao.toUpperCase();
            switch (opcao) {
                case "1":
                    verEncomendas();
                    break;
                case "2":
                    consultarStock();
                    break;
                case "3":
                    updateInfo();
                    break;
                case "G":
                    gravar();
                    break;
                case "S":
                    this.model.logout();
                    this.view = new LoginView();
                    start();
                    break;

            }

        } while (!(opcao.equals("S")));
    }

    /**
     * Método que permite à loja ver encomendas que possui à espera para serem entregues.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void verEncomendas() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        view.show("Encomendas prontas a entregar\n");
        Collection<Object> aux = model.interpreta(12, new ArrayList<>());
        for (Object e : aux) {
            view.show(e + "\n");
        }
        view.show("Press Enter to Exit");
        Input.lerString();
    }

    /**
     * Método que permite à loja consultar o seu stock.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void consultarStock() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        view.show("Produtos dispoiníveis na Loja\n");
        view.show("PRODUTO  |  CÓDIGO  |  PREÇO\n");
        Collection<Object> aux = model.interpreta(11, new ArrayList<>());
        for (Object e : aux) {
            view.show(e + "\n");
        }
        view.show("Press Enter to Exit");
        Input.lerString();
    }

    /**
     * Método que permite à loja atualizar informação sobre si.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void updateInfo() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        String opcao;
        do {
            view.show(model.interpreta(27,new ArrayList<>()).size()==1);
            opcao = Input.lerString();
            opcao = opcao.toUpperCase();
            switch (opcao) {
                case "1":
                    model.interpreta(13, new ArrayList<>());
                    break;
                case "2":
                    List<Object> aux = new ArrayList<>();
                    boolean valid = false;
                    String code, nome;
                    double p;
                    do {
                        view.show("Indique o código do produto que pretende adicionar: ");
                        code = Input.lerString();
                        if (code.isEmpty()) return;
                        else if (code.charAt(0) != 'p') {
                            view.show("Código inválido\n");
                        } else valid = true;
                    } while (!valid);
                    aux.add(code);
                    view.show("Indique o nome do produto: ");
                    nome = Input.lerString();
                    if (nome.isEmpty()) return;
                    else aux.add(nome);
                    view.show("Indique o preço a que pretende vender o produto: ");
                    p = Input.lerDouble();
                    aux.add(p);
                    model.interpreta(14, aux);
                    break;
                case "3":
                    List<Object> aux2 = new ArrayList<>();
                    double tempo;
                    view.show("Indique o tempo médio de atendimento: ");
                    tempo = Input.lerDouble();
                    aux2.add(tempo);
                    model.interpreta(28,aux2);
                    break;
                case "S":
                    break;
            }
        } while (!opcao.equals("S"));
    }

    /*--------------------------------------------------EMPRESA--------------------------------------------------*/

    /**
     * Controlador auxiliar da empresa.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     * @throws ClassNotFoundException Exceção.
     */
    private void controladorAuxiliarTransportadora() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException, ClassNotFoundException {
        String opcao = "";
        Transportadora t = (Transportadora) model.getLogged();
        do {
            this.view = new EmpresaView();
            view.show(t.getDisponibilidade());
            opcao = Input.lerString();
            opcao = opcao.toUpperCase();
            switch (opcao) {
                case "1":
                    entregarEncomendaTransportadora();
                    break;
                case "2":
                    consultarSistemaTransportadora();
                    break;
                case "3":
                    alterarDisponibilidadeTransportadora();
                    break;
                case "4":
                    calcularFaturado();
                    break;
                case "5":
                    mostrarClassificacaoTransportadora();
                    break;
                case "6":
                    top10transportadora();
                    break;
                case "G":
                    gravar();
                    break;
                case "S":
                    this.model.logout();
                    this.view = new LoginView();
                    start();
                    break;

            }

        } while (!(opcao.equals("S")));
    }

    /**
     * Método que permite a uma transportadora efetuar uma entrega.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void entregarEncomendaTransportadora() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        List<Object> aux = new ArrayList<>();
        String codEnc;
        view.show("Indique o código de encomenda que pretende entregar:\n");
        if ((codEnc = Input.lerString()).isEmpty()) return;
        aux.add(codEnc);
        Collection<Object> res = model.interpreta(15, aux);
        if (res.isEmpty()) view.show("A encomenda não se encontra no Sistema ou  não está no seu raio de ação.\n");
        else view.show("A entrega encontra-se pendente e à espera de aprovação do Utilizador!\n");
    }

    /**
     * Método que permite à transportadora consultar o sistema e ver encomendas que estejam no seu raio de ação.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void consultarSistemaTransportadora() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> aux = model.interpreta(16, new ArrayList<>());
        view.show("Encomendas Disponiveis para Entrega\n");
        if (aux.isEmpty()) view.show("-> Não existem encomendas disponiveis para poder entregar!\n");
        for (Object e : aux) {
            view.show(e + "\n");
        }
        view.show("Press Enter to exit\n");
        Input.lerString();
    }

    /**
     * Método que permite à transportadora alterar a disponibilidade no sistema.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void alterarDisponibilidadeTransportadora() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        List<Object> aux = new ArrayList<>();
        view.show("Pretende mostrar-se disponivel (1) ou indisponível(0)?\n");
        int opcao = Input.lerInt();
        aux.add(opcao);
        model.interpreta(19, aux);
        view.show("Press Enter to exit");
        Input.lerString();
    }

    /**
     * Método que permite à transportadora ver o total faturado entre duas datas.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void calcularFaturado() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        List<Object> args = new ArrayList<>();
        view.show("Data de Início\n");
        view.show("Ano: ");
        int y1 = Input.lerInt();
        int m1, d1, m2, d2;
        int aux;
        do {
            view.show("Mes: ");
            m1 = Input.lerInt();
            if (m1 < 1 || m1 > 12) System.out.println("Mês inválido, insira um número no intervalo [1:12]");
        } while (m1 < 1 || m1 > 12);
        if (m1 == 2) aux = 28;
        else if (m1 == 1 || m1 == 3 || m1 == 5 || m1 == 7 || m1 == 8 || m1 == 10 || m1 == 12) aux = 31;
        else aux = 30;
        do {
            view.show("Dia: ");
            d1 = Input.lerInt();
            if (d1 < 1 || d1 > aux) System.out.println("Dia inválido, insira um número no intervalo [1:" + aux + "]");
        } while (d1 < 1 || d1 > aux);
        view.show("\nData de Fim\n");
        view.show("Ano: ");
        int y2 = Input.lerInt();
        do {
            view.show("Mes: ");
            m2 = Input.lerInt();
            if (m2 < 1 || m2 > 12) System.out.println("Mês inválido, insira um número no intervalo [1:12]");
        } while (m2 < 1 || m2 > 12);
        if (m2 == 2) aux = 28;
        else if (m2 == 1 || m2 == 3 || m2 == 5 || m2 == 7 || m2 == 8 || m2 == 10 || m2 == 12) aux = 31;
        else aux = 30;
        do {
            view.show("Dia: ");
            d2 = Input.lerInt();
            if (d2 < 1 || d2 > aux) System.out.println("Dia inválido, insira um número no intervalo [1:" + aux + "]");
        } while (d2 < 1 || d2 > aux);
        args.add(y1);
        args.add(m1);
        args.add(d1);
        args.add(y2);
        args.add(m2);
        args.add(d2);
        Collection<Object> res = model.interpreta(20, args);
        view.show("O total faturado entre o dia " + d1 + " do mes " + m1 + " do ano " + y1 + " e o dia " + d2 + " do mes " + m2 + " do ano " + y2 + " é  " + res.toString() + "\n");
    }

    /**
     * Método que permite à transportadora ver a sua classificação.
     * @throws IOException
     * @throws UserInexistenteException
     * @throws AlreadyEvaluatedException
     * @throws ProdutoInexistenteException
     */
    private void mostrarClassificacaoTransportadora() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> aux = model.interpreta(24, new ArrayList<>());
        double c = 0;
        for (Object e : aux) c = (double) e;
        view.show("A sua classificação é de " + c + " em 5\n");
        view.show("Press Enter to exit");
        Input.lerString();
    }

    /**
     * Método que permite à transportadora visualizar o top10 de transportadoras que mais quilómetros percorreram.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void top10transportadora() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        Collection<Object> res = model.interpreta(26, new ArrayList<>());
        view.show("------TOP10 TRANSPORTADORAS------\n");
        for (Object e : res) {
            view.show(e + "\n");
        }
        view.show("--------------------------------\n");

        view.show("Press Enter to exit");
        Input.lerString();
    }


    /*--------------------------------------------------COMMON--------------------------------------------------*/

    /**
     * Método que permite ao User efetuar registro no sistema.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void register() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        view = new WhatUserLoginView();
        boolean valid = false;
        String username, password, nome;
        double latitude, longitude;
        List<Object> aux = new ArrayList<>();
        do {
            view.show("Username: ");
            username = Input.lerString();
            if (username.isEmpty()) return;
            else if (username.charAt(0) != 'u' && username.charAt(0) != 'v' && username.charAt(0) != 't' && username.charAt(0) != 'l') {
                view.show("Username inválido\n");
            } else valid = true;
        } while (!valid);
        aux.add(username); // 0
        view.show("Password: ");
        password = Input.lerString();
        aux.add(password); // 1
        view.show("Indique o seu nome: ");
        nome = Input.lerString();
        aux.add(nome); // 2
        view.show("Indique a sua localização\n");
        view.show("Latitude: ");
        latitude = Input.lerDouble();
        aux.add(latitude); // 3
        view.show("Longitude: ");
        longitude = Input.lerDouble();
        aux.add(longitude); // 4
        switch (username.charAt(0)) {
            case 'v':
                view.show("Tem certificado para entregas médicas?\n");
                view.show("Sim(1) | Não(0)\n");
                aux.add(Input.lerInt() == 1); // 5
                view.show("Indique o seu raio de ação: ");
                aux.add(Input.lerDouble()); // 6
                break;
            case 'l':
                view.show("Pretende informar sobre a fila de espera?\n");
                view.show("Sim(1) | Não(0)\n");
                int info = Input.lerInt();
                aux.add(info == 1); // 5
                if (info == 1) {
                    view.show("Indique o tempo médio de atendimento (em minutos): ");
                    aux.add(Input.lerDouble()); // 6
                } else aux.add(0.0);
                break;
            case 't':
                view.show("Indique o seu NIF: ");
                aux.add(Input.lerString()); // 5
                view.show("Indique o seu raio de ação: ");
                aux.add(Input.lerDouble()); // 6
                view.show("Indique a taxa de transporte: ");
                aux.add(Input.lerDouble()); // 7
                view.show("Indique a capacidade total de encomendas que é capaz de realizar: ");
                aux.add(Input.lerInt()); // 8
                break;
            default:
        }
        model.interpreta(1, aux);
    }

    /**
     * Método que permite ao User efetuar login no sistema.
     * @return True caso o login seja válido, falso em caso contrário.
     */
    private boolean login() {
        boolean valid = false;
        view = new WhatUserLoginView();
        view.show("Username: ");
        String username = Input.lerString();
        view.show("Password: ");
        String pass = Input.lerString();
        try {
            valid = model.checkLoggin(username, pass);
            if (!valid) view.show("Password incorreta!\n");
        } catch (UserInexistenteException m) {
            view.show(m.getMessage() + ": User inexistente\n");
        }
        return valid;
    }

    /**
     * Método que permite carregar um estado de um ficheiro objeto.
     * @throws IOException Exceção.
     * @throws ClassNotFoundException Exceção.
     */
    private void loadFile() throws IOException, ClassNotFoundException {
        boolean valid = false;
        String filename;
        do {
            view.show("Insira o nome do ficheiro objeto: ");
            filename = Input.lerString();
            if (filename.isEmpty()) return;
            else if (Files.exists(Paths.get(filename))) {
                valid = true;
            } else view.show("Ficheiro Objeto Inválido\n");
        } while (!valid);
        this.model = TrazAqui.loadTrazAqui(filename);
    }

    /**
     * Método que permite gravar o estado num ficheiro objeto.
     * @throws IOException Exceção.
     * @throws UserInexistenteException Exceção.
     * @throws AlreadyEvaluatedException Exceção.
     * @throws ProdutoInexistenteException Exceção.
     */
    private void gravar() throws IOException, UserInexistenteException, AlreadyEvaluatedException, ProdutoInexistenteException {
        List<Object> aux = new ArrayList<>();
        String defaultFileName = "trazAqui.dat";
        aux.add(defaultFileName);
        model.interpreta(23, aux);
    }

}
