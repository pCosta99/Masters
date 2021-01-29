package Controller;
import Modelo.*;
import View.*;

import java.io.*;
import java.time.LocalDateTime;
import java.util.*;

public class TrazAquiController implements Serializable {


    /**
     * Controlador principal
     * @param view          Classe TrazAquiView
     * @throws InterruptedException
     * @throws IOException
     * @throws ClassNotFoundException
     */
    public void interpretador(TrazAquiView view) throws InterruptedException, IOException, ClassNotFoundException {
        TrazAquiModel p = new TrazAquiModel();
        view.inicio();
        Scanner s = new Scanner(System.in);
        char letra = s.next().charAt(0);
        if(letra == 'S' || letra == 's'){
            p.parseDefault();
        }
        else {
            try {
                p = p.carregaEstado("TrazAqui.dat");
            } catch (FileNotFoundException e) {
                p.parseDefault();
            }
        }
        int escolha;
        do{
            view.menuInicial();
            Scanner sc = new Scanner(System.in);
            try {
                escolha = sc.nextInt();
                switch (escolha){
                    case 1:
                        inicioSessao(p,view);
                        break;
                    case 2:
                        novaSessao(p,view);
                        break;
                    case 3:
                        registo(p,view);
                }
            }catch (InputMismatchException e) {
                escolha = -1;
                view.opcao();
            }

            /*String a1 = new String(" _____                   _               _ " );
            String a2 = new String("/__   \_ __ __ _ ____   /_\   __ _ _   _(_)");
            System.out.println(a1);
            System.out.println("/__   \_ __ __ _ ____   /_\   __ _ _   _(_)" );
            System.out.println("  / /\/ '__/ _` |_  /  //_\\ / _` | | | | | ");
            System.out.println(" / /  | | | (_| |/ /  /  _  \ (_| | |_| | |" );
            System.out.println(" \/   |_|  \__,_/___| \_/ \_/\__, |\__,_|_|" );
            System.out.println("                                |_|        ");*/

        }while (escolha!=4);
        p.guardaEstado("TrazAqui.dat");
    }

    /**
     * Controlador de inicio de sessão
     * @param p           Classe TrazAquiModel
     * @param view        Classe TrazAquiView
     * @throws InterruptedException
     */
    public void inicioSessao(TrazAquiModel p, TrazAquiView view){

        char letra;
        Scanner sc = new Scanner(System.in);
        String var;
        do{
            view.inicioSessao();
            var = sc.next().toUpperCase();
            letra = var.charAt(0);
            switch (letra) {
                case 'U': {
                    String codigoU = view.sessaoUti();
                    Utilizador u = p.getUtilizador().get(codigoU);
                    if (u == null) {
                        view.errado();
                    } else {
                        view.bemvindo();
                        aplicacao(u, p, view);
                        System.out.println();
                    }
                    break;
                }
                case 'V': {
                    String codigoV = view.sessaoVol();
                    Voluntario v = p.getVoluntario().get(codigoV);
                    if (v == null)
                        view.errado();
                    else {
                        Scanner vo = new Scanner(System.in);
                        view.bemvindo();

                        int num;
                        do {
                            view.estado();
                            try {
                                num = vo.nextInt();
                                if (num == 1) {
                                    view.AreaCliente(v);
                                } else if (num == 2) {
                                    view.media(v.mediaClassificacao());
                                } else if (num == 3) {
                                    view.registo(v);
                                }
                            }catch (InputMismatchException e){
                                view.opcao();
                                break;
                            }
                        } while (num != 4);
                    }
                    break;
                }
                case 'T': {
                    String codigoT = view.sessaoTrans();
                    Transportadora t = p.getTransportador().get(codigoT);
                    if (t == null)
                        view.errado();
                    else {
                        view.bemvindo();
                        if (t instanceof TransportadoraMulti) {
                            sessaoTransMulti(p,view,t);
                        } else {
                           sessaoTrans(view,p,t);
                        }
                    }
                    break;
                }
                case 'L': {
                    String codigoL = view.sessaoLoja();
                    Loja l = p.getLoja().get(codigoL);
                    if (l == null)
                        view.errado();
                    else {
                        view.bemvindo();
                        sessaoLoja(p, l, view);
                    }
                    break;
                }
            }
        }while(letra!='S');
    }

    /**
     * Controlador de criar nova sessão
     * @param p             Classe TrazAquiModel
     * @param view          Classe TrazAquiView
     */
    public void novaSessao(TrazAquiModel p, TrazAquiView view) {
        char letra;
        view.criarSessao();
        Scanner sc = new Scanner(System.in);
        String var = sc.next().toUpperCase();
        letra = var.charAt(0);
        System.out.println();
        switch (letra){
            case 'U': {
                Utilizador u = view.criarUtili(p);
                if(u != null){
                    p.insereUtilizadorMap(u);
                    view.criadaSucesso();
                    aplicacao(u,p,view);
                }
                break;
            }
            case 'V':{
                Voluntario v = view.criarVoluntario(p);
                if(v != null){
                    p.insereVoluntarioMap(v);
                    view.criadaSucesso();
                }
                break;
            }
            case 'T': {
                Transportadora t = view.criaTransportadora(p);
                if(t != null)
                    p.insereTransportadoraMap(t);
                    view.criadaSucesso();
                break;
            }
            case 'L':{
                Loja loja = view.criaLoja(p);
                if(loja != null){
                    p.insereLojaMap(loja);
                    sessaoLoja(p,loja,view);
                    view.criadaSucesso();
                }
                break;
            }
        }
    }

    /**
     * Controlador de gerar uma encomenda
     * @param u             Utilizador com sessão inicicializada
     * @param p             Classe TrazAquiModel
     * @param view          Classe TrazAquiView
     */
    public void aplicacao(Utilizador u, TrazAquiModel p, TrazAquiView view){
        Scanner sc = new Scanner(System.in);
        Random ran = new Random();
        int escolha;
        do {
            view.encomendasEntregues(u);
            view.encomendasPendentes(u);
            view.menuGerarEncomenda();
            try {
                escolha = sc.nextInt();
                switch (escolha) {
                    case 1: {
                        view.encomendaMedica();
                        int enNmedica = sc.nextInt();
                        boolean meds;
                        if (enNmedica == 1) {
                            meds = true;
                        } else if (enNmedica == 2) {
                            meds = false;
                        } else
                            break;
                        String codUtilizador = u.getCodUti();
                        char letra = 'e';
                        int num1 = ran.nextInt(10);
                        int num2 = ran.nextInt(10);
                        int num3 = ran.nextInt(10);
                        int num4 = ran.nextInt(10);
                        StringBuilder s = new StringBuilder();
                        String codEncomenda = s.append(letra).append(num1).append(num2).append(num3).append(num4).toString();
                        String codLoja = escolhaLoja(p.getLoja(), view);
                        Loja loja = p.getLoja().get(codLoja);
                        List<LinhaEncomenda> prod = escolhaProdutos(codLoja, p, view);
                        if (prod == null)
                            view.encomendaCancelada();
                        else {
                            double peso = 0;
                            for (LinhaEncomenda linha : prod) {
                                peso += linha.getPesoU();
                            }
                            double tempo = 0;
                            Encomenda enc = new Encomenda(codEncomenda, codUtilizador, codLoja, peso, false, tempo, meds, prod);
                            loja.adicionaPendente(enc);
                            view.encomendaProcesso();
                        }
                        break;
                    }
                    case 2: {
                        String cEncomenda = view.imprimePendentes(u);
                        if (cEncomenda == null) break;
                        Encomenda enc = u.devolvePendente(cEncomenda);
                        if (enc == null) {
                            view.encomendaPendente();
                            break;
                        }
                        String cLoja = enc.getCodLoja();
                        boolean levantar = view.isLevantar();
                        Voluntario livre = p.voluntariosDisponiveis(u, enc);
                        Loja l = p.getLoja().get(cLoja);
                        if (!levantar) {
                            if (livre == null) {
                                Map<String, Map.Entry<Double, Double>> trans = p.transportadorasCustos(u, enc, l);
                                String codTrans = view.transportadoraDisponivel(trans, p);
                                Transportadora t = p.getTransportador().get(codTrans);
                                if (t != null) {
                                    if (t instanceof TransportadoraMulti) {
                                        ((TransportadoraMulti) t).adicionaPendente(enc);
                                        if (((TransportadoraMulti) t).isCheio()) {
                                            List<Encomenda> pend = ((TransportadoraMulti) t).getPendentes();
                                            for (Encomenda e : pend) {
                                                e.setEntregue(true);
                                                double tempo = t.tempodeEncomenda(e, u, l);
                                                e.setTempo(tempo);
                                                t.adicionaRegisto(e, LocalDateTime.now());
                                                u.incrementaNencomendas();
                                                t.incrementa(u.getGps());
                                            }
                                            ((TransportadoraMulti) t).cheia(p);
                                            ((TransportadoraMulti) t).remove();
                                        }
                                    } else {
                                        enc.setEntregue(true);
                                        p.removeAceite(cEncomenda);
                                        double tempo = t.tempodeEncomenda(enc, u, l);
                                        enc.setTempo(tempo);
                                        t.adicionaRegisto(enc, LocalDateTime.now());
                                        u.incrementaNencomendas();
                                        t.incrementa(u.getGps());
                                        view.encomendaSucesso();
                                    }
                                    int clas = view.classificacaoT();
                                    t.adicionaClassi(clas);
                                } else {
                                    view.transportadoraInvalida();
                                    break;
                                }
                            } else {
                                view.encomendaCaminho(livre);
                                enc.setEntregue(true);
                                p.removeAceite(cEncomenda);
                                livre.adicionaRegisto(enc, LocalDateTime.now());
                                int clas = view.classificacaoV();
                                livre.adicionaClassi(clas);
                                u.incrementaNencomendas();
                                livre.setLivre(false);
                            }
                            u.removePendente(enc);
                            p.removeAceite(cEncomenda);
                        } else {
                            u.removePendente(enc);
                            p.removeAceite(cEncomenda);
                        }
                        l.adicionaRegistoUtilizador(enc);
                        break;
                    }
                    case 3: {
                        String codloja = escolhaLoja(p.getLoja(), view);
                        Loja lo = p.getLoja().get(codloja);
                        Map<String, LinhaEncomenda> cata = lo.getCatalogo();
                        view.catalogoProdutos(cata);
                        break;
                    }
                    case 4: {
                        Scanner area = new Scanner(System.in);
                        int areaEscolha;
                        do {
                            view.areaCliente();
                            areaEscolha = area.nextInt();
                            if (areaEscolha == 1) {
                                view.perfil(u);
                            }
                            if (areaEscolha == 2) {
                                view.editarPerfil(u);
                            }
                            if (areaEscolha == 3) {
                                view.pedidos();
                                int transporte = area.nextInt();
                                if (transporte == 1) {
                                    List<Transportadora> tras = new ArrayList<>();
                                    for (Transportadora t : p.getTransportador().values()) {
                                        Transportadora transportadora = t.transportUtili(u);
                                        if (transportadora != null)
                                            tras.add(transportadora);
                                    }
                                    if (tras.size() == 0)
                                        view.encomendaVaziaTrans();
                                    for (Transportadora t : tras) {
                                        List<Encomenda> enco = t.ordenaRegisto(u);
                                        view.listaEncomendasTransp(t, enco);
                                    }
                                } else if (transporte == 2) {
                                    List<Voluntario> vol = new ArrayList<>();
                                    for (Voluntario v : p.getVoluntario().values()) {
                                        Voluntario voluntario = v.transportUtili(u);
                                        if (voluntario != null)
                                            vol.add(voluntario);
                                    }
                                    if (vol.size() == 0)
                                        view.encomendaVaziaVol();
                                    for (Voluntario v : vol) {
                                        List<Encomenda> enco = v.ordenaRegistVoluntario(u);
                                        view.listaEncomendasVol(v, enco);
                                    }
                                }
                            }
                        } while (areaEscolha != 4);
                        break;
                    }
                }
            }
            catch (InputMismatchException e){
                view.opcao();
                break;
            }
        }while(escolha!=5);
    }

    /**
     * Escolha da loja desejada
     * @param loja            Map de Lojas
     * @param view            Classe TrazAquiView
     * @return String         Codigo da  loja
     */
    public String escolhaLoja(Map<String, Loja> loja, TrazAquiView view){
        view.produtosLoja(loja);
        Scanner sc = new Scanner(System.in);
        return sc.next();
    }


    /**
     * Escolha do produto desejado
     * @param l                         Codigo da loja
     * @param p                         Classe TrazAquiModel
     * @param view                      Classe TrazAquiView
     * @return List<LinhaEncomenda>     Lista de Linhas de encomendas
     */
    public List<LinhaEncomenda> escolhaProdutos(String l, TrazAquiModel p, TrazAquiView view) {
        Loja loja = p.getLoja().get(l);
        Map<String, LinhaEncomenda> cat = loja.getCatalogo();
        if(cat.size() == 0){
            view.listaVazia();
            return null;
        }
        else{
            view.catalogoProdutos(cat);
            return view.listaProdutosEscolhida(cat);
        }

    }

    /**
     * Sessão da loja
     * @param p                   Classe TrazAquiModel
     * @param l                   Loja com a sessão iniciada
     * @param view                Classe TrazAquiView
     */
    public void sessaoLoja(TrazAquiModel p, Loja l, TrazAquiView view){
        Loja atual = p.getLoja().get(l.getCodLoja());
        view.encomendasPendentes(atual);
        view.inserirCatalogo(atual,p);
    }

    /**
     * Sessão de transportadora Multi
     * @param p                 Classe TrazAquiModel
     * @param view              Classe TrazAquiView
     * @param t                 Transportadora Multi
     */
    public void sessaoTransMulti(TrazAquiModel p, TrazAquiView view, Transportadora t){
        Scanner sc = new Scanner(System.in);
        int num;
        do {
            view.menuTranspMulti();
            try {
                num = sc.nextInt();
                if (num == 1) {
                    view.areaClienteT(t);
                } else if (num == 2) {
                    view.media(t.mediaClassificacao());
                } else if (num == 3) {
                    view.registoT(t);
                } else if (num == 4) {
                    Map.Entry<LocalDateTime, LocalDateTime> tempo = view.dadosFaturacao();
                    double faturacao = t.faturacaoDadoTempo(tempo.getKey(), tempo.getValue(), p);
                    view.faturacao(tempo.getKey(), tempo.getValue(), faturacao);
                } else if (num == 5) {
                    view.encomendasPorEntregar((TransportadoraMulti) t);
                }
            }catch (InputMismatchException e){
                view.opcao();
                break;
            }
        } while (num != 6);
    }

    /**
     * Sessão de transportadora normal
     * @param view              Classe TrazAquiView
     * @param p                 Classe TrazAquiModel
     * @param t                 Transportadora Normal
     */
    public void sessaoTrans(TrazAquiView view, TrazAquiModel p, Transportadora t){
        int num;
        Scanner sc = new Scanner(System.in);
        do {
            view.menuTransp();
            try {
                num = sc.nextInt();
                if (num == 1) {
                    view.areaClienteT(t);
                } else if (num == 2) {
                    view.media(t.mediaClassificacao());
                } else if (num == 3) {
                    view.registoT(t);
                } else if (num == 4) {
                    Map.Entry<LocalDateTime, LocalDateTime> tempo = view.dadosFaturacao();
                    double faturacao = t.faturacaoDadoTempo(tempo.getKey(), tempo.getValue(), p);
                    view.faturacao(tempo.getKey(), tempo.getValue(), faturacao);
                }
            }
            catch (InputMismatchException e){
                view.opcao();
                break;
            }
        } while (num != 5);
    }

    /**
     * Informações da aplicação
     * @param p               Classe TrazAquiModel
     * @param view            Classe TrazAquiView
     */
    public void registo(TrazAquiModel p, TrazAquiView view){
        int escolha;
        do{
            view.registoMenu();
            Scanner sc = new Scanner(System.in);
            escolha = sc.nextInt();
            switch (escolha){
                case 1:{
                    List<Utilizador> util = p.dezUtilizador();
                    view.utilizadores(util);
                    break;
                }
                case 2:{
                    List<Transportadora> tran = p.dezTransportadoras();
                    view.transporta(tran);
                    break;
                }
            }

        }while(escolha!=3);
    }


}
