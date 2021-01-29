package View;

import Controller.Controller;
import Helpers.IPair;
import Helpers.ITriplet;
import Helpers.Pair;
import Helpers.Triplet;
import Model.*;

import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static View.InputHelper.TYPE.STRING;
import static View.InputHelper.*;
import static View.ViewHelper.*;

public class View implements IView {

    private List<IPair<Integer, String>> menuQueries;
    private Controller controller;
    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> currentOptionSet;
    private int currentOption;
    private List<String> info; // nome do user, loja, etc...


    public View(Controller c) {
        this.controller = c;
        this.menuQueries = new ArrayList<>();
        this.currentOptionSet = new LinkedHashMap<>();
        this.info = new ArrayList<>(Collections.nCopies(10,""));
    }


    public void showFinished() {
        Window window = makeWindow(true,"TERMINADO");
        window.addBlock(new Block(Arrays.asList(newLine("Concluído!!").colorBG(GREEN_BOLD),newLine("Pressione ENTER para continuar")),1,1,1,1));
        this.showWindow(window);
    }

    public void showSaving() {
        Window window = makeWindow(true,"GUARDAR FICHEIRO");
        window.addBlock(new Block(Collections.singletonList(newLine("A guardar, aguarde...").colorBG(BLINK+GREEN_BOLD)),1,1,1,1));
        this.showWindow(window);
    }

    public void showReading() {
        Window window = makeWindow(true,"LER FICHEIRO");
        window.addBlock(new Block(Collections.singletonList(newLine("A carregar, aguarde...").colorBG(BLINK+GREEN_BOLD)),1,1,1,1));
        this.showWindow(window);
    }

    //FIXME - clones
    public View(ArrayList<IPair<Integer, String>> queriesInfo) {
        this.menuQueries = queriesInfo;
    }

    public View() {
        this.menuQueries = new ArrayList<>();
    }


    public View(View outraView) {
        this.menuQueries = outraView.getMenuQueries();
    }

    public List<IPair<Integer, String>> getMenuQueries() {
        return this.menuQueries.stream().map(IPair::clone).collect(Collectors.toList());
    }


    public void setCurrentOptionSet(Map<Integer, ITriplet<String, InputSequence, Function<List<Object>, Object>>> currentOptionSet) {
        this.currentOptionSet = currentOptionSet;
    }



    @Override
    public void setMenuQueries(ArrayList<IPair<Integer, String>> menuQueries) {
        this.menuQueries = new ArrayList<>(menuQueries);
    }

    @Override
    public void showWindow(Window w) {
        ViewHelper.clearScreen();
        w.render();
        w.display();
    }

    @Override
    public void printStringError(String line) {
        System.out.println(RED+line+RESET);
    }

    @Override
    public void printStringNormal(String line) {
        System.out.println(line);
    }

    @Override
    public void printStringBright(String line) {
        System.out.println(GREEN+line+RESET);
    }

    @Override
    public void printInputLine() {
        System.out.print("> ");
    }


    private Panel menuPanel(Map<Integer,String> menu) {
        Block b = new Block();
        b.addLine(new Line(" Escolha uma opção            ").colorBG(BLACK+WHITE_BACKGROUND));
        for(Map.Entry<Integer,String> en : menu.entrySet()) {
            if(en.getKey() >= 0)  b.addLine(new Line("│ " + en.getKey() + " - " + en.getValue()));
            else b.addLine(new Line("│ "));
        }
        b.setMargins(Arrays.asList(1,1,1,1));
        return new Panel(b);
    }

    private Panel menuPanelSelected(Map<Integer,String> menu, int selection) {
        Block b = new Block();
        b.addLine(new Line(" Escolha uma opção            ").colorBG(BLACK+WHITE_BACKGROUND));
        for(Map.Entry<Integer,String> en : menu.entrySet()) {
            if(en.getKey() >= 0) {
                Line line = new Line("│ " + en.getKey() + " - " + en.getValue());
                if(en.getKey() == selection) {
                    line = line.colorBG(BLUE_BACKGROUND);
                }
                b.addLine(line);
            }
            else b.addLine(new Line("│ "));

        }
        b.setMargins(Arrays.asList(1,20,1,1));
        return new Panel(b);
    }

    public Window menuWindow(boolean banner, String title, Map<Integer,String> menu ) {
        Window w = makeWindow(banner,title);
        w.addPanel(menuPanel(menu));
        return w;
    }

    private Block gatherInputCommands() {
        Block b = new Block();
        b.addLine(new Line(" COMANDOS: X - Cancelar Querie | B - Voltar atrás ").color(0,BLUE_BOLD));
        return b;
    }


    public Window menuWindowSelected(boolean banner, String title, Map<Integer,String> menu, int selection ) {
        Window w = makeWindow(banner,title);
        w.addPanel(menuPanelSelected(menu, selection));
        return w;
    }


    public Window makeWindow(boolean banner, String title) {
        Window w = new Window();
        if(banner)w.addBanner();
        if(title != null) w.addTitle(title);
        return w;
    }



    private Window inputLoginWindow(int selected, InputSequence seq) {
        Panel p = this.menuPanelSelected(optionsToInfo(loginOptions()), selected);
        p.add(inputBlock(seq,33,"LOGIN",Arrays.asList("Identificação:","Palavra-passe:")));
        p.setSeparator(0," ");
        Window w = makeWindow(true,"OPÇÕES DE LOGIN");
        w.addPanel(p);
        w.addPanel(new Panel(gatherInputCommands()));
        return w;
    }




    public void execOption(int selected) {
        InputSequence seq = this.currentOptionSet.get(selected).getSecond();
        while(seq.active && !seq.ended) {
            seq.nextCommand(this,inputLoginWindow(selected,seq));
        }
        if (seq.ended && seq.active && currentOptionSet.get(selected).getThird() != null ) {
            seq.exec(currentOptionSet.get(selected).getThird());
        }
    }

    //Menu das opções de login
    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> loginOptions() {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        map.put(1,new Triplet<>("Cliente", loginInputSeq("Cliente",this.controller), validateLogin));
        map.put(2,new Triplet<>("Loja", loginInputSeq("Loja",this.controller), validateLogin));
        map.put(3,new Triplet<>("Transportadora", loginInputSeq("Transportadora",this.controller), validateLogin));
        map.put(4,new Triplet<>("Voluntário", loginInputSeq("Voluntário",this.controller), validateLogin));
        map.put(-1,new Triplet<>("", null, null));
        map.put(5,new Triplet<>("Registar", new InputSequence(), registEntity));
        map.put(6,new Triplet<>("Guardar Estado",
                new InputSequence(Collections.singletonList(new AbstractMap.SimpleEntry<>(TYPE.STRING, fileName()))), Save));
        map.put(7,new Triplet<>("Carregar Estado",
                new InputSequence(Collections.singletonList(new AbstractMap.SimpleEntry<>(TYPE.STRING, fileName()))), Load));
        map.put(0,new Triplet<>("Sair", new InputSequence(), exitApp));
        map.put(10,new Triplet<>("ADMIN",new InputSequence() , printSystem));
        return map;
    }

    public Function<List<Object>,Object> Save = (args) -> {
        try {
            this.showSaving();
            this.controller.save((String)args.get(0));
            this.showFinished();
            Input.readAny();
        } catch (IOException e) {
            System.out.println(e);
            this.printStringError("Erro ao gravar, pressione ENTER para continuar");
            Input.readAny();
        }
        ITriplet<String,Boolean,List<Double>> info = new Triplet<>("Other",true,Arrays.asList(0.0));
        return new Pair<>(info,null);
    };


    public Function<List<Object>,Object> Load = (args) -> {
        try {
            this.showReading();
            this.controller.read((String)args.get(0));
            this.showFinished();
            Input.readAny();
        } catch (IOException | ClassNotFoundException e) {
            this.printStringError("Erro ao carregar, pressione ENTER para continuar");
            Input.readAny();
        }
        ITriplet<String,Boolean,List<Double>> info = new Triplet<>("Other",true,Arrays.asList(0.0));
        return new Pair<>(info,null);
    };

    public Function<List<Object>,Object> printSystem = (args) -> {
        this.controller.printSystem();
        return null;
    };

    public Function<List<Object>,Object> exitApp = (args) -> {
        System.exit(0);
        return null;
    };


    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> userOptions() {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        map.put(1,new Triplet<>("Fazer Encomenda", new InputSequence(), makingOrder));
        map.put(2,new Triplet<>("Ver notificações", new InputSequence(), seeNotif));
        map.put(3,new Triplet<>("Informações de Entrega", new InputSequence(), null));
        map.put(4,new Triplet<>("Definições", new InputSequence(), null));
        map.put(-1,new Triplet<>("", null, null));
        map.put(0,new Triplet<>("LOGOUT", null, null));
        return map;
    }


    //Menu das encomendas em raio
    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> notifs(Utilizador util) {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        Pair<Encomenda,Pair<String,Double>> notif = util.getNotification();
        if(notif != null) {
            map.put(1,new Triplet<>(notif.toString(),new InputSequence(),acceptTransp));
        }
        map.put(-1,new Triplet<>("", null, null));
        map.put(0,new Triplet<>("Voltar atrás", null, null));
        return map;
    }

    public Function<List<Object>,Object> acceptTransp = (args) -> {
        Utilizador u = this.controller.getUtilizador(this.info.get(0));
        u.acceptTransp();
        return null;
    };

    public Function<List<Object>,Object> seeNotif = (args) -> {
        Utilizador u = this.controller.getUtilizador(this.info.get(0));
        int option = -100;
        setCurrentOptionSet(notifs(u));
        IPair<Integer,Integer> ret;
        while (option != -1) {
            option = -100;
            Window w = this.menuWindowSelected(true,"MENU PRINCIPAL",optionsToInfo(this.currentOptionSet),option);
            ret = Input.readInt(isMenuOption(notifs(u)),this,w,false);
            option = ret.getSecond();
            if(ret.getFirst()>=0) {
                if(option != 0) {
                    this.currentOption = option;
                    execOption(option);
                } else option = -1;
            } else {
                option = -1;
            }
            setCurrentOptionSet(notifs(u));
        }
        return null;
    };

    private void runUser(Utilizador user) {
        this.info.set(0,user.getCodUtilizador());
        int option = -100;
        setCurrentOptionSet(userOptions());
        IPair<Integer,Integer> ret;
        while (option != -1) {
            option = -100;
            Window w = this.menuWindowSelected(true,"MENU PRINCIPAL",optionsToInfo(this.currentOptionSet),option);
            ret = Input.readInt(isMenuOption(userOptions()),this,w,false);
            option = ret.getSecond();
            if(ret.getFirst()>=0) {
                if(option != 0) {
                    this.currentOption = option;
                    execOption(option);
                } else option = -1;
            } else {
                option = -1;
            }
            setCurrentOptionSet(userOptions());
        }
    }

    //Quando é selecionado fazer encomenda
    public Function<List<Object>,Object> makingOrder = (args) -> {
        this.runShowLojas();
        return null;
    };

    //Quando uma loja é selecionada
    public Function<List<Object>,Object> showStoreProds = (args) -> {
        System.out.println();
        Loja loja = this.controller.getLoja(this.currentOption-1);
        Table table = new Table(Arrays.asList("Código","Descrição","Quantidade","Preço"));
        loja.getProdutos().entrySet().forEach(en -> table.addLine(
                Arrays.asList( en.getKey(),
                        en.getValue().getFirst(),
                        Double.toString(en.getValue().getSecond()),
                        Double.toString(en.getValue().getThird())
                ))
        );
        this.runShowStoreProds(table,loja);
        return null;
    };

    private void runShowLojas() {
        int option = -100;
        setCurrentOptionSet(storeOptions(this.controller));
        IPair<Integer,Integer> ret;
        while (option != -1) {
            option = -100;
            Window w = this.menuWindowSelected(true,"LOJAS DISPONÍVEIS",optionsToInfo(this.currentOptionSet),option);
            ret = Input.readInt(isMenuOption(storeOptions(this.controller)),this,w,false);
            option = ret.getSecond();
            if(ret.getFirst()>=0) {
                if(option != 0) {
                    this.currentOption = option;
                    execOption(option);
                } else option = -1;

            } else {
                option = -1;
            }
            setCurrentOptionSet(storeOptions(this.controller));
        }
    }

    private void runShowStoreProds(Table t, Loja loja) {
        this.info.set(1,loja.getCodLoja());

        Map<String, Triplet<String, Double, Double>> produtos = loja.getProdutos();
        InputSequenceEnc encSeq = new InputSequenceEnc();
        encSeq.setProdutos(produtos);
        String title = "TABELA";
        int option = 0;
        Window window = this.table(t,title);
        while (option != -1){
            option = Input.readOption(encDefaultCommands(),this,window);
            if(option == 1) {
                encSeq.addProdCommand();
                //FIXME
                List<Block> blocks = new ArrayList<>();
                Window w = makeWindow(true,title);
                blocks.add(instructionBlockTable());
                blocks.add(tableToBlock(t));
                blocks.add(inputBlockEnc(encSeq,33,"INPUT", encSeq.labels));
                Panel p = new Panel(blocks);
                w.addPanel(p);


                while(encSeq.active && !encSeq.ended) {
                    blocks = new ArrayList<>();
                    w = makeWindow(true,title);
                    blocks.add(instructionBlockTable());
                    blocks.add(tableToBlock(t));
                    blocks.add(inputBlockEnc(encSeq,33,"INPUT", encSeq.labels));
                    p = new Panel(blocks);
                    w.addPanel(p);
                    encSeq.nextCommand(this,w);
                }

            } else if (option == 2) {
                Encomenda enc = makeEncomenda(loja, this.info.get(0),this.info.get(1),encSeq.getArgs());
                this.controller.requestEncomeda(enc);
            }
        }
    }




    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> lojaOptions() {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        map.put(1,new Triplet<>("Gerir encomendas em Espera", new InputSequence(), checkWaiting));
        map.put(2,new Triplet<>("Informações de Entrega", new InputSequence(), null));
        map.put(3,new Triplet<>("Definições", new InputSequence(), null));
        map.put(-1,new Triplet<>("", null, null));
        map.put(0,new Triplet<>("LOGOUT", null, null));
        return map;
    }

    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> TranspOptions() {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        map.put(2,new Triplet<>("Informações de Entrega", new InputSequence(), null));
        map.put(3,new Triplet<>("Definições", new InputSequence(), null));
        map.put(-1,new Triplet<>("", null, null));
        map.put(0,new Triplet<>("LOGOUT", null, null));
        return map;
    }

    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> VolOptions() {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        map.put(1,new Triplet<>("Transportar Encomenda", new InputSequence(), showEncsEmRaio));
        map.put(2,new Triplet<>("Informações de Entrega", new InputSequence(), null));
        map.put(3,new Triplet<>("Definições", new InputSequence(), null));
        map.put(-1,new Triplet<>("", null, null));
        map.put(0,new Triplet<>("LOGOUT", null, null));
        return map;
    }
    public Function<List<Object>,Object> showEncsEmRaio = (args) -> {
        Voluntario vol = this.controller.getVoluntario(this.info.get(0));
        runShowEnsEmRaio(vol);
        return null;
    };

    //Menu das encomendas em raio
    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> encsEmRaio(Voluntario vol) {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        List<Encomenda> encs = this.controller.encsEmRaioSorted(vol);
        for(int i = 0; i < encs.size(); i++) {
            map.put(i+1,new Triplet<>(encs.get(i).toString(),new InputSequence(),transportarEncomenda));
        }
        map.put(-1,new Triplet<>("", null, null));
        map.put(0,new Triplet<>("Voltar atrás", null, null));
        return map;
    }

    private void runShowEnsEmRaio(Voluntario vol) {
        int option = -100;
        setCurrentOptionSet(encsEmRaio(vol));
        IPair<Integer,Integer> ret;
        while (option != -1) {
            option = -100;
            Window w = this.menuWindowSelected(true,"ENCOMENDAS EM RAIO",optionsToInfo(this.currentOptionSet),option);
            ret = Input.readInt(isMenuOption(encsEmRaio(vol)),this,w,false);
            option = ret.getSecond();
            if(ret.getFirst()>=0) {
                if(option != 0) {
                    this.currentOption = option;
                    execOption(option);
                } else option = -1;

            } else {
                option = -1;
            }
            setCurrentOptionSet(encsEmRaio(vol));
        }
    }

    //Quando é selecionado fazer encomenda
    public Function<List<Object>,Object> transportarEncomenda = (args) -> {
        Voluntario vol = this.controller.getVoluntario(this.info.get(0));
        this.info.set(1,this.controller.encsEmRaioSorted(vol).get(this.currentOption-1).getCodEncomenda());
        this.controller.transportByVol(vol.getCodVoluntario(),this.info.get(1));
        return null;
    };



    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> lojaProdsOptions() {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        List<String> lojas = controller.getLojasNames();
        for(int i = 0; i <  lojas.size(); i++) {
            map.put(i+1,new Triplet<>(lojas.get(i),new InputSequence(),null));
        }
        return map;
    }

    //Menu das lojas
    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> storeOptions(Controller controller) {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        List<String> lojas = controller.getLojasNames();
        for(int i = 0; i <  lojas.size(); i++) {
            map.put(i+1,new Triplet<>(lojas.get(i),new InputSequence(),showStoreProds));
        }
        map.put(-1,new Triplet<>("", null, null));
        map.put(0,new Triplet<>("Voltar atrás", null, null));
        return map;
    }


    //Menu das lojas
    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> encEsperaOptions(Loja loja) {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        List<Encomenda> encs = loja.getEncsEmEspera();
        for(int i = 0; i <  encs.size(); i++) {
            map.put(i+1,new Triplet<>(encs.get(i).toString(),new InputSequence(),aceitarEmEspera));
        }
        map.put(-1,new Triplet<>("", null, null));
        map.put(0,new Triplet<>("Voltar atrás", null, null));
        return map;
    }



    private void runLoja(Loja loja) {
        this.info.set(0,loja.getCodLoja());
        int option = -100;
        setCurrentOptionSet(lojaOptions());
        IPair<Integer,Integer> ret;
        while (option != -1) {
            option = -100;
            Window w = this.menuWindowSelected(true,"MENU PRINCIPAL",optionsToInfo(this.currentOptionSet),option);
            ret = Input.readInt(isMenuOption(lojaOptions()),this,w,false);
            option = ret.getSecond();
            if(ret.getFirst()>=0) {
                if(option != 0) {
                    this.currentOption = option;
                    execOption(option);
                } else option = -1;
            } else {
                option = -1;
            }
            setCurrentOptionSet(lojaOptions());
        }
    }

    private void runTransportadora(Transportadora transp) {
        this.info.set(0,transp.getCodEmpresa());
        int option = -100;
        setCurrentOptionSet(TranspOptions());
        IPair<Integer,Integer> ret;
        while (option != -1) {
            option = -100;
            Window w = this.menuWindowSelected(true,"MENU PRINCIPAL",optionsToInfo(this.currentOptionSet),option);
            ret = Input.readInt(isMenuOption(TranspOptions()),this,w,false);
            option = ret.getSecond();
            if(ret.getFirst()>=0) {
                if(option != 0) {
                    this.currentOption = option;
                    execOption(option);
                } else option = -1;
            } else {
                option = -1;
            }
            setCurrentOptionSet(TranspOptions());
        }
    }

    private void runVol(Voluntario vol) {
        this.info.set(0,vol.getCodVoluntario());
        int option = -100;
        setCurrentOptionSet(VolOptions());
        IPair<Integer,Integer> ret;
        while (option != -1) {
            option = -100;
            Window w = this.menuWindowSelected(true,"MENU PRINCIPAL",optionsToInfo(this.currentOptionSet),option);
            ret = Input.readInt(isMenuOption(VolOptions()),this,w,false);
            option = ret.getSecond();
            if(ret.getFirst()>=0) {
                if(option != 0) {
                    this.currentOption = option;
                    execOption(option);
                } else option = -1;
            } else {
                option = -1;
            }
            setCurrentOptionSet(VolOptions());
        }
    }


    public void runLogin() {
        int option = -100;
        setCurrentOptionSet(loginOptions());
        IPair<Integer,Integer> ret;
        while (option != -1) {
            option = -100;
            Window w = this.menuWindowSelected(true,"OPÇÕES DE LOGIN",optionsToInfo(this.currentOptionSet),option);
            ret = Input.readInt(isMenuOption(loginOptions()),this,w,false);
            option = ret.getSecond();
            if(ret.getFirst()>=0) {
                this.currentOption = option;
                execOption(option);
            } else {
                option = -1;
            }
            setCurrentOptionSet(loginOptions());
        }
    }




    private Encomenda makeEncomenda(Loja loja, String codUser, String codLoja,  List<Object> args) {
        List<LinhaEncomenda> lsEnc = new ArrayList<>();
        for(int i = 0; i<args.size(); i+=2) {
            String codP = (String)args.get(i);
            Double quant = Double.parseDouble((String)args.get(i+1));
            String desc = loja.getProdutos().get(codP).getFirst();
            Double vUnit = loja.getProdutos().get(codP).getThird();
            lsEnc.add(new LinhaEncomenda(codP,desc,quant,vUnit));
        }
        //FIXME peso aleatório
        return new Encomenda(this.controller.nextEncomedaID(),codUser,codLoja,loja.getGps(),this.controller.getUtilizador(codUser).getGps(),10,lsEnc);

    }

    //seq.nextCommand(this,inputLoginWindow(selected,seq));



    public Function<List<Object>,Object> validateLogin = (args) -> {
        Character typeUserNow = 'u';
        if(this.currentOption == 1) typeUserNow = 'u';
        if(this.currentOption == 2) typeUserNow = 'l';
        if(this.currentOption == 3) typeUserNow = 't';
        if(this.currentOption == 4) typeUserNow = 'v';
        if( controller.getSystemUser(typeUserNow,(String) args.get(0) ).checkPassword((String)args.get(1)) ) {
            if(typeUserNow == 'u') this.runUser((Utilizador) controller.getSystemUser(typeUserNow,(String) args.get(0)));
            else if(typeUserNow == 'l') this.runLoja((Loja) controller.getSystemUser(typeUserNow,(String) args.get(0)));
            else if(typeUserNow == 't') this.runTransportadora((Transportadora) controller.getSystemUser(typeUserNow,(String) args.get(0)));
            else this.runVol((Voluntario) controller.getSystemUser(typeUserNow,(String) args.get(0)));
        } else {
            System.out.println("NÃO DEU...."); //FIXME
        }
        return null;
    };





    public Function<List<Object>,Object> checkWaiting = (args) -> {
        Loja loja = this.controller.getLoja(this.info.get(0));
        int option = -100;
        setCurrentOptionSet(encEsperaOptions(loja));
        IPair<Integer,Integer> ret;
        while (option != -1) {
            option = -100;
            Window w = this.menuWindowSelected(true,"ENCOMENDAS EM ESPERA",optionsToInfo(this.currentOptionSet),option);
            ret = Input.readInt(isMenuOption(encEsperaOptions(loja)),this,w,false);
            option = ret.getSecond();
            if(ret.getFirst()>=0) {
                if(option != 0) {
                    this.currentOption = option;
                    execOption(option);
                } else option = -1;
            } else {
                option = -1;
            }
            setCurrentOptionSet(encEsperaOptions(loja));
        }
        return null;
    };



    //Quando é selecionado fazer encomenda
    public Function<List<Object>,Object> aceitarEmEspera = (args) -> {
        Loja loja = this.controller.getLoja(this.info.get(0));
        Encomenda enc = loja.getEncsEmEspera().get(this.currentOption-1);
        Window w = makeWindow(true,"Encomenda");
        Block b = new Block(Arrays.asList(newLine("Aceitar? (S/N)")));
        w.addPanel(new Panel(b));
        this.showWindow(w);
        String s = Input.lerString();
        if(Character.toUpperCase(s.charAt(0))=='S') loja.aceitarEncomeda(enc);
        return null;
    };


    private Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> entityOptions() {
        Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> map = new LinkedHashMap<>();
        map.put(1,new Triplet<>("Cliente",
                new InputSequence(
                    Arrays.asList(
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny())
                    )
                )
                , registarUser));
        map.put(2,new Triplet<>("Loja",
                new InputSequence(
                    Arrays.asList(
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                            new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny())
                    )
                ), registarLoja));
        map.put(3,new Triplet<>("Transportadora", new InputSequence(
                Arrays.asList(
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny())
                )
        ), registarTransp));
        map.put(4,new Triplet<>("Voluntário", new InputSequence(
                Arrays.asList(
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny()),
                        new AbstractMap.SimpleEntry<>(TYPE.STRING, isAny())
                )
        ), registarVol));
        map.put(-1,new Triplet<>("", null, null));
        map.put(0,new Triplet<>("Sair", new InputSequence(), exitApp));
        return map;
    }


    public Function<List<Object>,Object> registEntity = (args) -> {
        int option = -100;
        setCurrentOptionSet(entityOptions());
        IPair<Integer,Integer> ret;
        while (option != -1) {
            option = -100;
            Window w = this.menuWindowSelected(true,"REGISTAR",optionsToInfo(this.currentOptionSet),option);
            ret = Input.readInt(isMenuOption(entityOptions()),this,w,false);
            option = ret.getSecond();
            if(ret.getFirst()>=0) {
                if(option != 0) {
                    this.currentOption = option;
                    execOptionRegistar(option);
                } else option = -1;
            } else {
                option = -1;
            }
            setCurrentOptionSet(entityOptions());
        }
        return null;
    };

    public Function<List<Object>,Object> registarUser = (args) -> {
        Utilizador u = new Utilizador(this.controller.nextUtilizadorID(),(String) args.get(2),new GPS(
                Double.parseDouble((String)args.get(3)),
                Double.parseDouble((String)args.get(4))),
                Arrays.asList(this.controller.getSistema()),(String)args.get(0),(String) args.get(1));
        this.controller.addUtilizador(u);
        return null;
    };

    public Function<List<Object>,Object> registarTransp = (args) -> {
        Transportadora t = new Transportadora(this.controller.nextTranspID(),(String) args.get(2),new GPS(
                Double.parseDouble((String)args.get(3)),
                Double.parseDouble((String)args.get(4))),
                Double.parseDouble((String)args.get(5)),
                (String)args.get(6),
                Double.parseDouble((String)args.get(7)),
                Double.parseDouble((String)args.get(8)),
                Double.parseDouble((String)args.get(9)),
                (String)args.get(0),(String) args.get(1));
        this.controller.addTransp(t);
        return null;
    };

    public Function<List<Object>,Object> registarLoja = (args) -> {
        Loja l = new Loja(this.controller.nextLojaId(),(String) args.get(2),new GPS(
                Double.parseDouble((String)args.get(3)),
                Double.parseDouble((String)args.get(4))),
                Double.parseDouble((String)args.get(5)),
                Arrays.asList(this.controller.getSistema()),(String)args.get(0),(String) args.get(1));
        this.controller.addLoja(l);
        return null;
    };

    public Function<List<Object>,Object> registarVol = (args) -> {
        Voluntario vol = new Voluntario(this.controller.nextVolID(),(String) args.get(2),new GPS(
                Double.parseDouble((String)args.get(3)),
                Double.parseDouble((String)args.get(4))),
                Double.parseDouble((String)args.get(5)),
                Double.parseDouble((String)args.get(6)),
                (String)args.get(0),(String) args.get(1));
        this.controller.addVol(vol);
        return null;
    };

    private Window inputLoginWindowRegistar(int selected, InputSequence seq, List<String> labels) {
        Panel p = this.menuPanelSelected(optionsToInfo(loginOptions()), selected);
        p.add(inputBlock(seq,33,"LOGIN",labels));
        p.setSeparator(0," ");
        Window w = makeWindow(true,"OPÇÕES DE LOGIN");
        w.addPanel(p);
        w.addPanel(new Panel(gatherInputCommands()));
        return w;
    }

    List<String> userLabels() {
        return Arrays.asList("ID para login","Palavra-passe","Nome","Coordenada X","Coordenada Y");
    }
    List<String> volLabels() {
        return Arrays.asList("ID para login","Palavra-passe","Nome","Coordenada X","Coordenada Y","Raio","Velocidade (m/s)");
    }
    List<String> lojaLabels() {
        return Arrays.asList("ID para login","Palavra-passe","Nome","Coordenada X","Coordenada Y","Tempo de Espera");
    }
    List<String> transpLabels() {
        return Arrays.asList("ID para login","Palavra-passe","Nome","Coordenada X","Coordenada Y","Raio","NIF","Preço por km","Taxa Peso","Velocidade (m/s)");
    }

    public void execOptionRegistar(int selected) {
        InputSequence seq = this.currentOptionSet.get(selected).getSecond();
        while(seq.active && !seq.ended) {
            List<String> labels = null;
            if(selected==1) labels = userLabels();
            if(selected==2) labels = lojaLabels();
            if(selected==3) labels = transpLabels();
            if(selected==4) labels = volLabels();
            seq.nextCommand(this,inputLoginWindowRegistar(selected,seq,labels));
        }
        if (seq.ended && seq.active && currentOptionSet.get(selected).getThird() != null ) {
            seq.exec(currentOptionSet.get(selected).getThird());
        }
    }



    private Block instructionBlockTable() {
        Block b = new Block();
        b.addLine(new Line("Opções").colorBG(BLACK+WHITE_BACKGROUND));

        b.addLine(new Line("A - Adicionar Linha"));
        b.addLine(new Line("C - Confirmar Pedido"));
        b.addLine(new Line("X - Sair"));
        b.setMargins(Arrays.asList(1,1,1,1));
        return b;
    }

    private Block tableToBlock(ITable table) {
        List<String> lines = table.toStringList();
        List<Line> linesFinal = lines.stream().map(l -> newLine(l)).collect(Collectors.toList());
        return new Block(linesFinal);
    }


    public Window table(ITable t, String title) {
        List<Block> blocks = new ArrayList<>();
        Window w = makeWindow(true,title);
        blocks.add(instructionBlockTable());
        blocks.add(tableToBlock(t));
        Panel p = new Panel(blocks);
        w.addPanel(p);
        return w;
    }

    /**
     * Função capaz de mostrar os resultados de uma Querie em forma de Tabela
     * @param q número da querie
     * @param querieResult resultado da querie
     */
    private void showQuerieTable(int q, IPair<ITriplet<String,Boolean,List<Double>>,Object> querieResult) {
        ITable t = (ITable) querieResult.getSecond();
        String title = "TABELA";
        int option = 0;
        Window window = this.table(t,title);
        while (option != -1){
            option = Input.readOption(tableDefaultCommands(),this,window);
        }
    }








    public BetterPredicate isMenuOption(Map<Integer, ITriplet<String, InputSequence, Function<List<Object>,Object>>> options) {
        return new BetterPredicate((p-> (Integer)p >= 0 && options.containsKey((Integer)p)),("uma opção válida"));
    }


    public View clone() {
        return new View(this);
    }

}