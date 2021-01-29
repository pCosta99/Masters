package Controler;

import Model.*;
import View.*;
import javafx.event.ActionEvent;
import javafx.scene.Node;
import javafx.stage.Stage;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.*;


public class Controler implements IControler {
    private IModel model;
    private IView view;
    private Timer timer = new Timer();

    public Controler() {
        model = new Model();
        try {
            //inicia(); // TO BE RUNNED IF S* HITS THE FAN
            this.model = model.loadEstado();
        } catch (IOException | ClassNotFoundException e) {
            view.alert("Erro", "O controler falhou a dar load do estado.");
        }
        view = new View(this);
    }
                
    public void setModel(IModel model) {
        this.model = model;
    }
    public void save() {
        try {
            model.guardaEstado();
        } catch (IOException e) {
            view.alert("Erro", "O controller falhou a guardar o estado.");
        }
    }
    public IModel getModel(){
        return model;
    }
    public void end_scene(ActionEvent e) {
        final Node source = (Node) e.getSource();
        final Stage stage = (Stage) source.getScene().getWindow();
        stage.close();
    }

    // DONE
    public void update_user(IUtilizador u){
        view.make_window("Menu de Utilizador " + u.getNome(), view.menu_user(u, model.lojas(), u.historico(), model.encomendas_u(u)));
    }
    public void update_transportadora(ITransportadora t){
        view.make_window("Menu de Transportadora " + t.getNome(), view.menu_transportadora(t, model.lojas(), t.getHistorico(), t.fat()));
    }
    public void update_voluntario(IVoluntario v){
        view.make_window("Menu de Voluntário " + v.getNome(), view.menu_voluntario(v,model.lojas(), v.getHistorico()));
    }
    public void update_loja(ILoja l){
        view.make_window("Menu de Loja " + l.getNome(), view.menu_loja(l, l.get_encomendas_fila()));
    }

    // DONE
    public void loja_selecionada(IUtilizador u, String nome) {
        ILoja loja = model.loja_nome(nome);
        view.make_window("Lista de produtos da Loja '" + loja.getNome() + "'", view.select_produtos(u, loja,loja.produtos()));
    }
    public void loja_selecionada(ITransportadora t, String nome) {
        ILoja loja = model.loja_nome(nome);
        List<String> lista = model.precisa_recolha(loja);
        if(lista.size() > 0){
            view.make_window("Lista de encomendas da Loja '" + loja.getNome() + "'", view.encomendas_ativas(t, lista));
        } else view.alert("Não há mais encomendas.","A loja escolhida não têm encomendas ativas de momento");
    }
    public void loja_selecionada(IVoluntario v, String nome) {
        ILoja loja = model.loja_nome(nome);
        List<String> lista = model.precisa_recolha(loja);
        if(lista.size() > 0){
            view.make_window("Lista de encomendas da Loja '" + loja.getNome() + "'", view.encomendas_ativas(v, lista));
        } else view.alert("Não há mais encomendas.","A loja escolhida não têm encomendas ativas de momento");
    }

    public void finalizar_encomenda(IUtilizador u, String estafeta, String value){
        for(IEncomenda enc : model.getEncMap().values()){
            if(enc.getEstafeta().contains(estafeta)){
                u.addHistorico(enc);
                u.setAcessos(u.getAcessos()+1);

                ILoja l = model.loja(enc.getLoja());
                if(!l.check_fila())
                    view.alert("Fila cheia", "A Loja de momento está cheia, o pedido vai ser processado, mas primeiro aguarda que alguém saia da fila.");
                l.addHistorico(enc);

                if(value.startsWith("t")){
                    ITransportadora t = model.transportadora(value);
                    long tempo = (long) (l.f_time() * 100 * distancia(u.getId(), l.getId(), t.getId()));
                    enc.setTempo((int) tempo );
                    timer.schedule( new Finalizar_rt(t, enc, u, l) , tempo);
                }
                if(value.startsWith("v")){
                    IVoluntario v = model.voluntario(value);
                    v.not_available();
                    long tempo = (long) (l.f_time() * 100 * distancia(u.getId(), l.getId(), v.getId()));
                    enc.setTempo((int) tempo );
                    timer.schedule(new Finalizar_rv(v, enc, u, l), tempo);
                }
                return;
            }
        }
    }
    public void pedir_recolha(ITransportadora t, String value){
        if(!t.check_available()){
            view.alert("Atingiu-se o máximo", "A transportadora não consegue transportar mais encomendas.");
        }
        if(!model.getEncMap().containsKey(value)){
            view.alert("Encomenda Fantasma", "OOPS! Parece que alguém fez mal o inventário da semana passada!");
            return;
        }
        String userid = model.encomenda(value).getUserId();
        String lojaid = model.encomenda(value).getLoja();
        if(dentro_range_t(model.user(userid), model.loja(lojaid), t)){
            DecimalFormat df = new DecimalFormat("####0.00");
            t.setPreco_transporte(t.getPreco_km()*distancia(userid, lojaid, value));
            model.encomenda(value).getEstafeta().add("Transportadora: " + t.getId() + " - Nome: " + t.getNome() + " - " + df.format(t.getPreco_transporte()) + "€ - Rating: " + t.estrela());
        } else view.alert("Encomenda fora de alcance", "O range da sua transportadora não permite realizar esta recolha.");
    }
    public void pedir_recolha(IVoluntario v, String value){
        if(!v.check_available()){
            view.alert("Atingiu-se o máximo", "Este voluntário já se encontra a transportar uma encomenda.");
        }
        if(!model.getEncMap().containsKey(value)){
            view.alert("Encomenda Fantasma", "OOPS! Parece que alguém fez mal o inventário da semana passada!");
            return;
        }
        String userid = model.encomenda(value).getUserId();
        String lojaid = model.encomenda(value).getLoja();
        if(dentro_range_v(model.user(userid), model.loja(lojaid), v)){
            model.encomenda(value).getEstafeta().add("Voluntário: " + v.getId() + " - Nome: " + v.getNome() + " - Rating: " + v.estrela());
        } else view.alert("Encomenda fora de alcance", "O seu range não permite realizar esta recolha.");
    }

    public void rating(IUtilizador u, String s, char type){
        if(type == 't'){
            model.transportadora(s).getRating().add(view.rating("Introduza o rating", "Por favor, avalie a sua satisfação perante a encomenda realizada. [0 muito mau] [10 muito bom]"));
        }
        if(type == 'v'){
            model.voluntario(s).getRating().add(view.rating("Introduza o rating", "Por favor, avalie a sua satisfação perante a encomenda realizada. [0 muito mau] [10 muito bom]"));
        }
    }
    public void listar_on_going(){
        view.make_window("Encomendas Ativas", view.print_list(model.encomendas_ativas()));
    }
    public void listar_top_users(){
        view.make_window("Top 10 Utilizadores", view.print_list(model.top10Acessos()));
    }
    public void listar_top_transportadoras(){ view.make_window("Top 10 Transportadoras", view.print_list(model.top10Distancias()));}

    // REGISTO
    public void registaTransportadora(String id, String nome, String email, String pwd, String nif, double range, double preco) throws IOException {

        ITransportadora transportadora = new Transportadora();
        Double[] loc = localizacao();

        transportadora.setId(id);
        transportadora.setPreco_km(preco);
        transportadora.setRange(range);
        transportadora.setNif(nif);
        transportadora.setNome(nome);
        transportadora.setEmail(email);
        transportadora.setPwd(pwd);
        transportadora.setLocalizacaoX(loc[0]);
        transportadora.setLocalizacaoY(loc[1]);

        model.addTransportadora(transportadora);
        model.guardaEstado();
    }
    public void registaVoluntario(String id, String nome, String email, String pwd, double range) throws IOException {

        IVoluntario voluntario = new Voluntario();

        Double[] loc = localizacao();

        voluntario.setId(id);
        voluntario.setRange(range);
        voluntario.setNome(nome);
        voluntario.setEmail(email);
        voluntario.setPwd(pwd);
        voluntario.setLocalizacaoX(loc[0]);
        voluntario.setLocalizacaoY(loc[1]);


        model.addVoluntario(voluntario);
        model.guardaEstado();

    }
    public void registaLoja(String id, String nome, String email, String pwd) throws IOException {
        ILoja loja = new Loja();

        Double[] loc = localizacao();

        loja.setId(id);
        loja.setNome(nome);
        loja.setEmail(email);
        loja.setPwd(pwd);
        loja.setLocalizacaoX(loc[0]);
        loja.setLocalizacaoY(loc[1]);

        model.addLoja(loja);
        model.guardaEstado();
    }
    public void registaUtilizador(String id, String nome, String email, String pwd) throws IOException {
        IUtilizador utilizador = new Utilizador();

        Double[] loc = localizacao();

        utilizador.setId(id);
        utilizador.setEmail(email);
        utilizador.setId(id);
        utilizador.setPwd(pwd);
        utilizador.setNome(nome);
        utilizador.setEstado(0);
        utilizador.setAcessos(0);
        utilizador.setLocalizacaoX(loc[0]);
        utilizador.setLocalizacaoY(loc[1]);

        model.addUtilizador(utilizador);
        model.guardaEstado();
    }

    // VALIDAÇÃO
    public void validaRegUser(String email, String pwd, String nome) {
        if(email == null) return;
        if(pwd == null) return;
        if(nome == null) return;

        if (!model.validaRegistoUser(email)) {
            view.alert("Erro.", "Email já em uso. Tente novamente com um novo email.");
            return;
        }
        try {
            registaUtilizador("u" + model.contaNCodUser(), nome, email, pwd);
        } catch (IOException e) {
            view.alert("Erro.", "Programa falhou a registar um utilizador.");
        }
    }
    public void validaRegTrans(String email, String pwd, String nome, String nif, String range, String precokm) {
        if(email == null) return;
        if(pwd == null) return;
        if(nome == null) return;
        if(nif == null) return;
        if(range == null) return;
        if(precokm == null) return;

        if (!model.validaRegistoTrans(email)) {
            view.alert("Erro.", "Email já em uso. Tente novamente com um novo email.");
            return;
        }

        try {
            registaTransportadora("t" + model.contaNCodTrans(), nome, email, pwd, nif, Double.parseDouble(range), Double.parseDouble(precokm));
        } catch (IOException e) {
            view.alert("Erro.", "Programa falhou a registar uma transportadora.");
        }
    }
    public void validaRegVol(String email, String pwd, String nome, String range) {
        if(email == null) return;
        if(pwd == null) return;
        if(nome == null) return;
        if(range == null) return;

        if (!model.validaRegistoVol(email)) {
            view.alert("Erro.", "Email já em uso. Tente novamente com um novo email.");
            return;
        }

        try {
            registaVoluntario("v" + model.contaNCodVol(), nome, email, pwd, Double.parseDouble(range));
        } catch (IOException e) {
            view.alert("Erro.", "Programa falhou a registar uma voluntário.");
        }
    }
    public void validaRegLoja(String email, String pwd, String nome) {
        if(email == null) return;
        if(pwd == null) return;
        if(nome == null) return;

        if(!model.validaRegistoLoja(email)) {
            view.alert("Erro.", "Email já em uso. Tente novamente com um novo email.");
            return;
        }

        try {
            registaLoja("l" + model.contaNCodLoja(), nome, email, pwd);
        } catch (IOException e) {
            view.alert("Erro.", "Programa falhou a registar uma loja.");
        }
    }
    public void validaLogInUser(String email, String pwd) {
        if (model.validaLogInUser(email, pwd)) {
            IUtilizador u = model.getUser(email);
            view.make_window("Menu de Utilizador " + u.getNome(), view.menu_user(u, model.lojas(), u.historico(), model.encomendas_u(u)));
        } else view.alert("Erro no login.","Credenciais erradas ou não existentes.");
    }
    public void validaLogInTrans(String email, String pwd) {
        if (model.validaLogInTrans(email, pwd)){
            ITransportadora t = model.getTrans(email);
            view.make_window("Menu de Transportadora " + t.getNome(), view.menu_transportadora(t, model.lojas(), t.getHistorico(), t.fat()));
            return;
        }
        view.alert("Erro no login.","Credenciais erradas ou não existentes.");
    }
    public void validaLogInVol(String email, String pwd) {
        if (model.validaLogInVol(email, pwd)) {
            IVoluntario v = model.getVol(email);
            view.make_window("Menu de Voluntário " + v.getNome(), view.menu_voluntario(v, model.lojas(), v.getHistorico()));
            return;
        }
        view.alert("Erro no login.","Credenciais erradas ou não existentes.");
    }
    public void validaLogInLoja(String email, String pwd) {
        if(model.validaLogInLoja(email, pwd)){
            ILoja l = model.getLoja(email);
            view.make_window("Menu de Loja " + l.getNome(), view.menu_loja(l, l.get_encomendas_fila()));
            return;
        }
        view.alert("Erro no login.","Credenciais erradas ou não existentes.");
    }

    // done
    public void pedidoUser(LinhaEncomenda produto, String idLoja, String userId) throws IOException {
        IEncomenda encomenda = new Encomenda();
        String id = "e" + model.contaNCodEnc();

        encomenda.setId(id);
        encomenda.setLoja(idLoja);
        encomenda.setUserId(userId);
        encomenda.addProdutos(produto);
        encomenda.setPeso(produto.getPeso());
        encomenda.setPreco(produto.getPreco());

        model.addEncomenda(encomenda);
        model.loja(idLoja).addLista(encomenda);
    }
    public boolean dentro_range_t(IUtilizador user, ILoja loja, ITransportadora t) {
        return Math.sqrt(Math.pow((loja.getLocalizacaoX() - t.getLocalizacaoX()), 2) + Math.pow((loja.getLocalizacaoY() - t.getLocalizacaoY()), 2)) < t.getRange() && Math.sqrt(Math.pow((user.getLocalizacaoX() - t.getLocalizacaoX()), 2) + Math.pow((user.getLocalizacaoY() - t.getLocalizacaoY()), 2)) < t.getRange();
    }
    public boolean dentro_range_v(IUtilizador user, ILoja loja, IVoluntario v) {
        return Math.sqrt(Math.pow((loja.getLocalizacaoX() - v.getLocalizacaoX()), 2) + Math.pow((loja.getLocalizacaoY() - v.getLocalizacaoY()), 2)) < v.getRange() && Math.sqrt(Math.pow((user.getLocalizacaoX() - v.getLocalizacaoX()), 2) + Math.pow((user.getLocalizacaoY() - v.getLocalizacaoY()), 2)) < v.getRange();
    }
    public double distancia(String userid, String lojaid, String transid) {
        IUtilizador user = this.model.user(userid);
        ILoja loja = this.model.loja(lojaid);
        return Math.sqrt(Math.pow(Math.abs(loja.getLocalizacaoX() - user.getLocalizacaoX()), 2) + Math.pow((loja.getLocalizacaoY() - user.getLocalizacaoY()), 2));
    }
    public Double[] localizacao (){
        Double [] loc = new Double[2];

        Random r = new Random();
        double low = -100;
        double high = 100;
        double resultX = r.nextDouble();
        loc[0] = low + (resultX * (high - low));
        double resultY = r.nextDouble();
        loc[1] = low + (resultY * (high - low));

        return loc;
    }

    class Finalizar_rt extends TimerTask{
        ITransportadora t;
        IEncomenda enc;
        IUtilizador u;
        ILoja l;

        public Finalizar_rt(ITransportadora t, IEncomenda enc, IUtilizador u, ILoja l){
            this.t = t;
            this.enc = enc;
            this.u = u;
            this.l = l;
        }

        @Override
        public void run() {
            double t1 = t.getPreco_transporte();
            t.setDistancia(distancia(u.getId(), l.getId(), enc.getId()));
            DecimalFormat df = new DecimalFormat("####0.00");
            t.addHistorico("User: " + enc.getUserId() + " | Encomenda: " + enc.getId() + " | Preço: " + df.format(t1) + " | Tempo: " + (enc.getTempo()/600) + "minutos." );
            t.addFaturacao(t1);
            t.available();

            model.removeEncomenda(enc.getId());
            l.remove_fila();
            l.removeLista(enc.getId());
        }
    }

    class Finalizar_rv extends TimerTask{
        IVoluntario v;
        IEncomenda enc;
        IUtilizador u;
        ILoja l;

        public Finalizar_rv(IVoluntario v, IEncomenda enc, IUtilizador u, ILoja l){
            this.v = v;
            this.enc = enc;
            this.u = u;
            this.l = l;
        }

        @Override
        public void run() {
            v.n_encomedas();
            v.addHistorico("User: " + enc.getUserId() + " , Encomenda: " + enc.getId() + " , Tempo: " + enc.getTempo() + (enc.getTempo()/600) + "minutos.");
            v.available();

            model.removeEncomenda(enc.getId());
            l.remove_fila();
            l.removeLista(enc.getId());
        }
    }

    // not to be used ever again, emergencies only
    public void escreveMail() {

        int i = 1;
        for (IUtilizador u : this.model.getUserMap().values()) {
            if (u.getEmail() == null) u.setEmail("user" + i + "@gmail.com");
            if (u.getPwd() == null) u.setPwd("123456");
            i++;
        }
        i = 1;
        for (ITransportadora u : this.model.getTransMap().values()) {
            if (u.getEmail() == null) u.setEmail("trans" + i + "@gmail.com");
            if (u.getPwd() == null) u.setPwd("123456");
            i++;
        }
        i = 1;
        for (IVoluntario u : this.model.getVolMap().values()) {
            if (u.getEmail() == null) u.setEmail("vol" + i + "@gmail.com");
            if (u.getPwd() == null) u.setPwd("123456");
            i++;
        }
        i = 1;
        for (ILoja u : this.model.getLojaMap().values()) {
            if (u.getEmail() == null) u.setEmail("loja" + i + "@gmail.com");
            if (u.getPwd() == null) u.setPwd("123456");
            i++;
        }
    }
    public void inicia () throws IOException {
        model.fileToTrans();
        model.filetoLoja();
        model.fileToUser();
        model.fileToVol();
        model.loadInventLoja();
        escreveMail();
        model.guardaEstado();
    }
}







