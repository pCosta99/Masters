package Registos;

import Auxiliares.GPS;
import Auxiliares.Viagem;
import Encomendas.Encomenda;
import Perfis.*;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;




public class Registos implements Serializable {

    /*
    Variaveis
     */

    private HashMap<String, Perfil> perfis;
    private HashMap<String, String> emails;
    private HashMap<String, Encomenda> encomendas;
    private HashMap<String, ArrayList<Transportadores>> vaiRecolher;


    private Integer loja; //l 0000
    private Integer encomenda; //e
    private Integer utilizador; //u
    private Integer voluntario; // v
    private Integer empresa; // t
    private Integer produtos; // p



    /*
    Construtores
     */

    public Registos(HashMap<String,Perfil> perfis, HashMap<String,Encomenda> encomendas, HashMap<String, String> emails,HashMap<String,ArrayList<Transportadores>> vaiRecolher,int loja,int encomenda, int utilizador, int voluntario, int empresa, int produtos){
        this.perfis = perfis;
        this.encomendas = encomendas;
        this.emails = emails;
        this.vaiRecolher = vaiRecolher;
        this.loja = loja;
        this.encomenda = encomenda;
        this.utilizador = utilizador;
        this.voluntario = voluntario;
        this.empresa = empresa;
        this.produtos = produtos;
    }



    public Registos(){
        this.perfis = new HashMap<>();
        this.encomendas = new HashMap<>();
        this.emails = new HashMap<>();
        this.vaiRecolher = new HashMap<>();
        this.loja = 0;
        this.encomenda = 0;
        this.utilizador = 0;
        this.voluntario = 0;
        this.empresa = 0;
        this.produtos = 0;

    }

     
    public Registos(Registos r){
        this.setPerfis(r.getPerfis());
        this.setEncomendas(r.getEncomendas());
        this.setVaiRecolher(r.getVaiRecolher());
        this.loja = r.getLoja();
        this.encomenda = r.getEncomenda();
        this.utilizador = r.getUtilizador();
        this.voluntario = r.getVoluntario();
        this.empresa = r.getEmpresa();
        this.produtos = r.getProdutos();

    }

    

    /*
    Getters e Setters
     */

    public HashMap<String, Perfil> getPerfis() {
        HashMap<String,Perfil> res = new HashMap<>();
        for(HashMap.Entry<String,Perfil> p: this.perfis.entrySet()){
            res.put(p.getKey(),p.getValue().clone());
        }
        return res;
    }

    public void setPerfis (HashMap<String, Perfil> p) {
        this.perfis = new HashMap<>();
        p.entrySet().forEach(x -> this.perfis.put(x.getKey(), x.getValue().clone()));
    }

    public HashMap<String, Encomenda> getEncomendas() {
        HashMap<String,Encomenda> res = new HashMap<>();
        for(HashMap.Entry<String,Encomenda> e: this.encomendas.entrySet()){
            res.put(e.getKey(),e.getValue().clone());
        }
        return res;
    }

    public void setEncomendas(HashMap<String, Encomenda> enc) {
        this.encomendas = new HashMap<>();
        enc.entrySet().forEach(e -> this.encomendas.put(e.getKey(), e.getValue().clone()));
    }

    public HashMap<String, String> getEmails() {
        HashMap<String,String > res = new HashMap<>();
        for(HashMap.Entry<String,String> e: this.emails.entrySet()){
            res.put(e.getKey(),e.getValue());
        }
        return res;
    }

    public void setEmails(HashMap<String, String> mail) {
        this.emails = new HashMap<>();
        mail.entrySet().forEach(m -> this.emails.put(m.getKey(), m.getValue()));
    }

    public HashMap<String, ArrayList<Transportadores>> getVaiRecolher() {
        HashMap<String,ArrayList<Transportadores> > res = new HashMap<>();
        for(HashMap.Entry<String,ArrayList<Transportadores>> e: this.vaiRecolher.entrySet()){
            res.put(e.getKey(),e.getValue());
        }
        return res;
    }

    public void setVaiRecolher(HashMap<String, ArrayList<Transportadores>> vaiRec) {
        this.vaiRecolher = new HashMap<>();
        vaiRec.entrySet().forEach(m -> this.vaiRecolher.put(m.getKey(), m.getValue()));
    }




    public Integer getLoja() {
        return loja;
    }

    public void setLoja(Integer loja) {
        this.loja = loja;
    }

    public Integer getEncomenda() {
        return encomenda;
    }

    public void setEncomenda(Integer encomenda) {
        this.encomenda = encomenda;
    }

    public Integer getUtilizador() {
        return utilizador;
    }

    public void setUtilizador(Integer utilizador) {
        this.utilizador = utilizador;
    }

    public Integer getVoluntario() {
        return voluntario;
    }

    public void setVoluntario(Integer voluntario) {
        this.voluntario = voluntario;
    }

    public Integer getEmpresa() {
        return empresa;
    }

    public void setEmpresa(Integer empresa) {
        this.empresa = empresa;
    }

    public Integer getProdutos() {
        return produtos;
    }

    public void setProdutos(Integer produtos) {
        this.produtos = produtos;
    }

    /*
    Metodos
     */

    //Emails
    public String toStringEmails() {
        StringBuilder sb = new StringBuilder();

        for(Map.Entry<String, String> u: this.emails.entrySet()){
            sb.append("Email : \n").append(u.getKey()).append("\n").append("Código de perfil : \n").append(u.getValue());
        }
        return sb.toString();
    }


    // Perfis
    public String toStringPerfis() {
        StringBuilder sb = new StringBuilder();

        for(Perfil u: this.perfis.values()){
            sb.append(u.toString());
        }
        return sb.toString();
    }


    public String toStringPerfisUtilizadores() {
        StringBuilder sb = new StringBuilder();

        for(Perfil u: this.perfis.values()){
            if (u instanceof Utilizador){
                sb.append(u.toString());
            }
        }
        return sb.toString();
    }
    public String toStringPerfisTransportadores() {
        StringBuilder sb = new StringBuilder();

        for(Perfil t: this.perfis.values()){
            if(t instanceof Transportadores){
                sb.append(t.toString());
            }

        }
        return sb.toString();
    }

    public String toStringPerfisLojas() {
        StringBuilder sb = new StringBuilder();

        for(Perfil l: this.perfis.values()){
            if(l instanceof Loja){
                sb.append(l.getNome()).append("\n");
            }
        }
        return sb.toString();
    }
    
    

    // Encomendas
    public String toStringEncomendas() {
        StringBuilder sb = new StringBuilder();

        for(Encomenda e: this.encomendas.values()){
            sb.append(e.toString());
        }
        return sb.toString();
    }
    


    public GPS getCoordenadas (String codigo){

        if (this.perfis.containsKey(codigo)){
            return this.perfis.get(codigo).getCoordenadas();
        }
        else
        return new GPS();
    }

    
    // Classifica o transportador que efetuou a ultima encomenda
    public void classificarTransportadores(int c, String codTransportador,Encomenda enc) {

        if (this.perfis.get(codTransportador) instanceof Transportadores) {

            int numClassificacoes = ((Transportadores) perfis.get(codTransportador)).getClassificacoes();
            double media = ((Transportadores) perfis.get(codTransportador)).getClassificacaoFinal();


            double novaMedia = ((numClassificacoes * media) + c) / (numClassificacoes + 1);
            ((Transportadores)this.perfis.get(codTransportador)).setClassificacaoFinal(novaMedia);
            ((Transportadores)this.perfis.get(codTransportador)).setClassificacoes(numClassificacoes + 1);

            ArrayList<Viagem> viagem = ((Transportadores)this.perfis.get(codTransportador)).getTrips();
            for(Viagem v: viagem){
                if(v.getCodEncomenda().equals(enc.getCodEncomenda())){
                    v.estaClassificado(true); // atualiza a viagem a estar ja classificada
                }
            }
        }
    }

    // Devolve os 10 utilizadores que mais utilizam a aplicacao
    public void top10utilizadores() {
        ArrayList<Perfil> u = new ArrayList<>();

        for (Perfil p : this.perfis.values()) {
            if (p instanceof Utilizador) {
                u.add(p.clone());
            }
        }


        if (u.size() < 10) {
            System.out.println("Não existem 10 utilizadores para mostrar!");
        }


        else {

            u.sort(Perfil::compareTamHistorico);

            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < 10; i++) {
                sb.append(u.get(i).toString()).append("\n");
            }
            System.out.println("Estes são os utilizadores que mais usufruem da nossa aplicação:\n");
            System.out.println(sb.toString());
        }
    }

    // Devolve os 10 transportadoras que coms mais quilometros percorridos
    public void top10transportadoras() {
        ArrayList<Perfil> t = new ArrayList<>();

        for (Perfil p : this.perfis.values()) {
            if (p instanceof Empresa) {
                t.add(p.clone());
            }
        }
        if (t.size() < 10) {
            System.out.println("Não existem 10 transportadores para mostrar!");
        } else {
            t.sort(Transportadores::compareKmsPercorridos);
            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < 10; i++) {
                sb.append(t.get(i).toString()).append("\n");
            }
            System.out.println("Estes são as empresas com mais kilometros percorridos:\n");
            System.out.println(sb.toString());
        }
    }

    // Verifica se um email existe
    public Boolean mailExiste(String mail){
        return emails.containsKey(mail);
    }

    // Verifica se o codigo de um user existe
    public Boolean userExiste(String codigoUtil) {
        return perfis.containsKey(codigoUtil);
    }

    public Boolean lojaExiste(String nome) {
        for(Perfil p: perfis.values()){
            if (p instanceof Loja){
                if(p.getNome().equals(nome)){return true;}
            }
        }
        return false;
    }

    public Boolean encomendaExiste(String encomenda){
        return this.encomendas.containsKey(encomenda);
    }

    //Como a loja existe, retorna o codigo desta  ||| MAYBE NOT NEEDED!
    public String codigoLoja(String nome){
        for(Perfil p: perfis.values()){
            if (p instanceof Loja){
                if(p.getNome().equals(nome)){return p.getCodigoPerfil();}
            }
        }
        return "";
    }

    //Se empresa existe, retorna o codigo desta, senao retorna string vazia
    public String codigoEmpresa(String nomeEmpresa){
        for(Perfil p: perfis.values()){
            if (p instanceof Empresa){
                if(p.getNome().equals(nomeEmpresa)){return p.getCodigoPerfil();}
            }
        }
        return "";
    }

    public String codigoUtilizador(String nomeUtilizador){
        for(Perfil p: perfis.values()){
            if (p instanceof Utilizador){
                if(p.getNome().equals(nomeUtilizador)){return p.getCodigoPerfil();}
            }
        }
        return "";
    }

    public Perfil getPerfil(String codigoPerfil){
        return perfis.get(codigoPerfil);
    }

    public String codigoPerfil(String email){
        return emails.get(email);
    }


    public Boolean loginCorreto(String email,String pass){
        if ((emails.containsKey(email))) {
            String codigoPerfil = emails.get(email);
            if ((perfis.containsKey(codigoPerfil))){
                return (perfis.get(codigoPerfil).getPass().equals(pass));
            }
        }
        return false;
    }

    public Boolean jaExisteMail(String email){
        return this.emails.containsKey(email);
    }



    // Adiciona uma encomenda ao hashMap de encomendas
    public void adicionaEncomenda(Encomenda e){
        this.encomendas.put(e.getCodEncomenda(),e);
    }


    // DISPLAY as encomendas que estao no raio de acao da transportadora
    public String encomendasNoRaio(Transportadores t){
        StringBuilder res = new StringBuilder();

        for(Perfil p : this.perfis.values()){
            if(p instanceof Loja){
                Double d = t.getCoordenadas().distanciaEntre(p.getCoordenadas());
                if(t.getCoordenadas().estaDentro(d, t.getRaio())){
                    if(((Loja) p).sinalizarEncomendas()) {
                        res.append(((Loja) p).toStringEncomendas(t));
                    }
                }
            }
        }

        return res.toString();
    }


    public Loja getPerfilLoja(String codigoLoja){
        return (Loja) perfis.get(codigoLoja);
    }

    // Adiciona um novo perfil ao registo de perfis
    public void addPerfil(Perfil perfil){
        this.emails.put(perfil.getEmail(),perfil.getCodigoPerfil());
        this.perfis.put(perfil.getCodigoPerfil(),perfil);
    }

    // Adiciona uma nova encomenda ao registo de encomendas
    public void addEncomenda(Encomenda e){
        this.encomendas.put(e.getCodEncomenda(),e);
    }

    public Encomenda buscaEncomenda(String codEnc){return this.encomendas.get(codEnc);}

    //vaiRecolher -- dada uma encomenda , vai a lista de transportadores e
    //devolve o arrayList de voluntarios disponiveis para a entrega desta encomenda

    public ArrayList<Voluntario> verificarVoluntarioEncomenda(Encomenda e){
        ArrayList<Voluntario> vol = new ArrayList<>();
        for(Transportadores t : this.vaiRecolher.get(e.getCodEncomenda())){
            if (t instanceof Voluntario){
                vol.add((Voluntario) t.clone());
            }
        }
        return vol;
    }


    public ArrayList<Empresa> verificarEmpresaEncomenda(Encomenda e){
        ArrayList<Empresa> emp = new ArrayList<>();
        for(Transportadores t : this.vaiRecolher.get(e.getCodEncomenda())){
            if (t instanceof Empresa){
                emp.add((Empresa) t.clone());
            }
        }
        return emp;
    }

    // Display de todas as empresas que querem fazer a entrega de uma encomenda especifica
    public void toStringEmpresasVaiRevolher(ArrayList<Empresa> empresas,Encomenda encomenda){
        StringBuilder  sb = new StringBuilder();

        for(Empresa e: empresas){
            sb.append(" - ").append(e.getNome()).append(" fará a entrega da sua encomenda por ").append(e.calculaPrecoTransporte(encomenda,this)).append("€").append("\n");
        }
        System.out.println(sb.toString());
    }


    public void atualizaPerfil(Perfil p){
        String cod = p.getCodigoPerfil();
        this.perfis.replace(cod,p);
    }

    // Apenas para encomendas
    public String toStringGrava(){
        StringBuilder sb  = new StringBuilder();
        for(Encomenda enc: this.encomendas.values()){
            if(enc.getEntregue()){
                sb.append("Aceite:").append(enc.getCodEncomenda());
            }
        }
        return sb.toString();
    }

    public void atualizaRegistos(HashMap<String,Perfil> p, HashMap<String,String> em, HashMap<String,Encomenda> en, HashMap<String,ArrayList<Transportadores>> trans, int lr, int encr, int ur, int vr,int er, int pr){

        if(this.getPerfis().isEmpty()){
            HashMap<String, Perfil> per = new HashMap<>(p);
            this.setPerfis(per);
        }
        else{
            HashMap<String,Perfil> per = this.getPerfis();
            per.putAll(p);
            this.setPerfis(per);
        }

        if(this.getEmails().isEmpty()){
            HashMap<String, String> email = new HashMap<>(em);
            this.setEmails(email);
        }
        else{
            HashMap<String,String> email = this.getEmails();
            email.putAll(em);
            this.setEmails(email);
        }

        if(this.getEncomendas().isEmpty()){
            HashMap<String, Encomenda> enc = new HashMap<>(en);
            this.setEncomendas(enc);
        }
        else{
            HashMap<String,Encomenda> enc = this.getEncomendas();
            enc.putAll(en);
            this.setEncomendas(enc);
        }


        if(this.getVaiRecolher().isEmpty()){
            HashMap<String,ArrayList<Transportadores>> vairec = new HashMap<>(trans);
            this.setVaiRecolher(vairec);
        }
        else{
            HashMap<String,ArrayList<Transportadores>> vairec = this.getVaiRecolher();
            vairec.putAll(trans);
            this.setVaiRecolher(vairec);
        }


        this.setLoja(lr);
        this.setEncomenda(encr);
        this.setUtilizador(ur);
        this.setVoluntario(vr);
        this.setEmpresa(er);
        this.setProdutos(pr);
    }

    public void atualizaVaiRecolher(String codigoEncomenda, String codigo) {
        if (codigo.equals("")) { // utilizador coloca encomenda no vaiRecolher
            HashMap<String, ArrayList<Transportadores>> vaiRecolher1;
            if (this.vaiRecolher == null) {
                vaiRecolher1 = new HashMap<>();
                ArrayList<Transportadores> transportador = new ArrayList<>();
                vaiRecolher1.put(codigoEncomenda, transportador);
            }
            else { // ja existe vaiRecolher
                vaiRecolher1 = new HashMap<>(this.vaiRecolher);
                vaiRecolher1.put(codigoEncomenda,new ArrayList<>());
            }
            setVaiRecolher(vaiRecolher1);
        }
        else{ // o transportador coloca-se no vaiRecolher
            HashMap<String,ArrayList<Transportadores>> vr = new HashMap<>(this.vaiRecolher);
            ArrayList<Transportadores> al;
            if (this.vaiRecolher.get(codigoEncomenda) == null) {
                al = new ArrayList<>();
            }
            else {
                al = vr.get(codigoEncomenda);
            }
            al.add((Transportadores) this.getPerfil(codigo));
            vr.put(codigoEncomenda,al);
            this.setVaiRecolher(vr);
        }
    }

    public void atualizaEmails(String codPerfil, String email){

        HashMap<String, String> emails;
        if (this.emails == null){

            emails = new HashMap<>();
        }
        else{
            emails = new HashMap<>(this.getEmails());
        }
        emails.put(email,codPerfil);
        this.setEmails(emails);
    }


}
