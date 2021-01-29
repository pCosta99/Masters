package Model;

import Helpers.Pair;

import java.io.*;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class Sistema implements Observer, Serializable {

    private Map<String,Utilizador> utilizadores;
    private Map<String,Voluntario> voluntarios;
    private Map<String,Transportadora> transportadoras;
    private Map<String,Loja> lojas;
    private Map<String,Encomenda> encomendas;
    private Map<String,Encomenda> encsEmEspera; //provavelmente terá mais infromação que simplesmente uma encomenda

    private Map<String,Encomenda> encsEmTransporte;

    private List<RegistoEnc> registosEnc;

    private String currentLastProdID;
    private String currentLastEncID;
    private String currentLastUserID;
    private String currentLastVolID;
    private String currentLastLojaID;
    private String currentLastTranspID;


    public Sistema() {
        utilizadores = new HashMap<>();
        voluntarios = new HashMap<>();
        transportadoras = new HashMap<>();
        lojas = new HashMap<>();
        encomendas = new HashMap<>();
        encsEmEspera = new HashMap<>();
        registosEnc = new ArrayList<>();
    }

    public void registarEncomenda(Encomenda enc) {
        this.encsEmEspera.put(enc.getCodEncomenda(),enc);
        this.lojas.get(enc.getCodLoja()).addEncEmEspera(enc);
    }

    public String getBiggestProductID() {
        return this.encomendas.values()
                .stream()
                .map(Encomenda::getLinhasEncomenda)
                .flatMap(List::stream)
                .map(l -> l.getProduto())
                .sorted()
                .map(p -> p.getCodProduto())
                .reduce((first, second) -> second)
                .orElse(null);
    }

    public String getBiggestEncID() {
        return this.encomendas.values()
                .stream()
                .sorted()
                .map(Encomenda::getCodEncomenda)
                .reduce((first, second) -> second)
                .orElse(null);
    }

    public String getBiggestUserID() {
        return this.utilizadores.values()
                .stream()
                .sorted()
                .map(Utilizador::getCodUtilizador)
                .reduce((first, second) -> second)
                .orElse(null);
    }

    public String getBiggestVolID() {
        return this.voluntarios.values()
                .stream()
                .sorted()
                .map(Voluntario::getCodVoluntario)
                .reduce((first, second) -> second)
                .orElse(null);
    }

    public String getBiggestLojaID() {
        return this.lojas.values()
                .stream()
                .sorted()
                .map(Loja::getCodLoja)
                .reduce((first, second) -> second)
                .orElse(null);
    }

    public String getBiggestTranspID() {
        return this.transportadoras.values()
                .stream()
                .sorted()
                .map(Transportadora::getCodEmpresa)
                .reduce((first, second) -> second)
                .orElse(null);
    }



    public String nextUserId() {
        String next = "u" + Integer.toString(Integer.parseInt(currentLastUserID.substring(1))+1);
        this.currentLastUserID = next;
        return next;
    }

    public String nextProdId() {
        String next = "p" + Integer.toString(Integer.parseInt(currentLastProdID.substring(1))+1);
        this.currentLastProdID = next;
        return next;
    }

    public String nextEncId() {
        String next = "e" + Integer.toString(Integer.parseInt(currentLastEncID.substring(1))+1);
        this.currentLastEncID = next;
        return next;
    }

    public String nextVolId() {
        String next = "v" + Integer.toString(Integer.parseInt(currentLastVolID.substring(1))+1);
        this.currentLastVolID = next;
        return next;
    }

    public String nextLojaId() {
        String next = "l" + Integer.toString(Integer.parseInt(currentLastLojaID.substring(1))+1);
        this.currentLastLojaID = next;
        return next;
    }

    public String nextTranspId() {
        String next = "t" + Integer.toString(Integer.parseInt(currentLastTranspID.substring(1))+1);
        this.currentLastTranspID = next;
        return next;
    }


    public void generateProducts() {
        for(Loja l : this.lojas.values()) {
            for(int i = 0; i<5; i++) {
                double min = 1;
                double max = 100;
                Random random = new Random();
                double quant = min + (max - min) * random.nextDouble();
                double preco = min + (max - min) * random.nextDouble();
                l.registarProd(new Produto(nextProdId(),("Produto"+i)),quant,preco);
            }
        }
    }

    public String getCurrentLastTranspID() {
        return currentLastTranspID;
    }

    public void setCurrentLastTranspID(String currentLastLTranspID) {
        this.currentLastTranspID = currentLastLTranspID;
    }

    public String getCurrentLastLojaID() {
        return currentLastLojaID;
    }

    public void setCurrentLastLojaID(String currentLastLojaID) {
        this.currentLastLojaID = currentLastLojaID;
    }

    public String getCurrentLastVolID() {
        return currentLastVolID;
    }

    public void setCurrentLastVolID(String currentLastVolID) {
        this.currentLastVolID = currentLastVolID;
    }

    public String getCurrentLastUserID() {
        return currentLastUserID;
    }

    public void setCurrentLastUserID(String currentLastUserID) {
        this.currentLastUserID = currentLastUserID;
    }

    public String getCurrentLastProdID() {
        return currentLastProdID;
    }

    public void setCurrentLastProdID(String currentLastProdID) {
        this.currentLastProdID = currentLastProdID;
    }

    public String getCurrentLastEncID() {
        return currentLastEncID;
    }

    public void setCurrentLastEncID(String currentLastEncID) {
        this.currentLastEncID = currentLastEncID;
    }

    public void avaliarPedidoDeEntrega(String codEntregador, Encomenda enc){}

    public void registarUtilizador(Utilizador user){
        this.utilizadores.put(user.getCodUtilizador(),user);
    }
    public void registarVoluntario(Voluntario vol){
        this.voluntarios.put(vol.getCodVoluntario(),vol);
    };
    public void registarTransportadora(Transportadora transp){
        this.transportadoras.put(transp.getCodEmpresa(),transp);
    };
    public void registarLoja(Loja loja){
        this.lojas.put(loja.getCodLoja(),loja);
    }

    public void addEncomenda(Encomenda enc) {
        this.encomendas.put(enc.getCodEncomenda(),enc);
    }

    public void addEmEspera(Encomenda enc) {
        this.encsEmEspera.put(enc.getCodEncomenda(),enc);
    }

    public Map<String, SystemUser> getUtilizadores() {
        return utilizadores.entrySet().stream().collect(Collectors.toMap(en->en.getKey(), en -> {
            return (SystemUser) en.getValue();
        }));
    }

    public Map<String, SystemUser> getLojas() {
        return lojas.entrySet().stream().collect(Collectors.toMap(en->en.getKey(), en -> {
            return (SystemUser) en.getValue();
        }));
    }

    public Map<String, SystemUser> getTransportadoras() {
        return transportadoras.entrySet().stream().collect(Collectors.toMap(en->en.getKey(), en -> {
            return (SystemUser) en.getValue();
        }));
    }

    public Map<String, SystemUser> getVoluntarios() {
        return voluntarios.entrySet().stream().collect(Collectors.toMap(en->en.getKey(), en -> {
            return (SystemUser) en.getValue();
        }));
    }


    public List<Encomenda> encsEmRaioSorted(Voluntario vol) {
        GPS gpsVol = vol.getGps();
        double raioVol = vol.getRaio();
        List<Encomenda> s = this.encomendas.values().stream().
                filter(enc -> (Math.sqrt(Math.pow(gpsVol.getX() - enc.getLojaGPS().getX(), 2) + Math.pow(gpsVol.getY() - enc.getLojaGPS().getY(), 2)) <= raioVol))
                .sorted()
                .collect(Collectors.toList());
        return s;
    }

    public void transportByVol(String codVol, String codEnc) {
        Voluntario vol = this.voluntarios.get(codVol);
        Encomenda enc = this.encomendas.get(codEnc);
        this.encomendas.remove(enc.getCodEncomenda());
        double time = vol.timeToTransport(enc);
        System.out.println("Voluntário está a transportar, tempo: " + time+" segundos");
        new java.util.Timer().schedule(
                new java.util.TimerTask() {
                    @Override
                    public void run() {
                        System.out.println("Encomenda " + enc.getCodEncomenda() + " chegou!!");
                        registosEnc.add(new RegistoEnc(
                                enc.getCodUtilizador(),
                                enc.getCodLoja(),
                                vol.getCodVoluntario(),
                                0,
                                time,
                                LocalDateTime.now()
                        ));
                    }
                },
                (int)time*1000
        );

    }

    public SystemUser getSystemUser(Character type, String cod) {
        if(type == 'u') return (SystemUser) new Utilizador(this.utilizadores.get(cod));
        if(type == 'l') return (SystemUser) new Loja(this.lojas.get(cod));
        if(type == 't') return (SystemUser) new Transportadora(this.transportadoras.get(cod));
        else return (SystemUser) new Voluntario(this.voluntarios.get(cod));

    }

    public Loja getLoja(String cod) {
        return this.lojas.get(cod);
    }

    public Utilizador getUtilizador(String cod) {
        return this.utilizadores.get(cod);
    }

    public Voluntario getVoluntario(String cod) {
        return this.voluntarios.get(cod);
    }


    public List<String> getLojasNames() {
        return this.lojas.values().stream().map(Loja::getNomeLoja).sorted().collect(Collectors.toList());
    }

    public List<Loja> getLojasSorted() {
        return this.lojas.values().stream().sorted(Comparator.comparing(Loja::getNomeLoja)).collect(Collectors.toList());
    }

    public Map<String, Voluntario> getVoluntariosNormal() {
        return voluntarios;
    }

    public Map<String, Transportadora> getTransportadorasNormal() {
        return transportadoras;
    }

    public Map<String, Loja> getLojasNormal() {
        return lojas;
    }

    public void giveEncsToTransps() {
        List<Encomenda> encs = new ArrayList<>(this.encomendas.values());
        for(Transportadora t : this.transportadoras.values()) {
            if(!encs.isEmpty()) {
                if(t.getSTATUS() == 0) {
                    giveEncToTransp(t,encs.get(0));
                    this.encomendas.remove(encs.get(0).getCodEncomenda());
                    encs = new ArrayList<>(this.encomendas.values());
                }

            }
        }
    }

    public void giveEncToTransp(Transportadora t, Encomenda enc) {
        double custo = t.addEncomenda(enc);
        Utilizador user = this.utilizadores.get(enc.getCodUtilizador());
        user.setNotif(new Pair<>(enc,new Pair<>(t.getCodEmpresa(),custo)));
        t.setSTATUS(1);
    }

    public Map<String, Encomenda> getEncomendas() {
        return encomendas;
    }

    public Map<String, Encomenda> getEncsEmEspera() {
        return encsEmEspera;
    }

    public void aceitaEncomenda(String codEnc) {
        this.encomendas.put(codEnc,this.encsEmEspera.get(codEnc));
        this.encsEmEspera.remove(codEnc);
        this.giveEncsToTransps();
    }

    public void transportByTransp(String codTransp, Encomenda enc, Double custo) {
        Transportadora transp = this.transportadoras.get(codTransp);
        transp.removeEncomenda();
        transp.setSTATUS(0);
        double time = transp.timeToTransport(enc);
        System.out.println("Voluntário está a transportar, tempo: " + time+" segundos");
        new java.util.Timer().schedule(
                new java.util.TimerTask() {
                    @Override
                    public void run() {
                        System.out.println("Encomenda " + enc.getCodEncomenda() + " chegou!!");
                        registosEnc.add(new RegistoEnc(
                                enc.getCodUtilizador(),
                                enc.getCodLoja(),
                                transp.getCodEmpresa(),
                                custo,
                                time,
                                LocalDateTime.now()
                        ));
                        giveEncsToTransps();
                    }
                },
                (int)time*1000
        );

    }

    @Override
    public String toString() {
        return "Sistema{" +
                "utilizadores=" + utilizadores +
                "\n voluntarios=" + voluntarios +
                "\n transportadoras=" + transportadoras +
                "\n lojas=" + lojas +
                "\n encomendas=" + encomendas +
                "\n encsEmEspera=" + encsEmEspera +
                "\n encsEmTransporte=" + encsEmTransporte +
                "\n registosEnc=" + registosEnc +
                "\n currentLastProdID='" + currentLastProdID + '\'' +
                "\n currentLastEncID='" + currentLastEncID + '\'' +
                '}';
    }

    @Override
    public void update(Observable obs, String label, List<Object> data) {
        if(label.equals("EncomendaAceite")) {
            String codEnc = (String) data.get(0);
            this.encomendas.put(codEnc,this.encsEmEspera.get(codEnc));
            this.encsEmEspera.remove(codEnc);
        } else if(label.equals("AceitarTransp")) {
            Pair<Encomenda,Pair<String,Double>> enc = (Pair<Encomenda,Pair<String,Double>>) data.get(0);
            this.transportByTransp(enc.getSecond().getFirst(),enc.getFirst(),enc.getSecond().getSecond());
        }
    }


    public void save(String filePath) throws IOException {
        FileOutputStream o = new FileOutputStream(filePath);
        ObjectOutputStream r = new ObjectOutputStream(o);
        r.writeObject(this);
        r.flush();
        r.close();
    }

    public static Sistema read(String filePath) throws IOException, ClassNotFoundException {
        FileInputStream r = new FileInputStream(filePath);
        ObjectInputStream o = new ObjectInputStream(r);
        Sistema g = (Sistema) o.readObject();
        o.close();
        return g;
    }


    public void addUtilizador(Utilizador u) {
        this.utilizadores.put(u.getCodUtilizador(),u);
        System.out.println(u);
    }


    public void addVol(Voluntario vol) {
        this.voluntarios.put(vol.getCodVoluntario(),vol);
        System.out.println(vol);
    }

    public void addLoja(Loja l) {
        this.lojas.put(l.getCodLoja(),l);
        System.out.println(l);
    }


    public void addTransp(Transportadora t) {
        this.transportadoras.put(t.getCodEmpresa(),t);
        System.out.println(t);
    }
}
