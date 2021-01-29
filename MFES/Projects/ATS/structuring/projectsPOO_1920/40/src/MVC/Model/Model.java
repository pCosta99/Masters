package MVC.Model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.TreeMap;
import java.util.TreeSet;

import Base.Basic.Pair;
import Base.Encomenda.Aceite;
import Base.Encomenda.Encomenda;
import Users.Loja;
import Users.Transportadora;
import Users.Utilizador;
import Users.Voluntario;

public class Model implements Serializable{
    private TreeMap<String,Loja> lojas = new TreeMap<String,Loja>();
    private TreeMap<String,Transportadora> transportadoras = new TreeMap<String,Transportadora>();
    private TreeMap<String,Utilizador> utilizadores = new TreeMap<String,Utilizador>();
    private TreeMap<String,Voluntario> voluntarios = new TreeMap<String,Voluntario>();
    private TreeMap<String,Encomenda> encomendas = new TreeMap<String,Encomenda>();

    private TreeSet<Aceite> loja = new TreeSet<Aceite>();// adicionadas no inicio para aprovacao da loja
    private TreeSet<Aceite> sinalizadas = new TreeSet<Aceite>();// transferidas da loja para aqui quando a loja as aprova, aguardam aprovacao do utilizador
    private TreeSet<Aceite> aceite = new TreeSet<Aceite>();// transferidas das sinalizads para aceite quando o user aprova, estas sao vistas pelos voluntarios e transportadoras
    private TreeSet<Aceite> entregues = new TreeSet<Aceite>(); //entregues pelas transportadoras e viluntarios, o utilizador já pode dar rating

    public Model(){
        
    }

    public Model(Model x){
        this.lojas = x.lojas;
        this.transportadoras = x.transportadoras;
        this.utilizadores = x.utilizadores;
        this.voluntarios = x.voluntarios;
        this.encomendas = x.encomendas;
        this.loja = x.loja;
        this.sinalizadas = x.sinalizadas;
        this.aceite = x.aceite;
        this.entregues = x.entregues;
    }

    public void addLoja(Loja l) {
        this.lojas.put(l.getCodLoja(), l.clone());
    }

    public Loja getLoja(String x) {
        return this.lojas.get(x).clone();
    }

    public void addTransportadora(Transportadora l) {
        this.transportadoras.put(l.getCodEmpresa(), l.clone());
    }

    public void addUtilizador(Utilizador l) {
        this.utilizadores.put(l.getCodUtilizador(), l.clone());
    }

    public void addVoluntario(Voluntario l) {
        this.voluntarios.put(l.getCodVoluntario(),l.clone());
    }

    public void addEncomendas(Encomenda e) {
        this.encomendas.put(e.getCodEncomenda(), e.clone());
        this.loja.add(new Aceite(e));
        this.utilizadores.get(e.getCodUtilizador()).addEncomendasFeitas(e);
    }

    public void addSinalizadas(String e) {
        addSinalizadas(new Aceite(e));
    }

    public void addSinalizadas(Aceite e) {
        this.loja.remove(e);
        this.sinalizadas.add(e.clone());
    }

    public void addAceite(String transportadora, Aceite e) {
        this.sinalizadas.remove(e);
        this.encomendas.get(e.getCodEncomenda()).setCodTransportador(transportadora);
        this.aceite.add(e.clone());
    }

    public void removeAceite(Aceite e) {
        this.aceite.remove(e);
        this.encomendas.get(e.getCodEncomenda()).setCodTransportador("");
        this.sinalizadas.add(e);
    }

    public void addEntregues(Aceite e) {
        this.aceite.remove(e);
        this.entregues.add(e.clone());
        Encomenda x = this.encomendas.get(e.getCodEncomenda());
        Loja l = this.lojas.get(x.getCodLoja());
        Utilizador u = this.utilizadores.get(x.getCodUtilizador());
        Transportadora t = this.transportadoras.get(x.getCodTransportador());
        t.addKmFeitos(l, u);
        t.addEncomendaFeitas(e);
    }

    public void addAceiteLog(Aceite e) {
        this.loja.remove(e);
        this.sinalizadas.add(e.clone());
    }

    //ENCOMENDA

    // SO PARA ENCOMENDAS POR TRANSPORTADORA
    
    public boolean containsEncomenda(String encomenda) {
        return this.encomendas.containsKey(encomenda);
    }

    //UTILIZADOR

    public boolean contains(String email) {
        return this.utilizadores.containsKey(email);
    }

    public boolean password(String utilizador, String password) {
        return this.utilizadores.get(utilizador).getPassword().equals(password);
    }

    public Pair<List<String>,List<String>> getEncomendas(String codUtilizador) {
        Pair<List<String>,List<String>> ret = new Pair<>();
        List<String> first = new ArrayList<>();
        List<String> second = new ArrayList<>();
        for (Aceite aceite : this.sinalizadas) {
            Encomenda x = this.encomendas.get(aceite.getCodEncomenda());
            if(x.getCodUtilizador().equals(codUtilizador)) {
                first.add(x.getCodEncomenda());
                second.add(x.toString());
            }
        }
        ret.setFirst(first);
        ret.setSecond(second);
        return ret;
    }

    public Pair<List<String>,List<String>> getTransportadoras(String codEncomenda) {
        Pair<List<String>,List<String>> ret = new Pair<>();
        List<String> first = new ArrayList<>();
        List<String> second = new ArrayList<>();

        Encomenda e = this.encomendas.get(codEncomenda);
        Loja l = this.lojas.get(e.getCodLoja());
        Utilizador u = this.utilizadores.get(e.getCodUtilizador());
        for ( Transportadora t : this.transportadoras.values()) {
            if(t.isOn() && t.isNextTo(l,u)) {
                first.add(t.getCodEmpresa());
                second.add(String.format("%s - %s€", t.getNomeEmpresa(), t.custo(l, u, e) + l.tempo() * 0.5));
            }
        }
        ret.setFirst(first);
        ret.setSecond(second);
        return ret;
    }

    public Pair<List<String>,List<String>> getVoluntarios(String codEncomenda) {
        Pair<List<String>,List<String>> ret = new Pair<>();
        List<String> first = new ArrayList<>();
        List<String> second = new ArrayList<>();

        Encomenda e = this.encomendas.get(codEncomenda);
        Loja l = this.lojas.get(e.getCodLoja());
        Utilizador u = this.utilizadores.get(e.getCodUtilizador());
        for ( Voluntario t : this.voluntarios.values()) {
            if(t.isLivre() && t.isNextTo(l,u)) {
                first.add(t.getCodVoluntario());
                second.add(t.getNome());
            }
        }
        ret.setFirst(first);
        ret.setSecond(second);
        return ret;
    }

    public List<String> getPeriodo(String u, LocalDateTime a, LocalDateTime b) {
        List<String> ret = new ArrayList<>();
        Utilizador us = this.utilizadores.get(u);
        for (Aceite aceite : us.getEncomendasFeitas()) {
            Encomenda es = this.encomendas.get(aceite.getCodEncomenda());
            LocalDateTime ldt = es.getCriation();
            if(ldt.isAfter(a) && ldt.isBefore(b)) {
                ret.add(es.toString(this));
            }
        }
        return ret;
    }

    public List<String> getPeriodoVoluntario(String u, LocalDateTime a, LocalDateTime b) {
        List<String> ret = new ArrayList<>();
        Utilizador us = this.utilizadores.get(u);
        for (Aceite aceite : us.getEncomendasFeitas()) {
            Encomenda es = this.encomendas.get(aceite.getCodEncomenda());
            LocalDateTime ldt = es.getCriation();
            if(ldt.isAfter(a) && ldt.isBefore(b) && this.voluntarios.containsKey(es.getCodTransportador())) {
                ret.add(es.toString(this));
            }
        }
        return ret;
    }

    public List<String> getPeriodoTransportadora(String u, LocalDateTime a, LocalDateTime b) {
        List<String> ret = new ArrayList<>();
        Utilizador us = this.utilizadores.get(u);
        for (Aceite aceite : us.getEncomendasFeitas()) {
            Encomenda es = this.encomendas.get(aceite.getCodEncomenda());
            LocalDateTime ldt = es.getCriation();
            if(ldt.isAfter(a) && ldt.isBefore(b) && this.transportadoras.containsKey(es.getCodTransportador())) {
                ret.add(es.toString(this));
            }
        }
        return ret;
    }

    public Pair<List<String>,List<String>> getRatings(String u) {
        Pair<List<String>,List<String>> ret = new Pair<>();
        List<String> first = new ArrayList<>();
        List<String> second = new ArrayList<>();

        for (Aceite aceite : this.entregues) {
            Encomenda x = this.encomendas.get(aceite.getCodEncomenda());
            if(x.getCodUtilizador().equals(u) && x.getRating() == -1.0) {
                first.add(x.getCodEncomenda());
                second.add(x.toString(this));
            }
        }

        ret.setFirst(first);
        ret.setSecond(second);
        return ret;
    }

    public void SetRating(String e, Double rating) {
        this.encomendas.get(e).setRating(rating);
    }

    public Pair<List<String>,List<String>> getLojas() {
        Pair<List<String>,List<String>> ret = new Pair<>();
        List<String> first = new ArrayList<>();
        List<String> second = new ArrayList<>();

        for (Loja l : this.lojas.values()) {
            first.add(l.getCodLoja());
            second.add(l.getNomeLoja());
        }
        ret.setFirst(first);
        ret.setSecond(second);
        return ret;
    }


    // LOJA

    public List<String> getEncomendasLoja(String codLoja) {
        List<String> ret = new ArrayList<>();
        for (Aceite aceite : this.loja) {
            if(this.encomendas.get(aceite.getCodEncomenda()).getCodLoja().equals(codLoja)) {
                ret.add(aceite.getCodEncomenda());
            }
        }
        return ret;
    }

    public int getQueueLoja(String codLoja){
        return(this.lojas.get(codLoja).getQueue());
    }

	public boolean containsLoja(String loja) {
		return this.lojas.containsKey(loja);
    }
    
    public boolean passwordLoja(String loja, String pass) {
        return this.lojas.get(loja).getPassword().equals(pass);
    }
    
    //TRANSPORTADORA

    public boolean containsTransportadora(String codTransportadora) {
        return this.transportadoras.containsKey(codTransportadora);
    }

    public int transportadoraOn(String codTransportadora) {
        return (this.transportadoras.get(codTransportadora).getOn() ? 1 : 0);
    }

    public int toogleTransportadora(String codTransportadora) {
        return (this.transportadoras.get(codTransportadora).toogleOn() ? 1 : 0);
    }

    public boolean passwordTransportadora(String transportadora, String password) {
        return this.transportadoras.get(transportadora).getPassword().equals(password);
    }

    public double preco(String encomenda) {
        Encomenda e = this.encomendas.get(encomenda);
        Utilizador u = this.utilizadores.get(e.getCodUtilizador());
        Transportadora t = this.transportadoras.get(e.getCodTransportador());
        Loja l = this.lojas.get(e.getCodLoja());
        return t.custo(l, u, e) + l.tempo() * 0.5;
    }

    public double distancia(String encomenda) {
        Encomenda e = this.encomendas.get(encomenda);
        Utilizador u = this.utilizadores.get(e.getCodUtilizador());
        Transportadora t = this.transportadoras.get(e.getCodTransportador());
        Loja l = this.lojas.get(e.getCodLoja());
        return t.distancia(l, u);
    }

    public Pair<List<String>,List<String>> getEncomendasTransportadora(String codTransportadora) {
        Pair<List<String>,List<String>> ret = new Pair<>();
        List<String> first = new ArrayList<>();
        List<String> second = new ArrayList<>();
        for (Aceite aceite : this.aceite) {
            Encomenda x = this.encomendas.get(aceite.getCodEncomenda());
            if(x.getCodTransportador().equals(codTransportadora)) {
                first.add(x.getCodEncomenda());
                second.add(x.toString());
            }
        }
        ret.setFirst(first);
        ret.setSecond(second);
        return ret;
    }

    public void setEncomenda(String encomenda, int time) {
        Encomenda e = this.encomendas.get(encomenda);
        e.setTimePassed(time);
        e.setCusto(preco(encomenda));
        e.setReceived(LocalDateTime.now().plusMinutes(time));
    }



    //VOLUNTÁRIO

	public boolean containsVoluntario(String string) {
		return this.voluntarios.containsKey(string);
    }

    public Pair<List<String>,List<String>> getEncomendasVoluntario(String codVoluntario) {
        Pair<List<String>,List<String>> ret = new Pair<>();
        List<String> first = new ArrayList<>();
        List<String> second = new ArrayList<>();
        for (Aceite aceite : this.aceite) {
            Encomenda x = this.encomendas.get(aceite.getCodEncomenda());
            if(x.getCodTransportador().equals(codVoluntario)) {
                first.add(x.getCodEncomenda());
                second.add(x.toString());
            }
        }
        ret.setFirst(first);
        ret.setSecond(second);
        return ret;
    }
    
    public boolean passwordVoluntario(String vol, String pass) {
        return this.voluntarios.get(vol).getPassword().equals(pass);
    }

    public int toggleVoluntario(String codvoluntario) {
        return (this.voluntarios.get(codvoluntario).toogleOn() ? 1 : 0);
    }

    public double totalFaturado(String transportadora, LocalDateTime antes, LocalDateTime depois) {
        double ret = 0.0;
        double buf;
        for (Aceite x : this.transportadoras.get(transportadora).getEncomendasFeitas()) {
            Encomenda e = this.encomendas.get(x.getCodEncomenda());
            LocalDateTime receive = e.getReceived();
            if(receive.isAfter(antes) && receive.isBefore(depois)) {
                buf = e.getCusto();
                ret += (buf >= 0 ? buf : 0);
            }
        }
        return ret;
    }

    //QUERIES
    public List<String> top10Empresas() {
        TreeSet<Transportadora> ret = new TreeSet<>(new Comparator<Transportadora>() {

			@Override
			public int compare(Transportadora o1, Transportadora o2) {
                int ret = Double.compare(Double.valueOf(o2.getKmFeitos()), Double.valueOf(o1.getKmFeitos()));
                if(ret == 0) ret = 1;
                return ret;
			}
        });
        for (Transportadora transportadora : this.transportadoras.values()) {
            ret.add(transportadora);
        }
        List<String> rets = new ArrayList<>();
        int i = 0;
        for (Transportadora transportadora : ret) {
            if(i == 10) break;
            i++;
            rets.add(transportadora.getNomeEmpresa() + " - " + transportadora.getKmFeitos());
        }
        return rets;
    }

    public List<String> top10Utilizadores() {
        TreeSet<Utilizador> ret = new TreeSet<>(new Comparator<Utilizador>() {

			@Override
			public int compare(Utilizador o1, Utilizador o2) {
                int ret = Integer.compare(o2.getEncomendasFeitas().size(), o1.getEncomendasFeitas().size());
                if(ret == 0) ret = 1;
                return ret;
			}
        });
        for (Utilizador utilizador : this.utilizadores.values()) {
            ret.add(utilizador);
        }
        List<String> rets = new ArrayList<>();
        int i = 0;
        for (Utilizador utilizador : ret) {
            if(i == 10) break;
            i++;
            rets.add(utilizador.getNome() + " - " + utilizador.getEncomendasFeitas().size() + " encomendas");
        }
        return rets;
    }

}