package Perfis;

import Auxiliares.GPS;
import Auxiliares.Viagem;
import Encomendas.Encomenda;
import Registos.Registos;

import java.io.Serializable;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.chrono.ChronoLocalDateTime;
import java.util.ArrayList;
import java.util.Iterator;


public abstract class Transportadores extends Perfil implements Serializable {

    /*
    Variaveis
     */

    private Double raio;
    private Integer classificacoes;
    private Double classificacaoFinal;
    private Boolean prontoARecolher;
    private Boolean certificado;
    private ArrayList<Viagem> trips;
    private Double kmsPercorridos;
    private Integer capacidade; //1 ou + encomendas que faz de uma só vez
    private ArrayList<Encomenda> porEntregar; // tem 1 o mais encomendas, dependendo da capacidade
    private ArrayList<Encomenda> encomendasAceites; // tem todas as encomendas que foram aceites para entrega



    /*
    Construtores
     */

    public Transportadores(String codigoPerfil, GPS coordenadas, String nome, String email, String pass, Double raio, Integer classificacoes, Double classificacaoFinal, Boolean prontoARecolher, Boolean certificado, ArrayList<Viagem> trips, Double kmsPercorridos,Integer capacidade,ArrayList<Encomenda> porEntregar, ArrayList<Encomenda> encomendasAceites) {
        super(codigoPerfil, coordenadas, nome, email, pass);
        this.raio = raio;
        this.classificacoes = classificacoes;
        this.classificacaoFinal = classificacaoFinal;
        this.prontoARecolher = prontoARecolher;
        this.certificado = certificado;
        this.trips = trips;
        this.kmsPercorridos = kmsPercorridos;
        this.capacidade = capacidade;
        this.porEntregar = porEntregar;
        this.encomendasAceites = encomendasAceites;
    }

    public Transportadores() {
        super();
        this.raio = 0.0;
        this.classificacoes = 0;
        this.classificacaoFinal = 0.0;
        this.prontoARecolher = true;
        this.certificado = false;
        this.trips = new ArrayList<>();
        this.kmsPercorridos = 0.0;
        this.capacidade = 1;
        this.porEntregar = new ArrayList<>();
        this.encomendasAceites = new ArrayList<>();
    }

    public Transportadores(Transportadores other) {
        super(other);
        this.raio = other.getRaio();
        this.classificacoes = other.getClassificacoes();
        this.classificacaoFinal = other.getClassificacaoFinal();
        this.prontoARecolher = other.getProntoARecolher();
        this.certificado = other.getCertificado();
        this.trips = other.getTrips();
        this.kmsPercorridos = other.getKmsPercorridos();
        this.capacidade = other.getCapacidade();
        this.encomendasAceites = other.getEncomendasAceites();
    }


    /*
    Gets e Sets
     */

    public Double getRaio() {
        return raio;
    }

    public void setRaio(Double raio) {
        this.raio = raio;
    }

    public Integer getClassificacoes() {
        return classificacoes;
    }

    public void setClassificacoes(Integer classificacoes) {
        this.classificacoes = classificacoes;
    }

    public Double getClassificacaoFinal() {
        return classificacaoFinal;
    }

    public void setClassificacaoFinal(Double classificacaoFinal) {
        this.classificacaoFinal = classificacaoFinal;
    }

    public Boolean getProntoARecolher() {
        return prontoARecolher;
    }

    public void setProntoARecolher(Boolean prontoARecolher) {
        this.prontoARecolher = prontoARecolher;
    }

    public Boolean getCertificado() {
        return certificado;
    }

    public void setCertificado(Boolean certificado) {
        this.certificado = certificado;
    }

    public Double getKmsPercorridos() {
        return kmsPercorridos;
    }

    public void setKmsPercorridos(Double kmsPercorridos) {
        this.kmsPercorridos = kmsPercorridos;
    }

    public ArrayList<Viagem> getTrips() {
        ArrayList<Viagem> temp = new ArrayList<>();
        for (Viagem v : this.trips) temp.add(v.clone());
        return temp;
    }

    public void setTrips(ArrayList<Viagem> v) {
        this.trips = new ArrayList<>();
        for (Viagem i : v) trips.add(i.clone());
    }

    public Integer getCapacidade() {
        return capacidade;
    }

    public void setCapacidade(Integer capacidade) {
        this.capacidade = capacidade;
    }

    public ArrayList<Encomenda> getPorEntregar() {
        ArrayList<Encomenda> temp = new ArrayList<>();
        if (this.porEntregar != null) {
            for (Encomenda e : this.porEntregar) temp.add(e.clone());
        }
        return temp;
    }

    public void setPorEntregar(ArrayList<Encomenda> pe) {
        this.porEntregar = new ArrayList<>();
        for (Encomenda e : pe) porEntregar.add(e.clone());
    }

    public ArrayList<Encomenda> getEncomendasAceites() {
        ArrayList<Encomenda> temp = new ArrayList<>();
        for (Encomenda e : this.encomendasAceites) temp.add(e.clone());
        return temp;
    }

    public void setEncomendasAceites(ArrayList<Encomenda> pe) {
        this.encomendasAceites = new ArrayList<>();
        for (Encomenda e : pe) encomendasAceites.add(e.clone());
    }





    /*
    Métodos
     */

    public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        if (!super.equals(object)) return false;
        Transportadores that = (Transportadores) object;
        return super.equals(that) &&
                this.raio.equals(that.raio) &&
                this.classificacoes.equals(that.classificacoes) &&
                this.classificacaoFinal.equals(that.classificacaoFinal) &&
                this.prontoARecolher.equals(that.prontoARecolher) &&
                this.certificado.equals(that.certificado) &&
                this.trips.equals(that.trips) &&
                this.kmsPercorridos.equals(that.kmsPercorridos) &&
                this.porEntregar.equals(that.porEntregar) &&
                this.encomendasAceites.equals(that.encomendasAceites);
    }

    public abstract String toString();

    public abstract Transportadores clone();

    public void addViagem(Viagem v) {
        this.trips.add(v);
    }

    public abstract Duration tempoEntrega(Registos r); // definida de forma diferente nas empresas e nos voluntarios por causa da sua velocidade (os voluntarios neste caso tem a hipotese de entregar a pe)

    // Faz print de todas as Viagens
    public void viagensTotal() { // todo , passar tudo que faz print para devolver string, ao criar um transportador, este nao tera viagens (??)

        ArrayList<Viagem> x = new ArrayList<Viagem>(this.getTrips());
        StringBuilder sb = new StringBuilder();

        if (x.isEmpty()) { System.out.println("Ainda não realizaste nenhuma entrega de encomendas.\n"); }
        else {
            for (Viagem viagem : x) {
                sb.append(viagem.toString()).append("\n");
            }
            System.out.println(sb);
        }
    }

    // Faz print de todas as viagens realizadas num espaco de tempo
    public void viagensPeriodo(LocalDate inicio, LocalDate fim) {
        ArrayList<Viagem> x = new ArrayList<>(this.getTrips());
        StringBuilder sb = new StringBuilder();

        if (x.isEmpty()) { System.out.println("Não realizaste nenhuma entrega neste periodo de tempo.\n"); }
        else {
            // Se for para o mesmo dia
            if (inicio.isEqual(fim)) {
                for (Viagem v : this.trips) {
                    if ((LocalDate.from(v.getInicio()).atStartOfDay()).isEqual(LocalDate.from(inicio).atStartOfDay())) {
                        x.add(v);
                    }
                }
                for (Viagem viagem : x) {
                    sb.append(viagem.toString()).append("\n");
                }
                System.out.println(sb);
            }

            // Para dias diferentes
            else {
                if (inicio.isAfter(fim)) { System.out.println("As datas estão erradas, por favor tente novamente.\n"); }
                else {
                    for (Viagem v : this.trips) {
                        if (v.getInicio().toLocalDate().isAfter(inicio) && v.getInicio().toLocalDate().isBefore(fim)) {
                            x.add(v);
                        }
                    }
                    for (Viagem viagem : x) {
                        sb.append(viagem.toString()).append("\n");
                    }
                    System.out.println(sb);
                }
            }
        }
    }

    public void utilizadoresServidos(Registos r){

        StringBuilder sb = new StringBuilder();
        for(Viagem v : this.trips){

            sb.append(" - ").append(v.buscarUtilizador(r)).append("\n");
        }

        System.out.println("Aqui está a lista de todos os utilizadores que já serviu! : \n");
        System.out.println(sb.toString());

    }


    // Viagens realizadas para um utilizador especifico //todo
    public void viagensUtilizador(String codUtilizador, Registos r) {
        ArrayList<Viagem> x = new ArrayList<Viagem>();

        for (Viagem v : this.trips) {
            String utilizador = v.buscarUtilizador(r);
            if (utilizador.equals(codUtilizador)) {
                x.add(v);
            }
        }
        StringBuilder sb = new StringBuilder();

        for (Viagem viagem : x) {
            sb.append(viagem.buscarEncomenda(r));
        }
        System.out.println(sb);
    }

    // Display de todas as encomendas que foram aceites mas ainda nao foram entregues
    public void toStringEncomendasAceites(){
        StringBuilder sb = new StringBuilder();
        for(Encomenda e: this.encomendasAceites){
            if(e.getEntregue().equals(false)) {
                sb.append(" - ").append(e.getCodEncomenda()).append("\n");
            }
        }
        System.out.println(sb.toString());
    }

    public Double distTotal(Registos r){
        Encomenda e =this.encomendasAceites.get(this.encomendasAceites.size()-1); // ultima encomenda a ser aceite

        double dist2loja = r.getPerfil(this.getCodigoPerfil()).getCoordenadas().distanciaEntre(r.getPerfil(e.getCodLoja()).getCoordenadas());
        double dist2user = r.getPerfil(e.getCodLoja()).getCoordenadas().distanciaEntre(r.getPerfil(e.getCodUtilizador()).getCoordenadas());

        return dist2loja+dist2user;
    }

    // E um metodo abstrato, sera tratado de forma diferente nas empresas e nos voluntarios
    public abstract void entregaRealizada(Registos r);

    // Verifica se alguma das encomendas aceites ainda nao foi entregue
    public Boolean tudoEntregue(){
        for(Encomenda e: this.encomendasAceites){
            if(e.getEntregue().equals(false)){ return false;}

        }
        return true;
    }

    public void addAceite(Encomenda e) {
        this.encomendasAceites.add(e);
    }

    // compara os perfis pelo numero de km percorridos (só para transportadoras)
    public static int compareKmsPercorridos(Perfil y, Perfil x) {
        if ((x instanceof Transportadores) & (y instanceof Transportadores)) {
            return Double.compare(((Transportadores) x).getKmsPercorridos(), ((Transportadores) y).getKmsPercorridos());
        }
        else return 0;
    }

    // poe todas as encomendas que nao estavam entregues a estarem entregues
    public void atualizaAceites(Registos r){
        ArrayList<Encomenda> aceites = this.getEncomendasAceites();
        for(Encomenda e : aceites){
            if(e.getEntregue() == false){
                this.atualizaHistorico(e,this.getCodigoPerfil(),true);
                String codUtil = e.getCodUtilizador();
                r.getPerfil(codUtil).atualizaHistorico(e,this.getCodigoPerfil(),true);
            }
        }
        this.setEncomendasAceites(aceites);
    }


    // atualiza a ultima viagem para esta ja estar classificada
    public void atualizaViagens(String codEnc){
        ArrayList<Viagem> novo = new ArrayList<>(this.getTrips());
        for (Viagem x : novo) {
            {
                if(x.getCodEncomenda().equals(codEnc)) {
                    x.estaClassificado(true);
                }
            }
        }
        this.setTrips(novo);
    }

    // Verifica se, ao adicionar uma nova encomenda, se esta ja existe no array de encomendas que quer entregar
    public boolean jaTemEncomenda(String codEnc){
        for(Encomenda e: this.getPorEntregar()){
            if(e.getCodEncomenda().equals(codEnc)){
                return true;
            }
        }
        return false;
    }

    public int compareToTransportadores(Perfil o) {
        if (o instanceof Transportadores){
            return Double.compare(this.getKmsPercorridos(),(((Transportadores) o).getKmsPercorridos()));
        }
        return 0;
    }

}
