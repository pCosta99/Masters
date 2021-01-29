package Perfis;

import Auxiliares.GPS;
import Auxiliares.Viagem;
import Encomendas.Encomenda;
import Registos.Registos;

import java.time.Duration;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Random;

public class Empresa extends Transportadores{

    /*
    Variáveis de instância
     */

    private String nif;
    private Double taxaKm;


    
    /*
    Construtores
     */

    public Empresa(String codigoPerfil,GPS coordenadas, String nome, String email, String pass,Boolean prontoARecolher,Boolean certificado,ArrayList<Viagem> trips,Double kmsPercorridos, String nif, double raio, double taxaKm, int capacidade, int classificacoes, double classificacaoFinal,ArrayList<Encomenda> porEntregar, ArrayList<Encomenda> encomendasAceites) {
        super(codigoPerfil,coordenadas,nome,email,pass,raio,classificacoes,classificacaoFinal,prontoARecolher,certificado,trips,kmsPercorridos,capacidade,porEntregar,encomendasAceites);
        this.nif = nif;
        this.taxaKm = taxaKm;
    }

    public Empresa(){
        super();
        this.nif = "";
        this.taxaKm = 0.0;
    }

    public Empresa(Empresa other) {
        super(other);
        this.nif = other.getNif();
        this.taxaKm = other.getTaxaKm();
    }


    /*
    Getters e Setters
     */

    public String getNif() {
        return nif;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public Double getTaxaKm() {
        return taxaKm;
    }

    public void setTaxaKm(Double taxaKm) {
        this.taxaKm = taxaKm;
    }


    /*
    Metodos
     */

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Tansportadora: ").append(this.getCodigoPerfil()).append("\n")
                .append(" - Nome: ").append(this.getNome()).append("\n")
                .append(" - NIF: ").append(this.nif).append("\n")
                .append(" - Localização: ").append(this.getCoordenadas()).append("\n")
                .append(" - Classificação: ").append(this.getClassificacaoFinal()).append("\n")
                .append(" - Número de encomendas feitas: ") .append(this.getHistorico().size()).append("\n")
                .append(" - Número de KMs percorridos: ") .append(this.getKmsPercorridos()).append("\n");

        return sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Empresa that = (Empresa) o;
        return super.equals(that) &&
                this.nif.equals(that.getNif()) &&
                this.taxaKm.equals(that.getTaxaKm());
    }

    public Empresa clone(){
        return new Empresa(this);
    }



    public Double calculaPrecoTransporte(Encomenda enc, Registos r){
        double distLoja = this.getCoordenadas().distanciaEntre(enc.coordenadasLoja(r));
        double distUser = enc.coordenadasLoja(r).distanciaEntre(enc.coordenadasUser(r));
        return (distLoja+distUser)*this.getTaxaKm();

    }

    // Faz print do valor faturizado num determinda tempo
    public void faturacaoPeriodo(LocalDate inicio, LocalDate fim){
        double faturado = 0.0;


        if(this.getTrips().isEmpty()){ System.out.println("Ainda não completaste nenhum serviço para faturares.\n"); }

        else {

            // Se for para o mesmo dia
            if (inicio.isEqual(fim)) {
                for (Viagem v : this.getTrips()) {
                    if ((LocalDate.from(v.getInicio()).atStartOfDay()).isEqual(LocalDate.from(inicio).atStartOfDay())) {
                        faturado += v.getPreco();
                    }
                }
                System.out.println(faturado);
            }

            // Para dias diferentes
            else {
                if (inicio.isAfter(fim)) { System.out.println("As datas estão erradas, por favor tente novamente.\n"); }
                else {
                    for (Viagem v : this.getTrips()) {
                        if (v.getInicio().toLocalDate().isAfter(inicio) && v.getInicio().toLocalDate().isBefore(fim)) {
                            faturado += v.getPreco();
                        }
                    }
                    System.out.println(faturado);
                }
            }
        }
    }

    // Faz print do valor total faturizado
    public void faturacaoTotal() {
        double faturado = 0.0;
        if (this.getTrips().isEmpty()) { System.out.println("Ainda não completaste nenhum serviço para faturares.\n"); }
        else {
            for (Viagem viagem : this.getTrips()) {
                faturado += viagem.getPreco();
            }
            System.out.println(faturado);
        }
    }


    // tempo total que uma empresa demora a entregar a encomenda
    public Duration tempoEntrega(Registos r ){
        int max = 60;
        int min = 20; // a empresa nao tem a possibilidade de ir a pe (normalmente andam de carrinha)

        double distToloja = this.getCoordenadas().distanciaEntre(this.getHistorico().get(getHistorico().size() - 1).coordenadasLoja(r));
        double distTouser = this.getHistorico().get(getHistorico().size() - 1).coordenadasLoja(r).distanciaEntre(this.getHistorico().get(getHistorico().size() - 1).coordenadasUser(r));

        Random rng = new Random();

        int x = rng.nextInt((max - min) + 1) + min;
        int y = rng.nextInt((max - min) + 1) + min;

        double tempo = (distToloja / x) + (distTouser / y);

        return Duration.ofMinutes((long) (tempo * 60));
    }

    public void entregaRealizada(Registos r){
        for(Encomenda e: this.getEncomendasAceites()){
            if(e.getEntregue().equals(false)){
                Viagem viagem = new Viagem();
                viagem.setCodEncomenda((e.getCodEncomenda())); // atualiza o codigo de encomenda na viagem
                viagem.setDistanciaPercorrida(this.distTotal(r)); // calcula a distancia total
                viagem.setDuracao(this.tempoEntrega(r)); // atualiza o tempo de que demora a entregar
                viagem.setPreco(this.calculaPrecoTransporte(e,r)); // calcula o preco da viagem
                this.addViagem(viagem);
                this.setKmsPercorridos(this.getKmsPercorridos()+this.distTotal(r)); // atualiza os kms percorridos
                // viagem criada, logo esta entregue
                // a hora do comeco da viagem e por omissao (LocalDateTime.now)
            }
        }
    }

    public String toStringGrava(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getCodigoPerfil()).append(",")
                .append(this.getNome()).append(",")
                .append(this.getCoordenadas().toStringGrava()).append(",")
                .append(this.getNif()).append(",")
                .append(this.getRaio()).append(",")
                .append(this.getTaxaKm());

        return sb.toString();
    }

}

