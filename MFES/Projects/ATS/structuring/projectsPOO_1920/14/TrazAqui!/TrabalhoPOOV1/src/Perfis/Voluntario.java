package Perfis;

import Auxiliares.GPS;
import Auxiliares.Viagem;
import Encomendas.Encomenda;
import Registos.Registos;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Random;

public class Voluntario extends Transportadores {
    /*
    Variáveis de instância
     */


    /*
    Construtores
     */

    public Voluntario(String codigoPerfil,GPS coordenadas, String nome, String email, String pass, Double raio, Integer classificacoes, Double classificacaoFinal, Boolean prontoARecolher, Boolean certificado, ArrayList<Viagem> trips,Double kmsPercorridos,Integer capacidade,ArrayList<Encomenda> porEntregar,ArrayList<Encomenda> encomendasAceites) {

        super(codigoPerfil,coordenadas,nome,email,pass,raio,classificacoes,classificacaoFinal,prontoARecolher,certificado,trips,kmsPercorridos,1,porEntregar,encomendasAceites);

    }

    public Voluntario(){
        super();
    }

    public Voluntario(Voluntario other){
        super(other);
    }

    /*
    Métodos
     */

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Voluntário: ").append(this.getCodigoPerfil()).append("\n")
                .append(" - Nome: ").append(this.getNome()).append("\n")
                .append(" - Localização: ").append(this.getCoordenadas()).append("\n")
                .append(" - Classificação: ").append(this.getClassificacaoFinal()).append("\n")
                .append(" - Número de encomendas feitas: ") .append(this.getHistorico().size()).append("\n");


        return sb.toString();
    }


    public boolean equals(Object object) {
        return super.equals(object);
    }

    public Voluntario clone(){
        return new Voluntario(this);
    }



    // tempo total que uma empresa demora a entregar a encomenda
    public Duration tempoEntrega(Registos r ){
        int max = 60;
        int min = 5; // os voluntarios tambem andam a pe


        double distToloja = this.getCoordenadas().distanciaEntre(this.getHistorico().get(this.getHistorico().size() - 1).coordenadasLoja(r));
        double distTouser = this.getHistorico().get(this.getHistorico().size() - 1).coordenadasLoja(r).distanciaEntre(this.getHistorico().get(getHistorico().size() - 1).coordenadasUser(r));

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
                this.addViagem(viagem);
                int i = r.getPerfil(e.getCodUtilizador()).getHistorico().size();
                r.getPerfil(e.getCodUtilizador()).getHistorico().get(i-1).setEntregue(true);
                // a hora do comeco da viagem e por omissao (LocalDateTime.now)
            }
        }
    }

    public String toStringGrava(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getCodigoPerfil()).append(",")
                .append(this.getNome()).append(",")
                .append(this.getCoordenadas().toStringGrava()).append(",")
                .append(this.getRaio());

        return sb.toString();
    }

}